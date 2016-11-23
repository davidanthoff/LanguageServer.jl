using JuliaParser

#=
type Position
    line::Int
    column::Int
end

type Diagnostic
end

type Range
    start::Position
    stop::Position
end
=#

type VarInfo
    t::String # type of variable
    doc::String # location of this variable
end

# signature infor of a function or a block
type BlockInfo
    name::String # name of function
    fullname::String # name of method (function + BlockInfo)
    rtype::String # name of return type, default would be ANY
    localvars::Dict{String, VarInfo} # list of input arguments
    doc::String 
end
BlockInfo() = BlockInfo("", "", "", Dict(), "")

type Block
    uptodate::Bool
    kind::String # block kind: module, functioon, ...etc, expression block
    range::Range # range of this block
    ex::Any # the expression (for expression block only)
    subblocks::Vector{Block} # subblocks of this block
    info::BlockInfo
    diags::Vector{Diagnostic}
end

Block(kind::String, range::Range) = Block(false, kind, range, nothing, Block[], BlockInfo(), Diagnostic[])
Block(kind::String) = Block(kind, Range(Position(-1, -1), Position(-1, -1)))

const EMPTYBLOCK = Block(false, "", Range(Position(-1, -1), Position(-1, -1)), nothing, Block[], BlockInfo(), Diagnostic[])

import Base: string

string(tk::JuliaParser.Tokens.Token) = string(tk.val)

function process_function_name_expression(ex)
    if isa(ex, Symbol)
        return string(ex)
    elseif isa(ex, Expr) && ex.head == :curly
        return string(ex.args[1])
    end
    return string(ex)
end

function process_parameter_expression(ex)
    if isa(ex, Symbol)
        return string(ex), "Any", "$(string(ex))::Any"
    elseif isa(ex, Expr) && ex.head == :(::)
        if length(ex.args) >= 2
            name = string(ex.args[1])
            rtype = string(ex.args[2])
            doc = "$(name)::$(rtype)"
            return name, rtype, doc
        end
    elseif isa(ex, Expr) && ex.head == :kw
        dval = ex.args[2]
        name, rtype, _ = process_parameter_expression(ex.args[1])
        rtype == "Any" ? get_type(dval) : "Any"
        doc = "$(name)::$(rtype)=$(dval)"
        return name, rtype, doc
    elseif isa(ex, Expr) && ex.head == :...
        return process_parameter_expression(ex.args[1])
    end
    return "", "", ""
end

function process_signature_expression(ex)
    if !isa(ex, Expr)
        if isa(ex, Symbol)
            name = string(ex)
            return BlockInfo(name, name, "Any", Dict(), "")
        end 
        return BlockInfo()
    end
    if ex.head == :call
        name = process_function_name_expression(ex.args[1])
        localvars = Dict()
        for arg in ex.args[2:end]
            if isa(arg, Expr) && arg.head == :parameters
                for sub_arg in arg.args
                    vname, vrtype, vdoc = process_parameter_expression(sub_arg)
                    localvars[vname] = VarInfo(vrtype, vdoc)
                end
            else
                vname, vrtype, vdoc = process_parameter_expression(arg)
                localvars[vname] = VarInfo(vrtype, vdoc)
            end
        end
        fullname = string(ex)
        doc = "function $(fullname)::Any"
        return BlockInfo(name, fullname, "Any", localvars, doc)
    elseif ex.head == :(::)
        info = process_signature_expression(ex.args[1])
        info.rtype = string(ex.args[2])
        info.fullname = string(ex)
        info.doc = "function $(info.fullname)"        
        return info
    end
    return BlockInfo()
end

function is_short_form_function_expression(ex)
    if !isa(ex, Expr)
        return false
    elseif ex.head != :(=)
        return false
    else
        return is_function_signature_expression(ex.args[1])
    end
    return false
end

function is_function_signature_expression(ex)
    if !isa(ex, Expr)
        return false
    end
    if ex.head == :call
        return true
    elseif ex.head == :(::)
        return true
    end
    return false
end

function parse_expression(ts::Lexer.TokenStream, context="default")
    start = Position(ts.lineno - 1, 0)
    ps = Parser.ParseState()
    firsttoken = Lexer.peek_token(ts)
    ok = true
    ex = try
            if context == "let"
                Parser.parse_comma_sep_assigns(ps, ts)
            elseif context == "for"
                Parser.parse_comma_sep_iters(ps, ts, :for)
            else
                Parser.parse_eq(ps, ts)
            end
          catch diag
              ok = false
              Lexer.take_token(ts)
              Lexer.skip_to_eol(ts)
              Expr(:error, diag)
          end
    isa(ex, JuliaParser.Tokens.Token) &&  (ex = ex.val)
    stop = Position(max(ts.lineno - 2, start.line), 99)
    Lexer.take_token(ts)
    name = "expression"
    if firsttoken.val in Set([Symbol("using"), Symbol("import"), Symbol("const"), Symbol("abstract"), Symbol("local"), Symbol("global")])
        name = string(firsttoken.val)
    end
    return Block(ok, name, Range(start, stop), ex, Block[], BlockInfo(), Diagnostic[])
end

function parseblocks(uri::String, server::LanguageServerInstance, updateall=false)
    doc = String(server.documents[uri].data)
    blocks = parse_all(doc)
    server.documents[uri].blocks = blocks
    return
end

function parse_all(doc::String)
    blocks = Block[]
    push!(blocks, Block("toplevel"))
    context = "default"
    block_stack = Block[]
    push!(block_stack, blocks[1])
    ts = Lexer.TokenStream(doc)
    while !Lexer.eof(ts)
        tk = Lexer.next_token(ts, true)
        if Lexer.eof(tk)
            break
        end
        if tk.val in Set([Symbol("module"), Symbol("baremodule"), Symbol("type"), Symbol("immutable"), Symbol("bitstype"), Symbol("function"), Symbol("staggedfunction"), Symbol("macro"), Symbol("if"), Symbol("while"), Symbol("begin"), Symbol("for")])
            b = Block(string(tk.val), Range(Position(ts.lineno - 1, 0), Position(ts.lineno - 1, 0)))
            push!(blocks, b)
            push!(block_stack[end].subblocks, b)
            push!(block_stack, b)
            context = string(tk.val)
        elseif tk.val == Symbol("let")
            b = Block(string(tk.val), Range(Position(ts.lineno - 1, 0), Position(ts.lineno - 1, 0)))
            push!(blocks, b)
            push!(block_stack[end].subblocks, b)
            push!(block_stack, b)
            if Lexer.peek_token(ts).val !== '\n'
                sb = parse_expression(ts, "let")
                push!(blocks, sb)
                push!(block_stack[end].subblocks, sb)
            else
                push!(block_stack[end].subblocks, EMPTYBLOCK)
            end
        elseif tk.val in Set([:elseif, :else])
            b = Block(string(tk.val), Range(Position(ts.lineno - 1, 0), Position(ts.lineno - 1, 0)))
            push!(blocks, b)
            if block_stack[end].kind in Set(["if", "elseif"])
                block_stack[end].range.stop = Position(max(0, ts.lineno - 2), 0)
                pop!(block_stack)
                push!(block_stack[end].subblocks, b)
            end
            push!(block_stack, b)
        elseif tk.val == :end
            stop = Position(ts.lineno - 1, 79)
            block_stack[end].range.stop = stop
            pop!(block_stack)
        elseif tk.val === '\n'
            continue
        else
            ts.putback = tk
            b = parse_expression(ts, context)
            push!(blocks, b)
            push!(block_stack[end].subblocks, b)
            context = :default
        end
    end
    for b in block_stack
        b.range.stop.line = ts.lineno - 1
    end
    # update every blocks recursively from the root block
    update_block!(blocks[1])
    return blocks
end

function update_block!(b::Block)
    if isa(b.ex, Expr) && b.ex.head in Set([Symbol("incomplete"), Symbol("error")])
        return
    end
    if b.kind in ["function", "macro", "staggedfunction"]
        update_function_block!(b)
    elseif b.kind in ["type", "bitstype", "immutable"]
        update_type_block!(b)
    elseif b.kind in ["module", "baremodule"]
        update_module_block!(b)
    elseif b.kind in ["expression"]
        update_expression_block!(b)
    elseif b.kind in ["using", "import"]
        update_using_block!(b)
    elseif b.kind in ["const", "global"]
        update_const_block!(b)
    elseif b.kind in ["abstract"]
        update_abstract_block!(b)
    end
    for sb in b.subblocks
        # update its children
        update_block!(sb)
        if !(sb.kind in Set(["function", "staggedfunction", "macro", "type", "immutable", "bitstype", "let"]))
            # if a variable not in this block then added it
            for (vname, vinfo) in sb.info.localvars
                if !haskey(b.info.localvars, vname)
                    b.info.localvars[vname] = vinfo
                end
            end
        end
        if b.kind in Set(["module", "toplevel"]) 
            if sb.kind in Set(["function", "staggedfunction", "macro", "type", "bitstype", "module"])
                b.info.localvars[sb.info.name] = VarInfo(sb.kind, sb.info.doc)
            elseif sb.kind in Set(["using", "import", "assignment", "module"])
                for (vname, vinfo) in sb.info.localvars
                    if !haskey(b.info.localvars, vname)
                        b.info.localvars[vname] = vinfo
                    end
                end
            end
        end
    end
end

function update_function_block!(b::Block)
    isempty(b.subblocks) && return
    sig_block = b.subblocks[1]
    b.info = process_signature_expression(sig_block.ex)
    b.info.doc = "Defined at line $(b.range.start.line + 1)\n" * b.info.doc
end

function update_module_block!(b::Block)
    isempty(b.subblocks) && return
    sb = b.subblocks[1]
    if isa(sb.ex, Symbol)
        b.info.name = string(sb.ex)
        b.info.fullname = b.info.name
    end
end

function process_type_name_expression(ex)
    if isa(ex, Expr) && ex.head == :(<:)
        return process_type_name_expression(ex.args[1])
    else
        return process_function_name_expression(ex)
    end
    return ""
end

function update_type_block!(b::Block)
    # get name of the type block
    name_ex = !isempty(b.subblocks) ? b.subblocks[1].ex : nothing
    b.info.fullname = string(name_ex)
    b.info.name = process_type_name_expression(name_ex)
    b.info.doc = "$(b.kind) $(b.info.fullname)\n"
    # get field names
    for sb in b.subblocks[2:end]
        vname, vtype, vdoc = process_parameter_expression(sb.ex)
        b.info.localvars[vname] = VarInfo(vtype, vdoc)
        b.info.doc *= "\t$(vdoc)\n"
    end
end

function update_expression_block!(b::Block)
    isa(b.ex, Expr) && b.ex.head == :error && return
    if is_short_form_function_expression(b.ex)
        b.kind = "function"
        b.info = process_signature_expression(b.ex.args[1])
    elseif isa(b.ex, Expr) && b.ex.head == Symbol("=") && isa(b.ex.args[1], Symbol)
        b.kind = "assignment"
        vname = string(b.ex.args[1])
        # check if the function call is a constructor
        vtype = get_type(b.ex.args[2])
        if isa(b.ex.args[2], Expr) && b.ex.args[2].head == :call
            fname = string(b.ex.args[2].args[1])
            if isupper(fname[1])
                vtype = fname
            end
        end
        vdoc = string(b.ex)*"::$(vtype)"
        b.info.name = vname
        b.info.fullname = vname
        b.info.rtype = vtype
        b.info.localvars[vname] = VarInfo(vtype, vdoc)
    end
end

function update_using_block!(b::Block)
    ex = b.ex
    if isa(ex, Expr) && ex.head == Symbol(b.kind)
        b.info.localvars[string(b.ex.args[1])] = VarInfo("Module", "")
    else isa(ex, Expr) && ex.head == Symbol("toplevel")
        for subex in ex.args
            if isa(subex, Expr) && subex.head == Symbol(b.kind)
                mod = string(subex.args[1])
                b.info.localvars[mod] = VarInfo("Module", "")
                if length(subex.args) >= 2
                    name = string(subex.args[2])
                    # TODO, how to determine kind of imported symbol?
                    b.info.localvars[name] = VarInfo("Imported symbol", "$(mod).$(name)")
                end
            end
        end
    end
end

function update_const_block!(b::Block)
    if isa(b.ex, Expr) && b.ex.head == Symbol(b.kind)
        vname, vtype, vdoc = process_const_expression(b.ex)
        b.info.localvars[vname] = VarInfo(vtype, vdoc)
    end
end

function process_const_expression(ex)
    if isa(ex.args[1], Expr) && ex.args[1].head == Symbol("=")
        vname = string(ex.args[1].args[1])
        vtype = get_type(ex.args[1].args[2])
        vdoc = string(ex)
        return vname, vtype, vdoc
    elseif isa(ex.args[1], Expr) && ex.args[1].head == Symbol("const")
        return process_const_expression(ex.args[2])
    end
    return "", "", ""
end

function update_abstract_block!(b::Block)
    ex = b.ex
    if isa(ex, Expr) && ex.head == Symbol("abstract")
        if isa(ex.args[1], Symbol)
            vname = string(ex.args[1])
            vtype = "DataType"
            vdoc = string(ex)
            b.info.name = b.info.fullname = vname
            b.info.localvars[vname] = VarInfo(vtype, vdoc)
        elseif isa(ex.args[1], Expr) && ex.args[1].head == Symbol("<:")
            vname = string(ex.args[1].args[1])
            vtype = "DataType"
            vdoc = string(ex)
            b.info.name = b.info.fullname = vname
            b.info.localvars[vname] = VarInfo(vtype, vdoc)
        end
    end
end

import Base:<, in, intersect
<(a::Position, b::Position) =  a.line<b.line || (a.line≤b.line && a.character<b.character)
function in(p::Position, r::Range)
    (r.start.line < p.line < r.stop.line) ||
    (r.start.line == p.line && r.start.character ≤ p.character) ||
    (r.stop.line == p.line && p.character ≤ r.stop.character)  
end

intersect(a::Range, b::Range) = a.start in b || b.start in a

function get_block_stack(tdpp::TextDocumentPositionParams, server)
    root = server.documents[tdpp.textDocument.uri].blocks[1] # top level block
    stack = [root]
    return get_block_stack(stack, tdpp.position)
end

function get_block_stack(stack::Vector{Block}, pos::Position)
    b = stack[end]
    for sb in b.subblocks
        if pos in sb.range && sb.kind in Set(["module", "function", "macro", "type", "bitstype", "staggedfunction", "let"])
            push!(stack, sb)
            get_block_stack(stack, pos)
            return stack
        end
    end
    return stack
end

function get_blocks(word::AbstractString, tdpp::TextDocumentPositionParams, server)
    blocks = server.documents[tdpp.textDocument.uri].blocks
    out = Block[]
    for b in blocks
        if b.info.name == word
            push!(out, b)
        end
    end
    return out
end

function get_type(val)
    return try
                string(typeof(eval(val)))
           catch
                "Any"
           end
end

function get_type(word::String, tdpp::TextDocumentPositionParams, server)
    stack = get_block_stack(tdpp, server)
    rtype = "Any"
    stack == nothing && return rtype
    for b in reverse(stack)
        if word in keys(b.info.localvars)
            vinfo = b.info.localvars[word]
            return vinfo.t
        end
    end
    return rtype    
end