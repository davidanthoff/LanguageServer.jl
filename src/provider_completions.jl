function process(r::JSONRPC.Request{Val{Symbol("textDocument/completion")},TextDocumentPositionParams}, server)
    tdpp = r.params
    line = get_line(tdpp, server)
    word = get_word(tdpp, server)
    
    comp = Base.REPLCompletions.completions(line, tdpp.position.character)[1]
    n = length(comp)
    comp = comp[1:min(length(comp),25)]
    CIs = map(comp) do i
        s = get_sym(i)
        d = ""
        d = get_docs(s)
        d = isa(d,Vector{MarkedString}) ? (x->x.value).(d) : d
        d = join(d[2:end],'\n')
        d = replace(d,'`',"")
        
        kind = 6
        if isa(s, String)
            kind = 1
        elseif isa(s, Function)
            kind = 3
        elseif isa(s, DataType)
            kind = 7
        elseif isa(s, Module)
            kind = 9
        elseif isa(s, Number)
            kind = 12
        elseif isa(s, Enum)
            kind = 13
        end
        CompletionItem(i, kind, d)
    end
    static_list = get_static_completions(word, tdpp, server)
    for l in static_list
        push!(CIs, l)
    end
    n = length(CIs)
    completion_list = CompletionList(25<n,CIs)

    response =  JSONRPC.Response(get(r.id), completion_list)
    send(response, server)
end

function JSONRPC.parse_params(::Type{Val{Symbol("textDocument/completion")}}, params)
    return TextDocumentPositionParams(params)
end

function get_static_completions(word::String, tdpp::TextDocumentPositionParams, server)
    list = CompletionItem[]
    type_blocks = get_blocks(get_type(word[1:end-1], tdpp, server), tdpp, server)
    if !isempty(type_blocks)
        for (name, info) in type_blocks[1].info.localvars
            push!(list, CompletionItem(name, map_kind(info.t), info.doc)) 
        end
    end
    length(list) >= 1 && return list
    for b in get_block_stack(tdpp, server)
        if startswith(b.info.name, word)
            push!(list, CompletionItem(b.info.name, map_kind(b.kind), b.info.doc)) 
        end
        for (name, info) in b.info.localvars
            if startswith(name, word)
                push!(list, CompletionItem(name, map_kind(info.t), info.doc))
            end
        end
    end
    return list
end

function map_kind(kind)
    if kind in ["module", "baremodule"]
        return 9
    elseif kind in ["type", "bitstype", "immutable"]
        return 7
    elseif kind in ["function", "staggedfunction", "macro"]
        return 3
    end
    return 6
end