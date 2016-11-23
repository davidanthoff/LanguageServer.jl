type SymbolInformation 
    name::String 
    kind::Int 
    location::Location 
end 
 
function process(r::JSONRPC.Request{Val{Symbol("textDocument/documentSymbol")},DocumentSymbolParams}, server) 
    uri = r.params.textDocument.uri 
    blocks = server.documents[uri].blocks 
    syms = SymbolInformation[]
    for b in blocks
        if b.kind in Set(["function", "staggedfunction", "macro"])
            push!(syms, SymbolInformation(b.info.fullname, 12, Location(uri, b.range)))
        elseif b.kind in Set(["type", "bitstype", "immutable", "abstract"])
            push!(syms, SymbolInformation(b.info.fullname, 5, Location(uri, b.range)))
        elseif b.kind in Set(["module"])
            push!(syms, SymbolInformation(b.info.fullname, 2, Location(uri, b.range)))
            for sb in b.subblocks
                if sb.kind == "assignment"
                    push!(syms, SymbolInformation(sb.info.name, 5, Location(uri, sb.range)))
                end
            end
        elseif b.kind in Set(["using", "import"])
            for name in keys(b.info.localvars)
                rtype = b.info.localvars[name].t
                index = rtype == "Module" ? 2 : 7
                push!(syms, SymbolInformation(name, index, Location(uri, b.range)))
            end
        end 
    end 
    response = JSONRPC.Response(get(r.id), syms) 
    send(response, server) 
end

function JSONRPC.parse_params(::Type{Val{Symbol("textDocument/documentSymbol")}}, params) 
    return DocumentSymbolParams(params) 
end