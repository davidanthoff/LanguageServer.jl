function process(r::JSONRPC.Request{Val{Symbol("textDocument/hover")},TextDocumentPositionParams}, server)
    tdpp = r.params
    word = get_word(tdpp,server)
    sword = split(word,'.')
    if word == ""  
        send(JSONRPC.Response(get(r.id), Hover([])), server)
        return
    end

    documentation = get_hover(word, tdpp, server)
    isempty(documentation) && (word[end] == '.') && (documentation = get_hover(word[1:end-1], tdpp, server))
    isempty(documentation) && (documentation = get_docs(r.params, server))        
    response = JSONRPC.Response(get(r.id), Hover(documentation))
    send(response, server)
end

function JSONRPC.parse_params(::Type{Val{Symbol("textDocument/hover")}}, params)
    return TextDocumentPositionParams(params)
end

function get_hover(word::AbstractString, tdpp::TextDocumentPositionParams, server)
    stack = get_block_stack(tdpp, server)
    hover = MarkedString[]
    stack == nothing && return hover
    for b in reverse(stack)
        if word in keys(b.info.localvars)
            vinfo = b.info.localvars[word]
            push!(hover, MarkedString(vinfo.doc))
            return hover
        end
    end
    return hover
end