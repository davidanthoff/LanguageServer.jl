function process(r::JSONRPC.Request{Val{Symbol("textDocument/definition")},TextDocumentPositionParams}, server)
    word = get_word(r.params, server)
    x = get_sym(word)

    locations = map(methods(x).ms) do m
        (filename, line) = functionloc(m)
        filename = "file:$filename"
        return Location(filename, line)
    end

    tdpp = r.params
    blocks = get_blocks(word, tdpp, server)
    for b in blocks
        filename = tdpp.textDocument.uri
        push!(locations, Location(filename, b.range.start.line))
    end

    response = JSONRPC.Response(get(r.id),locations)
    send(response, server)
end

function JSONRPC.parse_params(::Type{Val{Symbol("textDocument/definition")}}, params)
    return TextDocumentPositionParams(params)
end
