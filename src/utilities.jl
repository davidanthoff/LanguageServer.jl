function get_line(uri::AbstractString, line::Integer, server::LanguageServerInstance)
    doc = server.documents[uri]
    return get_line(doc, line)
end

function get_line(tdpp::TextDocumentPositionParams, server::LanguageServerInstance)
    return get_line(tdpp.textDocument.uri, tdpp.position.line+1, server)
end

function get_word(tdpp::TextDocumentPositionParams, server::LanguageServerInstance, offset=0)
    line = IOBuffer(get_line(tdpp, server))
    word = Char[]
    e = s = 0
    c = ' '
    while position(line) < tdpp.position.character+offset
        e += 1
        c = read(line, Char)
        push!(word, c)
        !(Base.is_id_char(c) || c=='.') && empty!(word)
    end
    while !eof(line) && Base.is_id_char(c)
        e += 1
        c = read(line, Char)
        Base.is_id_char(c) && push!(word, c)
    end
    for i = 1:5 # Delete junk at front
        !isempty(word) && (word[1] in [' ','.','!'] || '0'≤word[1]≤'9') && deleteat!(word, 1)
    end
    isempty(word) && return ""
    return String(word)
end

function get_sym(str::AbstractString)
    name = split(str, '.')
    try
        x = getfield(Main, Symbol(name[1]))
        for i = 2:length(name)
            x = getfield(x, Symbol(name[i]))
        end
        return x
    catch
        return nothing
    end
end

function uri2filepath(uri::AbstractString)
    uri_path = normpath(unescape(URI(uri).path))

    if is_windows()
        if uri_path[1]=='\\'
            uri_path = uri_path[2:end]
        end

        uri_path = lowercase(uri_path)
    end
    return uri_path
end

function should_file_be_linted(uri, server)
    !server.runlinter && return false

    uri_path = uri2filepath(uri)

    workspace_path = server.rootPath

    if is_windows()
        workspace_path = lowercase(workspace_path)
    end

    if server.rootPath==""
        return false
    else
        return startswith(uri_path, workspace_path)
    end
end


sprintrange(range::Range) = "($(range.start.line+1),$(range.start.character)):($(range.stop.line+1),$(range.stop.character+1))" 