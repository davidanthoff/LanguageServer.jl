using LanguageServer
using Base.Test

tests = ["communication"]

for t in tests
    fp = joinpath("test_$t.jl")
    println("$fp ...")
    include(fp)
end