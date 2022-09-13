namespace Bast.Cli

open Bast
open Farkle

module App =

    let private exampleCode =
        @"; this is a comment
(let foo ""the funny"")
(let x #\c)"

    [<EntryPoint>]
    let main _ =
        let parseResult = RuntimeFarkle.parseString Lexer.bastLexer exampleCode

        match parseResult with
        | Ok result -> printfn $"{result}"
        | Error farkleError -> printfn $"{farkleError}"

        0