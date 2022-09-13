namespace Bast

open System
open System.Globalization
open System.Text
open Bast
open Farkle
open Farkle.Builder

module Lexer =

    let private strTransform = T (fun ctx span ->
        let mutable span = span.Slice(1, span.Length - 2)
        let sb = StringBuilder(span.Length)

        while not span.IsEmpty do
            match span.IndexOf('\\') with
            | -1 ->
                sb.Append(span) |> ignore
                span <- ReadOnlySpan.Empty
            | backslashIdx ->
                let charactersBeforeBackslash = span.Slice(0, backslashIdx)
                sb.Append(charactersBeforeBackslash) |> ignore
                let escapePayloadLength =
                    match span.[backslashIdx + 1] with
                    | 'a' -> sb.Append '\a' |> ignore; 1
                    | 'b' -> sb.Append '\b' |> ignore; 1
                    | 'f' -> sb.Append '\f' |> ignore; 1
                    | 'n' -> sb.Append '\n' |> ignore; 1
                    | 'r' -> sb.Append '\r' |> ignore; 1
                    | 't' -> sb.Append '\t' |> ignore; 1
                    | 'u' ->
                        let hexCode =
                            UInt16.Parse(span.Slice(backslashIdx + 2, 4), NumberStyles.HexNumber)
                        sb.Append(Operators.char hexCode) |> ignore
                        5
                    | 'v' -> sb.Append '\v' |> ignore; 1
                    | c -> sb.Append c |> ignore; 1

                span <- span.Slice(backslashIdx + 1 + escapePayloadLength)

        let loc = Location(ctx.StartPosition, ctx.EndPosition)
        (sb.ToString(), loc))

    let private makeToken factory value location =
        let expr = factory value in
        Token(expr, location)

    let private intLiteral =
        Terminals.signedIntRegex
        |> terminal "int"
               (T(
                fun ctx span ->
                    let i = Int32.Parse(span) in
                    let loc = Location(ctx.StartPosition, ctx.EndPosition) in
                    (i, loc))
               )

    let private charLiteral =
        Regex.concat [
            Regex.string @"#\"
            Regex.chars PredefinedSets.ANSIPrintable
        ]
        |> terminal "char"
               (T(
                fun ctx span ->
                    let ch = char (span.Slice(2).ToString()) in
                    let loc = Location(ctx.StartPosition, ctx.EndPosition) in
                    (ch, loc))
               )

    (* escapes, unicode escape, multiline, delimiter, name *)
    let private stringLiteral =
        Regex.concat [
            Regex.char '"'
            Regex.star <| Regex.choice [
                Regex.allButChars ['"'; '\\']
                Regex.concat [
                    Regex.char '\\'
                    Regex.choice [
                        Regex.char '\\'
                        Regex.char '"'
                        Regex.chars "abfnrtv"
                        Regex.char 'u' <&> (Regex.repeat 4 <| Regex.chars "1234567890ABCDEFabcdef")
                    ]
                ]
            ]
            Regex.char '"'
        ]
        |> terminal "string" strTransform

    let private trueBool =
        Regex.string "#t"
        |> terminal "true"
               (T(
                fun ctx _ ->
                    let loc = Location(ctx.StartPosition, ctx.EndPosition) in
                    (true, loc))
               )

    let private falseBool =
        Regex.string "#f"
        |> terminal "false"
               (T(
                fun ctx _ ->
                    let loc = Location(ctx.StartPosition, ctx.EndPosition) in
                    (false, loc))
               )

    let private boolLiteral = "boolean" ||= [
        !@ trueBool |> asIs
        !@ falseBool |> asIs
    ]

    let private symbol =
        Regex.regexString "[a-zA-Z_+\-*/\\\=<>!&]+"
        |> terminal "symbol"
               (T(
                fun ctx span ->
                    let s = span.ToString() in
                    let loc = Location(ctx.StartPosition, ctx.EndPosition) in
                    (s, loc))
               )

    let private expression = "expression" ||= [
        !@ charLiteral => fun chLit -> chLit ||> makeToken Char
        !@ stringLiteral => fun strLit -> strLit ||> makeToken String
        !@ intLiteral => fun intLit -> intLit ||> makeToken Integer
        !@ boolLiteral => fun boolLit -> boolLit ||> makeToken Bool
        !@ symbol => fun symLit -> symLit ||> makeToken Symbol
    ]

    let private sExpr = nonterminal "s-expression"
    sExpr.SetProductions(
        !@ expression => Atom,
        !& "(" .>>. many sExpr .>> ")" => SList
    )

    let bastLexer =
        many sExpr
        |> DesigntimeFarkle.caseSensitive true
        |> DesigntimeFarkle.addLineComment ";"
        |> RuntimeFarkle.build
