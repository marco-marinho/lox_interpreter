module Lox.Scanner

open Lox

let is_digit c = c >= '0' && c <= '9'

let is_alpha c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let rec scan_string input acc idx line =
    if idx = String.length input then
        let _ = Error.error line "unterminated string" in None
    else
        match input.[idx] with
        | '"' -> Some(Token.make_token Token.String "string" (Token.StringLiteral acc) line, idx + 1, line)
        | '\n' -> scan_string input (acc + "\n") (idx + 1) (line + 1)
        | o -> scan_string input (acc + string o) (idx + 1) line

let rec scan_number input acc idx line =
    if idx = String.length input then
        try
            Some(string acc, idx)
        with Failure _ ->
            Error.error line "invalid number"
            None
    else
        match input.[idx] with
        | '.' ->
            if not (is_digit input.[idx + 1]) then
                try
                    Some(string acc, idx)
                with Failure _ ->
                    Error.error line "invalid number"
                    None
            else
                scan_number input (acc + ".") (idx + 1) line
        | n when is_digit n -> scan_number input (acc + string n) (idx + 1) line
        | _ ->
            try
                Some(string acc, idx)
            with Failure _ ->
                Error.error line "invalid number"
                None

let rec scan_identifier input acc idx line =
    let gen_token =
        function
        | "and" -> Token.And
        | "class" -> Token.Class
        | "else" -> Token.Else
        | "false" -> Token.False
        | "for" -> Token.For
        | "fun" -> Token.Fun
        | "if" -> Token.If
        | "nil" -> Token.Nil
        | "or" -> Token.Or
        | "print" -> Token.Print
        | "return" -> Token.Return
        | "super" -> Token.Super
        | "this" -> Token.This
        | "true" -> Token.True
        | "var" -> Token.Var
        | "while" -> Token.While
        | _ -> Token.Indentifier

    if idx = String.length input then
        (gen_token acc, acc, idx)
    else
        match input.[idx] with
        | n when is_alpha n -> scan_identifier input (acc + string n) (idx + 1) line
        | _ -> (gen_token acc, acc, idx)


let scann input =
    let rec aux acc idx line input =
        if idx = String.length input then
            acc
        else
            match input.[idx] with
            | '(' -> aux (Token.make_token Token.LeftParen "(" Token.Null line :: acc) (idx + 1) line input
            | ')' -> aux (Token.make_token Token.RightParen ")" Token.Null line :: acc) (idx + 1) line input
            | '{' -> aux (Token.make_token Token.LeftBrace "{" Token.Null line :: acc) (idx + 1) line input
            | '}' -> aux (Token.make_token Token.RightBrace "}" Token.Null line :: acc) (idx + 1) line input
            | ',' -> aux (Token.make_token Token.Comma "," Token.Null line :: acc) (idx + 1) line input
            | '.' -> aux (Token.make_token Token.Dot "." Token.Null line :: acc) (idx + 1) line input
            | '-' -> aux (Token.make_token Token.Minus "-" Token.Null line :: acc) (idx + 1) line input
            | '+' -> aux (Token.make_token Token.Plus "+" Token.Null line :: acc) (idx + 1) line input
            | ';' -> aux (Token.make_token Token.Semicolon ";" Token.Null line :: acc) (idx + 1) line input
            | '*' -> aux (Token.make_token Token.Star "*" Token.Null line :: acc) (idx + 1) line input
            | '\t'
            | ' '
            | '\r' -> aux acc (idx + 1) line input
            | '\n' -> aux acc (idx + 1) (line + 1) input
            | '!' ->
                if input.[idx + 1] = '=' then
                    aux (Token.make_token Token.BangEqual "!=" Token.Null line :: acc) (idx + 2) line input
                else
                    aux (Token.make_token Token.Bang "!" Token.Null line :: acc) (idx + 1) line input
            | '=' ->
                if input.[idx + 1] = '=' then
                    aux (Token.make_token Token.EqualEqual "==" Token.Null line :: acc) (idx + 2) line input
                else
                    aux (Token.make_token Token.Equal "=" Token.Null line :: acc) (idx + 1) line input
            | '<' ->
                if input.[idx + 1] = '=' then
                    aux (Token.make_token Token.LessEqual "<=" Token.Null line :: acc) (idx + 2) line input
                else
                    aux (Token.make_token Token.Less "<" Token.Null line :: acc) (idx + 1) line input
            | '>' ->
                if input.[idx + 1] = '=' then
                    aux (Token.make_token Token.GreaterEqual ">=" Token.Null line :: acc) (idx + 2) line input
                else
                    aux (Token.make_token Token.Greater ">" Token.Null line :: acc) (idx + 1) line input
            | '/' ->
                if input.[idx + 1] = '/' then
                    let rec skip_comment idx =
                        if idx = String.length input then
                            acc
                        else if input.[idx] = '\n' then
                            aux acc (idx + 1) (line + 1) input
                        else
                            skip_comment (idx + 1)

                    skip_comment (idx + 1)
                else
                    aux (Token.make_token Token.Slash "/" Token.Null line :: acc) (idx + 1) line input
            | '"' ->
                let result = scan_string input "" (idx + 1) line in

                match result with
                | Some(token, idx, line) -> aux (token :: acc) idx line input
                | None -> acc
            | n when is_digit n ->
                let results = scan_number input "" idx line in

                match results with
                | Some(number, idx) ->
                    aux
                        (Token.make_token Token.Number "number" (Token.NumberLiteral(float number)) line
                         :: acc)
                        idx
                        line
                        input
                | None -> acc
            | n when is_alpha n ->
                let (token, identifier, idx) = scan_identifier input "" idx line in
                aux (Token.make_token token identifier Token.Null line :: acc) idx line input
            | _ ->
                Error.error line "unexpected character"
                acc

    List.rev ((Token.make_token Token.EOF "EOF" Token.Null (-1)) :: (aux [] 0 1 input))
