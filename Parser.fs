module Parser

open System
open FParsec

type BinaryOperationType =
    | Add
    | Subtract
    | Multiply
    | Divide

type Expression =
    | IntLiteral of int
    | FloatLiteral of float32
    | Identifier of String
    | BinaryOperation of (Expression * Expression * BinaryOperationType)
    
type Statement =
    | Assign
    | ExpressionStatement
    
type Program = {
    Statements: Statement list
}

let quote : Parser<_, unit> = skipChar '\"'
let whitespace : Parser<_, unit> = skipMany (skipChar ' ')
let whitespace1 : Parser<_, unit> = skipMany1 (skipChar ' ')

let parseIntOrFloatLiteral =
    numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"
    |>> fun n ->
        if n.IsInteger then Expression.IntLiteral (int n.String)
        else Expression.FloatLiteral (float32 n.String)
   .>> whitespace
   
let identifierParser = many1Chars (letter) |>> Expression.Identifier .>> whitespace

let operatorPrecedenceParser = OperatorPrecedenceParser<Expression, _, _>()

operatorPrecedenceParser.TermParser <- choice [
    parseIntOrFloatLiteral
    identifierParser
]

operatorPrecedenceParser.AddOperator <| InfixOperator ("*", whitespace, 1, Associativity.Left, fun x y -> BinaryOperation (x, y, Multiply))
operatorPrecedenceParser.AddOperator <| InfixOperator ("/", whitespace, 2, Associativity.Left, fun x y -> BinaryOperation (x, y, Divide))
operatorPrecedenceParser.AddOperator <| InfixOperator ("+", whitespace, 3, Associativity.Left, fun x y -> BinaryOperation (x, y, Add))
operatorPrecedenceParser.AddOperator <| InfixOperator ("-", whitespace, 4, Associativity.Left, fun x y -> BinaryOperation (x, y, Subtract))

let expressionParser = operatorPrecedenceParser.ExpressionParser

let assignStatementParser = skipString "let" >>. whitespace >>. identifierParser .>> 