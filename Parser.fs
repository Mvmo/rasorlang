module Parser

open System
open System.Net.Sockets
open FParsec

type Expression =
    | IntLiteral
    | FloatLiteral
    
