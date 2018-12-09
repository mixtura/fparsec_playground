open FParsec
open System

module AST =
    type Item = 
        | Variable of string 
        | Content of string
    
    type Op = | And of invered: bool | Or of inversed: bool 

    type Predicate = 
        | Equal of left:Item * right:Item
        | Like of left:Item * right:Item 
        | Chain of first: Predicate * last: Predicate * op: Op
    
    type Repeation = 
        | Every of period: TimeSpan 
        | Number of times: int
        | Until of Predicate

    type Wait =
        | For of period: TimeSpan
        | To of date: DateTime
        | Until of Predicate
       
    type Command = 
        | Send of content:string 
        | Wait of Wait * Command 
        | Repeat of Repeation * Command 
        | Chain of first:Command * last:Command

module Parser =
    open AST
    
    // joiners
    let pwhen = pstring "when"
    let pthen = pstring "then"
    let pand = pstring "and"
    let por = pstring "or"
    let pnot = pstring "not"
    let psend = pstring "send"
    let plike = pstring "like"

    // let puntil = pstring "until"
    // let pwait = pstring "wait"
    // let prepeat = pstring "repeat"
    // let pevery = pstring "every"
        
    let operator =  
        let andOp = pand >>% And;
        let orOp = por >>% Or;
        let mainOp = andOp <|> orOp
        
        pipe2
            mainOp
            (spaces1 >>. pnot |> opt)
            (fun op not -> 
            match not with 
                | Some _ -> op true 
                | _ -> op false) 

    let identifier = 
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy isIdentifierChar

    let item =
        let pcontent = isNoneOf ([''']) |> many1Satisfy
        let quote = pchar '''
        let content = between quote quote pcontent >>= (Content >> preturn)
        let variable = identifier >>= (Variable >> preturn)

        variable <|> content

    let predicate =
        let leftItem = item .>> spaces1  
        let rightItem = item 
        let equal = preturn Equal
        let like = plike .>> spaces1 >>= (fun _ -> Like |> preturn) 

        leftItem .>>. (like <|> equal) .>>. rightItem >>= (fun ((leftItem, predicateType), rightItem) -> predicateType(leftItem, rightItem) |> preturn) 

    let command = 
        let send = psend >>. spaces1 >>. many1Chars anyChar >>= (Send >> preturn)
        send

    let instruction = 
        pwhen 
        >>. spaces1
        >>. predicate
        .>> newline
        .>>. many (pand >>. spaces1 >>. predicate .>> newline)
        .>> pthen
        .>> spaces1
        .>>. command
        

open Parser

[<EntryPoint>]
let main argv =
    let itemParserTests = [
        run item "'content item'";
        run item "variableItem"
    ]

    let operatorsParserTests = [
        run operator "or";
        run operator "and";
        run operator "or not";
        run operator "and not"
    ]

    let predicateParserTests = [
        run predicate "message 'hello'";
        run predicate "message like 'hello'"
    ]

    let commandParserTests = [
        run command "send 'some message'"
    ]

    let instructionParserTests = [
        instruction, "when message like 'hello'\nand user 'user1'\nthen send 'hi'"
    ]
    
    let test p str =
        match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    let testMany = List.map(fun (p, str) -> test p str)

    testMany instructionParserTests |> ignore

    0 // return an integer exit code
