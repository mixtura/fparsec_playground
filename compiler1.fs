open FParsec
open System
open System.Collections.Generic

(*  Example

    when message 'hello'
    and author 'vasya'
    then save
    then send 'hi' 
    and wait for 10 sec
*)

module Utils =
    let apply2 f x y = f(x, y)
    let applyTuple2 f (x, y) = f x y

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

    type Rule = Rule of Predicate * Command list

module Evaluation =
    open AST

    let getVarContent (variables:IDictionary<string, string>) varName = variables.[varName]

    let evaluateItem varContentGetter = function
    | Content content -> content
    | Variable var -> varContentGetter(var)

    let rec evaluatePredicate itemEvaluator = function
    | Equal(left, right) -> (itemEvaluator left) = (itemEvaluator right)
    | Like(left, right) -> (itemEvaluator left) = (itemEvaluator right)
    | Chain(first, last, op) ->
        let firstRes = evaluatePredicate itemEvaluator first
        let lastRes = evaluatePredicate itemEvaluator last

        match op with
        | And(false) -> firstRes && lastRes
        | And(true) -> firstRes && (not lastRes)
        | Or(false) -> firstRes || lastRes
        | Or(true) -> firstRes || (not lastRes)

    let evaluateCommand sender = function
    | Send(content) -> sender(content)
    | _ -> ()

    let evaluateRule predicateEvaluator commandEvaluator (predicate, commands) =  
        match predicateEvaluator predicate with
        | true -> commands |> List.map commandEvaluator |> ignore; ()
        | false -> ()

    let evaluate variables =
        let varContentGetter = getVarContent variables
        let sender = fun content -> printfn "Sent: %s" content
        let itemEvaluator = evaluateItem varContentGetter
        let predicateEvaluator = evaluatePredicate itemEvaluator
        let commandEvaluator = evaluateCommand sender
        let ruleEvaluator = evaluateRule predicateEvaluator commandEvaluator

        ruleEvaluator

module Parser =
    open Utils
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
            (pnot |> opt)
            (fun op not -> 
            match not with 
                | Some _ -> op true 
                | _ -> op false) 

    let identifier = 
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy isIdentifierChar

    let content = 
        let quote = pchar '''
        let isContent = isNoneOf (['''; '\n'])        
        between quote quote (many1Satisfy isContent)

    let item =
        let content = content >>= (Content >> preturn)
        let variable = identifier >>= (Variable >> preturn)
        variable <|> content

    let predicate =
        let leftItem = item .>> spaces1
        let rightItem = item 
        let equal leftItem = item >>= (apply2 Equal leftItem >> preturn)
        let like leftItem = plike .>> spaces1 >>. item >>= (apply2 Like leftItem >> preturn)

        leftItem >>= (fun leftItem -> like leftItem <|> equal leftItem)

    let createPredicatesChain prevPredicate links =
        let rec inner predicateOpList prevPredicate = 
            match predicateOpList with 
            | [] -> prevPredicate
            | (op, predicate)::rest -> Chain(prevPredicate, predicate, op) |> inner rest
        inner links prevPredicate
    
    let command = psend >>. spaces1 >>. content >>= (Send >> preturn)
    
    let instruction = 
        let predicatesChain prevParser = 
            prevParser .>>. many (operator .>> spaces1 .>>. predicate .>> newline) 
            >>= (applyTuple2 createPredicatesChain >> preturn)

        let commandsSequence = 
            many (pthen <|> pand .>> spaces1 >>. command .>> newline)

        let whenStatement = pwhen .>> spaces1 >>. predicate .>> newline |> predicatesChain
 
        whenStatement .>>. commandsSequence

module Program =
    open Parser
    open Evaluation

    [<EntryPoint>]
    let main argv =
        let instructionParserTests = [
            instruction, "when message like 'hello'\nand user 'user1'\nthen send 'hi'\nand send 'heloooo user1'\nthen send 'how are you?'\n"
        ]
        
        let test p str =
            match run p str with
            | Success(result, _, _)   -> printfn "Success: %A" result
            | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

        let variables = (new Dictionary<string, string>())

        let parseAndEvaluate p str =             
            match run p str with
            | Success(result, _, _)   -> evaluate variables result
            | Failure(errorMsg, _, _) -> printfn "Fail to parse: %s" errorMsg

        let testMany = List.map(fun (p, str) -> test p str)
        let parseAndEvaluateMany = List.map(fun (p, str) -> parseAndEvaluate p str)

        printfn "********** Testing **********"
        
        testMany instructionParserTests |> ignore

        printfn "********** Evaluation **********"

        variables.["message"] <- "hello"
        variables.["user"] <- "user1"

        parseAndEvaluateMany instructionParserTests |> ignore

        0 // return an integer exit code
