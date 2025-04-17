// Задание 1

[<AbstractClass>]

type Shape() =
    abstract member Area: float
 

type IPrint =
    abstract member Print: unit -> unit
 

type Rectangle(width: int, height: int) =
    inherit Shape()
    member this.Width = width
    member this.Height = height
    override this.Area = float this.Width * float this.Height
    override this.ToString() =
        sprintf "ширина = %d, высота = %d, площадь = %.2f" this.Width this.Height this.Area
    interface IPrint with
        member this.Print() = System.Console.WriteLine(this.ToString())
 
type Square(side: int) =
    inherit Rectangle(side, side)

let PI = 3.14

type Circle(radius: int) =
    inherit Shape()
    member this.Radius = radius
    override this.Area = PI * float this.Radius * float this.Radius
    override this.ToString() =
        sprintf "Круг: радиус = %d, площадь = %.2f" radius this.Area
    interface IPrint with
        member this.Print() = System.Console.WriteLine(this.ToString())

let rect = Rectangle(4, 5)
let square = Square(3)
let circle = Circle(2)

(rect :> IPrint).Print()
(square :> IPrint).Print()
(circle :> IPrint).Print()

// Задание 2
type Maybe<'a> =
    | Just of 'a
    | None

module Functor =
    let map f maybe =
        match maybe with
        | Just x -> Just (f x)
        | None -> None

module Applicative =
    let pure' x = Just x
    let apply maybeF maybeX =
        match maybeF, maybeX with
        | Just f, Just x -> Just (f x)
        | _ -> None
 
module Monada =
    let bind f maybe =
        match maybe with
        | Just x -> f x
        | None -> None
    let return' = Applicative.pure'
 
let testFunctorLaws() =
    let law1 = Functor.map id (Just 5) = Just 5
    let f x = x + 1
    let g x = x * 2
    let law2 = 
        Functor.map (f >> g) (Just 5) = (Functor.map g << Functor.map f) (Just 5)
    law1 && law2
 
let testApplicativeFunctorLaws() =
    let law1 = 
        Applicative.apply (Applicative.pure' id) (Just 5) = Just 5
    let compose f g x = f (g x)
    let law2 = 
        Applicative.apply (
            Applicative.apply (
                Applicative.apply (
                    Applicative.pure' compose
                ) (Applicative.pure' ((+) 1))
            ) (Applicative.pure' ((*) 2))
        ) (Just 5) = Just (1 + (2 * 5))
    // Закон гомоморфизма
    let law3 = 
        Applicative.apply (Applicative.pure' ((*) 2)) (Applicative.pure' 3) = Applicative.pure' (2 * 3)
    law1 && law2 && law3
 
let testMonadLaws() =
    // Левая идентичность
    let f x = Just (x * 2)
    let law1 = 
        Monada.bind f (Monada.return' 5) = f 5

    // Правая идентичность
    let law2 = 
        Monada.bind Monada.return' (Just 5) = Just 5

    // Ассоциативность
    let g x = Just (x + 3)
    let law3 = 
        Monada.bind g (Monada.bind f (Just 5)) = Monada.bind (fun x -> Monada.bind g (f x)) (Just 5) 
    law1 && law2 && law3

printfn "Законы функтора выполняются: %b" (testFunctorLaws())
printfn "Законы аппликативного функтора выполняются: %b" (testApplicativeFunctorLaws())
printfn "Законы монады выполняются: %b" (testMonadLaws())

let double x = x * 2

let maybeDouble = Applicative.pure' double

printfn "Результат map: %A" (Functor.map double (Just 5))
printfn "Результат apply: %A" (Applicative.apply maybeDouble (Just 3))
printfn "Результат bind: %A" (Monada.bind (fun x -> Just (x * 3)) (Just 4))

// Задание 3
open FParsec

type Expr =
    | Number of float
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

let numberParser = pfloat |>> Number
let ws = spaces
let operatorParser op =
    between ws ws (skipChar op) |>> (fun () -> op)

let expr, exprRef = createParserForwardedToRef()

let term = 
    numberParser <|>
    between (skipChar '(' >>. ws) (ws >>. skipChar ')') expr

let mulDivOp = operatorParser '*' <|> operatorParser '/'
let addSubOp = operatorParser '+' <|> operatorParser '-'
let mulDiv = 
    term .>>. many (mulDivOp .>>. term)
    |>> fun (first, rest) ->
        rest |> List.fold (fun acc (op, term) ->
            if op = '*' then Mul(acc, term)
            else Div(acc, term)
        ) first
 
let addSub = 
    mulDiv .>>. many (addSubOp .>>. mulDiv)
    |>> fun (first, rest) ->
        rest |> List.fold (fun acc (op, term) ->
            if op = '+' then Add(acc, term)
            else Sub(acc, term)
        ) first

do exprRef := addSub

let fullParser = ws >>. expr .>> ws .>> eof
 
let parseExpression str =
    match run fullParser str with
    | Success(result, _, _) -> Some result
    | Failure(errorMsg, _, _) -> 
        printfn "Ошибка парсинга: %s" errorMsg
        None
 
let rec eval = function
    | Number n -> n
    | Add(a, b) -> eval a + eval b
    | Sub(a, b) -> eval a - eval b
    | Mul(a, b) -> eval a * eval b
    | Div(a, b) -> eval a / eval b

let testCases = [
    "2+3+4"
    "(2+3)*4"
    "3.5*(4-2/1)"
    "10/(2+3)"
]
 
testCases |> List.iter (fun test ->
    printfn "\nПарсинг выражения: %s" test
    match parseExpression test with
    | Some expr ->
        printfn "Структура: %A" expr
        printfn "Результат: %.2f" (eval expr)
    | None -> ()
)
