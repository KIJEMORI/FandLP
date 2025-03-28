// Задание 1
printfn "Hello from F#"
let PI = 3.141592653589

// Задание 2
let solve(a,b,c) =
    let D = b*b-4.0*a*c
    ((-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a))
//System.Console.WriteLine(solve(1.0, 2.0, -3.0));;

let solve2 = fun (a,b,c) ->
    let D = b*b-4.0*a*c
    ((-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a))

let solve3 a b c =
    let D = b*b-4.0*a*c
    ((-b+sqrt(D))/(2.0*a), (-b-sqrt(D))/(2.0*a))

// Задание 3
let main3 r length:float =
    let Cilindric radius= 
        PI*radius*radius
    let CilinridVolume radius length:float = 
        (Cilindric radius)*length
    CilinridVolume r length
//System.Console.WriteLine(main3 9 10)

// Задание 4
let main4 =
    let rec recur item =
        if(item/10 > 0)
        then 
            item%10 + recur (item/10)
        else 
            item%10
    System.Console.WriteLine(recur 212)
//main4

// Задание 5
let main5 =
    let rec Recur n = 
        let rec recur item sum =
            let litle_item = item%10
            if item = 0
            then
                sum
            else 
                let sum = sum+litle_item
                let item = item/10
                recur item sum
            
        recur n 0
    System.Console.WriteLine(Recur 212)

//main5

// Задание 6

let main6 a =
    let func1 b = 
        let rec sumCifr item sum= 
            match item with
            |item when item > 0 -> sumCifr (item/10) (sum + item%10)
            | _ -> sum
                
        sumCifr b 0
    
    let func2 b = 
        let rec factorial item prod =
            match item with
            |item when item > 0 -> factorial (item-1) (prod*item)
            | _ -> prod
        factorial b 1

    match a with
    | true -> func1
    | false -> func2

System.Console.WriteLine(main6 false 5)

// Задание 7

let main7 digit funct init =
    let rec step digit rez=
        let next_rez = funct (digit%10) rez
        let next_digit = digit/10
   
        match next_digit with
        | next_digit when next_digit > 0 -> step next_digit next_rez
        | _ -> next_rez

    step digit init


// Задание 8
let sum a b = a+b

System.Console.WriteLine(main7 711 (fun x y -> x+y) 0)
System.Console.WriteLine(main7 271 (fun x y -> x*y) 1)
System.Console.WriteLine(main7 711 (fun x y -> match x>y with| true -> x| false -> y) 0)
System.Console.WriteLine(main7 711 (fun x y -> match x<y with| true -> x| false -> y) 9)

// Задание 9

let main9 digit funct init funcItsNeed=
    let rec step digit rez=
        let itsNeed = funcItsNeed (digit%10)
        let next_digit = digit/10

        let next_rez = funct rez (digit%10) 
       
        match (next_digit, itsNeed) with
        | (next_digit,true) when next_digit > 0 -> step next_digit next_rez
        | (next_digit,false) when next_digit > 0 -> step next_digit rez 
        | (next_digit,true) -> next_rez
        | _ -> rez

    step digit init

// Задание 10
System.Console.WriteLine("Task 10")
System.Console.WriteLine(main9 7112 (fun x y -> x+y) 0 (fun x -> match x%2 with |1 -> true |0->false))
System.Console.WriteLine(main9 271 (fun x y -> x*y) 1 (fun x -> true))
System.Console.WriteLine(main9 111 (fun x y -> match x>y with| true -> x| false -> y) 0 (fun x -> match x with |x when x>5 -> true|_ -> false))
System.Console.WriteLine(main9 711 (fun x y -> match x<y with| true -> x| false -> y) 9 (fun x -> false))

// Задание 11
System.Console.Write("Введите ваш любимый язык: ")
let favoriteLanguage = System.Console.ReadLine()

let main11 language =
    match language with
    |"F#"|"Prolog" -> "Подлиза"
    |_ -> "Нормис"
System.Console.WriteLine(main11 favoriteLanguage)

// Задание 12
//System.Console.Write("Введите ваш любимый язык: ")
//favoriteLanguage = System.Console.ReadLine()

let main12 (language) =
    match language with
    |"F#"|"Prolog" -> "Подлиза"
    |_ -> "Нормис"
//System.Console.WriteLine(main12 favoriteLanguage)


// Задание 13

let vzaSimple a b =
    let rec poisk a b index = 
        let ostB = b%index
        let ostA = a%index
        match (ostA, ostB) with
        |(0,0) -> false
        | _ when index < a -> poisk a b (index+1)
        | _ -> true
    match (a,b) with
    | (a,b) when a<b -> poisk a b 2
    | _ -> poisk b a 2

let main13 digit funct init =
    let rec step minidigit rez=
        let itsNeed = vzaSimple (minidigit%10) digit
        let next_digit = minidigit/10

        let next_rez = funct rez (minidigit%10) 
       
        match (next_digit, itsNeed) with
        | (next_digit,true) when next_digit > 0 -> step next_digit next_rez
        | (next_digit,false) when next_digit > 0 -> step next_digit rez 
        | (next_digit,true) -> next_rez
        | _ -> rez

    step digit init

System.Console.WriteLine(main13 365 (fun x y -> x+y) 0)
System.Console.WriteLine(main13 271828 (fun x y -> x+y) 0)


// Задание 15

let main15 digit funct init funcItsNeed=
    let rec step minidigit rez=
        let itsNeed = vzaSimple (minidigit%10) digit
        let itsNeed = itsNeed && funcItsNeed (minidigit%10)
        let next_digit = minidigit/10

        let next_rez = funct rez (minidigit%10) 
       
        match (next_digit, itsNeed) with
        | (next_digit,true) when next_digit > 0 -> step next_digit next_rez
        | (next_digit,false) when next_digit > 0 -> step next_digit rez 
        | (next_digit,true) -> next_rez
        | _ -> rez

    step digit init

System.Console.WriteLine(main15 365 (fun x y -> x+y) 0 (fun x -> match x with |x when x>3 -> true |_ -> false))
System.Console.WriteLine(main15 271828 (fun x y -> x+y) 0 (fun x -> match x with |x when x>1 -> true|_->false))

// Вариант 7
// Задание 16

let isSimple a =
    let rec obhod index =
        match index with
        |index when (a%index=0 && index <> a)-> false   
        |index when index < a -> obhod (index+1)
        |_ -> true
    obhod 2

let main16 a =
    let rec obhod index sum=
        match index with
        |index when (a%index=0 && isSimple index) -> obhod (index+1) (sum+index) 
        |index when (index >= a) -> sum
        |_ -> obhod (index+1) sum
    obhod 1 0
System.Console.WriteLine("Task 16")
System.Console.WriteLine(main16 257)
System.Console.WriteLine(main16 267)

// Задание 17

let main17 a =
    let rec obhod digit count=
        let thisdigit = digit%10
        let next_a = digit/10
        let isNeed = thisdigit> 3 && thisdigit%2=1
        match next_a with
        | 0 when (isNeed) -> count + 1
        | 0 -> count
        | next_a when (isNeed) -> obhod next_a (count + 1)
        | _ -> obhod next_a count
    obhod a 0

System.Console.WriteLine("Task 17")
System.Console.WriteLine(main17 257)
System.Console.WriteLine(main17 557)
System.Console.WriteLine(main17 222)
System.Console.WriteLine(main17 333)

// Задание 18

let main18 a = 
    let sumCifr b =
        let rec sumCifr1 digit sum =
            let minidigit = digit%10
            let next_digit = digit/10
            match next_digit with
            | 0 -> sum + minidigit
            | _ -> sumCifr1 next_digit (sum+minidigit)
        sumCifr1 b 0

    let a_sumCifr = sumCifr a

    let rec obhod index prod =
        let isNeed = (a%index = 0 && a_sumCifr > (sumCifr index))
        
        match index with
        | index when index = a -> prod
        | index when isNeed -> obhod (index+1) (prod*index)
        | _ -> obhod (index+1) prod
    obhod 1 1

System.Console.WriteLine("Task 18")
System.Console.WriteLine(main18 257)
System.Console.WriteLine(main18 557)
System.Console.WriteLine(main18 222)
System.Console.WriteLine(main18 333)

// Задание 20

let main20 (x,y) = 
    match x with
    |1 -> main16 y
    |2 -> main17 y
    |3 -> main18 y
    |_ -> 0
