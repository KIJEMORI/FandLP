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
