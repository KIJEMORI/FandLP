
let createList n= 
    
    let rec add index (list:list<int>)=
        
        match index with
            | index when index <= n -> 
                System.Console.Write(string index + ") ")
                let item = System.Console.ReadLine()
                add (index+1) (list @ [int item])
            | _ -> list
       
    add 1 []

let list1 = createList 5

System.Console.WriteLine("Вывод: ")
let rec printList list =
    match list with
        |head::tail -> 
            System.Console.WriteLine(string head)
            printList tail
        | [] -> System.Console.WriteLine("конец")

printList list1

let rec main3 list (f:int->int->int) (p:int->bool) (acc:int) =
    match list with
    | head::tail -> 
        match p head with
        |true -> acc = f head acc
        |_ -> acc = acc
        main3 tail f p acc
    | [] -> acc

let rez3 = main3 list1 (fun x y -> x+y) (fun x -> true) 0

let rez41 = main3 list1 (fun x y -> match x>y with |true -> y |false -> x) (fun x -> true) 1000
let rez42 = main3 list1 (fun x y -> x+y) (fun x -> x%2=0) 0
let rez43 = main3 list1 (fun x y -> x+1) (fun x -> x%2<>0) 0

let main5 list = 
    let rec search list11 max quan= 
        match list11 with
        |head::tail -> 
            let rec poisk list1 elem count= 
                match list1 with
                |head1::tail1 ->
                    match elem=head1 with
                    |true -> poisk tail1 elem (count+1)
                    |false -> poisk tail1 elem count
                |[] -> count
                    
            let coun = poisk tail head 1
            match coun > quan with
            |true -> search tail head coun
            |false -> search tail max quan       
        |[] -> (max, quan) 
        
    search list 0 0

System.Console.WriteLine(main5 list1)

type 'string btree = 
    Node of 'string * 'string btree * 'string btree
    | Nil


let prefix root left right = (root(); left(); right())
let infix root left right = (left(); root(); right())
let postfix root left right = (left(); right(); root())


let f7 list = List.nth list (List.findIndex (fun x -> x = (List.max (List.map (fun el -> List.length (List.filter (fun elem -> (elem = el)) list)) list))) (List.map (fun el -> List.length (List.filter (fun elem -> (elem = el)) list)) list))   

System.Console.WriteLine(f7 list1)

let f8 list = List.length (List.filter (fun x -> (List.exists (fun el -> el * el = x) list)) list)
System.Console.WriteLine(f8 list1)

let rec cifrSum (n : int) : int = 
    match n with
    |0 -> 0
    |_ -> (n%10) + (cifrSum (n / 10))

let delCount n = 
    let rec delCount n index count = 
        match index with 
        |n -> count + 1
        |_ ->   
                match (n % index) with 
                |0 ->  delCount n (index+1) (count+1)
                |_ ->  delCount n (index+1) count
    delCount n 1 0

let f9 list1 list2 list3 = List.zip3 (List.rev (List.sort list1)) (List.sortBy (fun x -> (cifrSum x)) list2) (List.rev (List.sortBy (fun x -> (delCount x)) list3))

let list2 = f9 list1 list1 list1
printf "%A" list2

let createListString n= 
    
    let rec add index (list:list<string>)=
        
        match index with
            | index when index <= n -> 
                System.Console.Write(string index + ") ")
                let item = System.Console.ReadLine()
                add (index+1) (list @ [item])
            | _ -> list
       
    add 1 []

let list11 = createListString 5

let f10 list1 = (List.sortBy (fun x -> String.length(x)) list1)
let list21 = f10 list11
printf "%A" list21
