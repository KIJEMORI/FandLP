let simple n =
    let rec Simple index =
        match index > n/2 with
        |true -> true
        |false when (n%index = 0 && index <> 1 && index <> n) -> false
        |_ -> Simple (index+1)    
    Simple 1

let rad n:int =
    let rec Rad index sum = 
        match index > n/2 with
        |true -> sum
        |false when (simple index && n%index=0) -> Rad (index+1) (sum*index)
        |_ -> Rad (index+1) sum
    match simple n with
    |true -> Rad 1 n
    |false -> Rad 1 1

//System.Console.WriteLine(rad 504)

type Widget ={ID:int; Rad:int}

let createRadLists n= 
    
    let rec add index list1=
        
        match index with
            | index when index <= n -> 
                let item = index
                let rad = rad item
                System.Console.WriteLine(string item + " " + string rad)
                add (index+1) (list1 @ [{ID = item; Rad = rad}])
            | _ -> list1
       
    add 1 []

let length = 100000

let nlist = createRadLists length

let compareWidgets widget1 widget2 =
   if widget1.Rad < widget2.Rad then -1 else
   if widget1.Rad > widget2.Rad then 1 else
   0

let sortlist = List.sortWith compareWidgets nlist
