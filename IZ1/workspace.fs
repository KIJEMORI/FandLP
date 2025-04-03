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

System.Console.WriteLine(rad 504)
