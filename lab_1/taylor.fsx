let rec loop condition action modifier iter (value: float) tail =
    let next_value = modifier iter value
    if condition iter value then
        action value (loop condition action modifier (iter + 1) next_value tail)
    else tail  

let computeE iterations =
    loop (fun i _ -> i < iterations) (+) (fun iter value -> value*(1.0/float(iter+1))) 0 1.0 0.0

let e = computeE 20  

let accumulate f (cur: float) (iter: int) =
    loop (fun i _ -> i <= iter) (+) f 0 cur 0.0

let fact x = 
    loop (fun i _ -> i <= x ) (fun a b -> a*b) (fun i _ -> float(i + 1)) 0 1 1.0

let the_func (x: float) =
    (1. + x) * e ** -x

let taylor_dumb (iter: int) (x: float) =
    let f = fun (n: int) _ -> float( -1.0 ** (float(n)-1.0) * (float(n-1)/fact(n)) * x ** n ) in
    loop (fun i _ -> i < iter + 1) (+) f 2 1.0 0.0


let taylor_smart (iter: int) (x: float) =
    let x'n = function
        | _, 1 -> 1.  // a_1 = x (для n=1)
        | _, 2 -> -x*x/2.0  // a_2 = -x^2/2
        | a_n_prev, n -> a_n_prev * -x * float(n-1) / (float(n) * float(n-2))
    
    let f = fun n a_n ->
        if n = 0 then 1.0
        else x'n(a_n, n+1)
    
    accumulate f 0. iter

let print_table =
    printfn "%-8s %-15s %-15s %-15s" "x" "source_func" "taylor_dumb" "taylor_smart"

    let iter = 20
    for x in 0.1 .. 0.1 .. 1.0 do
        let s = the_func x
        let d = taylor_dumb iter x
        let t = taylor_smart iter x
        printfn "%-8.3f %-15.6f %-15.6f %-15.6f" x s d t

print_table