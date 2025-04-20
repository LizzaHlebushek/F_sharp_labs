let dichotomy (f: float -> float) (a: float) (b: float) (epsilon: float) =
    let rec findRoot (a:float) (b:float) =
        let c = (a+b)/2.0
        if abs (f c) < epsilon then c
        elif f c * f a < 0.0 then findRoot a c
        else findRoot c b

    if f a * f b > 0.0 then 
        failwith "The function must have different signs at the ends of the interval."
    else 
        findRoot a b


let iteration (f: float -> float) (a: float) (b: float) (epsilon: float) =
    let rec findRoot (x: float) =
        let newx = f x
        if abs(newx - x) < epsilon then
            newx
        else 
            findRoot(newx)
    findRoot a

let newton f f'x (a: float) (b: float) (epsilon: float) =
    let rec findRoot (x: float) =
        let newx = x - (f x) / (f'x x)
        if abs(newx - x) < epsilon then
            newx
        else 
            findRoot newx
    findRoot a

// функция 22:
let f22 x = 
    acos x - sqrt (1.0 - 0.3 * x ** 3.0)
let f22' x =
    (-1.0 / sqrt(1.0 - x**2.0)) + (9.0 * x**2.0) / (2.0 * sqrt(100.0 - 30.0 * x**3.0))
let phi22 x = 
    cos (sqrt (1.0 - 0.3 * x ** 3.0))

// функция 23
let f23 x = 
    3.0 * x - 4.0 * log x - 5.0
let f23' x = 
    3.0 - 4.0 / x
let phi23 x = 
    (4.0 * log x + 5.0) / 3.0

// функция 24
let f24 x = 
    cos (2.0 / x) - 2.0 * sin (1.0 / x) + 1.0 / x
let f24' x =
    (2.0 / x**2.0) * sin (2.0 / x) + (2.0 / x**2.0) * cos (1.0 / x) - 1.0 / x**2.0
let phi24 (x: float) =
    x - 0.527 * (cos (2.0 / x) - 2.0 * sin (1.0 / x) + 1.0 / x)

let print_table_line label f f' phi (a: float) (b: float) (value: float) =
    let delta = 1e-7
    printfn "| %-10s | %-1.4f | %1.4f | %1.4f | %1.4f |" 
        label
        (iteration phi a b delta)
        (dichotomy f a b delta)
        (newton f f' a b delta)
        value

printfn "| %-10s | %-6s | %-6s | %-6s | %-6s |"
    "Функция" "Итер." "Дихот." "Ньютон" "Знач."

print_table_line "Функция 22" f22 f22' phi22 0. 1. 0.5629
print_table_line "Функция 23" f23 f23' phi23 2. 4. 3.23
print_table_line "Функция 24" f24 f24' phi24 1. 2. 1.8756
