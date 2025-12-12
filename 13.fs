// 39.1
let rec rmodd = function
    | [] -> []
    | [_] -> []
    | first :: second :: tail -> second :: rmodd tail

// 39.2
let rec del_even = function
    | [] -> []
    | first :: tail when first % 2 = 0 -> del_even tail
    | first :: tail -> first :: del_even tail

// 39.3
// паттерн матчинг всегда принимает поледний аргумент, а если хочется получить второй, это нужна явно делать, а если оба -- тогда использовать два вложенных друг в друга паттерн матчинга
let rec multiplicity x = function
    | [] -> 0
    | head :: tail when head = x -> 1 + multiplicity x tail
    | head :: tail -> multiplicity x tail

// 39.4
let rec split = function
    | [] -> ([], [])
    | [x] -> ([x], [])
    | x1 :: x2 :: rest -> 
        let (first, second) = split rest
        (x1 :: first, x2 :: second)

// 39.5
let rec zip = function
    | ([], []) -> []
    | (x1::tail1, x2::tail2) -> (x1, x2) :: zip (tail1, tail2)
    | _ -> failwith "Different lengths"