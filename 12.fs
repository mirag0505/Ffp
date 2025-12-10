let rec append x = function
    | [] -> [x]
    | h::t -> h :: append x t

// 34.1
let rec upto = function
    | n when n <= 0 -> []
    | n -> append n (upto (n - 1))

// 34.2
let rec dnto = function
    | n when n <= 0 -> []
    | n -> n :: dnto (n - 1)

// 34.3
let rec evenn = function
    | n when n <= 0 -> []
    | n -> append (2 * (n - 1)) (evenn (n - 1))