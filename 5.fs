// 16.1
let notDivisible (n, m) = m % n = 0

// 16.2
let prime = function
    | 0 | 1 -> false
    | 2 -> true
    | n -> 
        let rec check = function
            | 1 -> true
            | k when notDivisible(k, n) -> false
            | k -> check (k - 1)
        check (n - 1)