// 20.3.1
let vat n x = x * (1.0 + float n / 100.0)

// 20.3.2
let unvat n y = y / (1.0 + float n / 100.0)

// 20.3.3
let rec min f =
    let rec find n =
        if f(n) = 0 then n else find (n + 1)
    find 1