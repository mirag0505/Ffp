// 47.4.1
let f n =
    let mutable result = 1
    let mutable i = 1
    while i <= n do
        result <- result * i
        i <- i + 1
    result

// 47.4.2
let fibo n =
    if n = 0 then 0
    elif n = 1 then 1
    else
        let mutable prev = 0
        let mutable current = 1
        let mutable counter = 2
        
        while counter <= n do
            let next = prev + current
            prev <- current
            current <- next
            counter <- counter + 1
        
        current