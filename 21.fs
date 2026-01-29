// 50.2.1
let fac_seq = 
    seq {
        let mutable current = 1
        let mutable fact = 1
        while true do
            yield fact
            current <- current + 1
            fact <- fact * current
    }

// 50.2.2
let seq_seq = 
    seq {
        let mutable n = 0
        let mutable positive = false
        while true do
            if n = 0 then
                yield 0
                n <- 1
            else
                if positive then
                    yield n
                    n <- n + 1
                    positive <- false
                else
                    yield -n
                    positive <- true
    }