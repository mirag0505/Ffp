// 49.5.1
let even_seq = 
    Seq.initInfinite (fun i -> (i + 1) * 2)

// 49.5.2
let fac_seq = 
    let rec factorial n acc =
        seq {
            yield acc
            yield! factorial (n + 1) (acc * (n + 1))
        }
    factorial 0 1

// 49.5.3
let seq_seq = 
    Seq.initInfinite (fun i -> 
        if i = 0 then 0
        elif i % 2 = 0 then i / 2
        else -(i + 1) / 2)