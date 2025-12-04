// 23.4.1
let toCopper (g, s, c) = g * 240 + s * 12 + c  

let fromCopper totalCopper =
    let gold = totalCopper / 240
    let remainder = totalCopper % 240
    let silver = remainder / 12
    let copper = remainder % 12
    (gold, silver, copper)

let (.+.) (g1, s1, c1) (g2, s2, c2) =
    let totalCopper = toCopper (g1, s1, c1) + toCopper (g2, s2, c2)
    fromCopper totalCopper

let (.-.) (g1, s1, c1) (g2, s2, c2) =
    let totalCopper = toCopper (g1, s1, c1) - toCopper (g2, s2, c2)
    fromCopper totalCopper

// 23.4.2
let neg (x, y) = (-x, -y)
let inv (x, y) = 
    let denom = x * x + y * y
    (x / denom, -y / denom)

let (.+) (a, b) (c, d) = (a + c, b + d)
let (.-) (a, b) (c, d) = (a, b) .+ (neg (c, d))
let (.*) (a, b) (c, d) = (a * c - b * d, b * c + a * d)
let (./) (a, b) (c, d) = (a, b) .* (inv (c, d))