module Queue

type Queue<'a> = {front: 'a list; rear: 'a list}

let empty = {front = []; rear = []}

// Добавляет элемент в конец очереди
let put e q = {q with rear = e::q.rear}

// Извлекает элемент из начала очереди
let get q =
    match q.front, q.rear with
    | [], [] -> failwith "Queue is empty"
    | h::t, _ -> (h, {front = t; rear = q.rear})
    | [], _ -> 
        let newFront = List.rev q.rear
        let element = List.head newFront
        let newQueue = {front = List.tail newFront; rear = []}
        (element, newQueue)