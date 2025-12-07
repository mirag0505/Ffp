type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) x y = ...