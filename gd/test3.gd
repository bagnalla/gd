
const A := 123
const B := 2.04
const C := "hello"

onready var myvar : bool = true 

func f(x : int) -> bool:
  return false if x < B else myvar

# func g(x : String) -> String:
#   return x + " world" # should work
