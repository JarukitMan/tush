#!/bin/env tush

opr Int a (Chr b Str c) something_impractical Int d = {
  echo (b + c)
  return a + d }

1 ('a' "Boom") something_impractical 10
# not_valid_yet("Bang!")

let a = 7.0
let b = 99.0

opr main = {
  opr Flt a insideAdd Flt b = {
    a + b
  }

  a insideAdd b
  echo MAIN DONE
}

opr not_valid_yet Str word = {
  echo word
}

# (10)insideAdd (11)
(main)

# You can kind of assign return types like this. Kind of. Composite types aren't allowed.
opr fib Int n = int (
  if n > 1 (
    return fib(n - 1) + fib(n - 2)
  )
  return n
)

opr fib_loop Int n = (
  let a = 0
  let b = 1
  _ = for i in 1..n (
    let temp = a
    a = a + b
    b = temp
  )
  return int a
)

echo FIB time! INPUT HERE!
echo (fib (int input))
echo FIB time! But fast! INPUT HERE!
echo (fib_loop (int input))
