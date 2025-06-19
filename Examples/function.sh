#!/bin/env tush

opr Int a (Chr b Str c) something_impractical Int d = {
  echo (b + c)
  return a + d }

1 ('a' "Boom") something_impractical 10
not_valid_yet("Bang!")

let a = 7.0
let b = 99.0

opr main Any _ = {
  opr Flt a insideAdd Flt b = {
    a + b
  }

  a insideAdd b
}

opr not_valid_yet Str word = {
  echo word
}

(10)insideAdd (11)
main
