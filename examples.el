;; -*- lexical-binding: t -*-

;; Only the first three examples per function are shown in the docs,
;; so make those good.

(require 's)

(defexamples s-trim
  (s-trim "trim ") => "trim"
  (s-trim " this") => "this"
  (s-trim " only  trims beg and end  ") => "only  trims beg and end")

(defexamples s-chop-suffix
  (s-chop-suffix "-test.js" "penguin-test.js") => "penguin"
  (s-chop-suffix "\n" "no newlines\n") => "no newlines"
  (s-chop-suffix "\n" "no newlines") => "no newlines")

(defexamples s-ends-with-p
  (s-ends-with-p ".md" "readme.md") => t
  (s-ends-with-p ".md" "readme.txt") => nil)
