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
  (s-ends-with-p ".md" "readme.txt") => nil
  (s-ends-with-p ".md" "md") => nil)

(defexamples s-starts-with-p
  (s-starts-with-p "lib/" "lib/file.js") => t
  (s-starts-with-p "test/" "lib/file.js") => nil
  (s-starts-with-p "lib/" "lib") => nil)

(defexamples s-split-words
  (s-split-words "under_score") => '("under" "score")
  (s-split-words "some-dashed-words") => '("some" "dashed" "words")
  (s-split-words "evenCamelCase") => '("even" "Camel" "Case"))

(defexamples s-lower-camel-case
  (s-lower-camel-case "some words") => "someWords"
  (s-lower-camel-case "dashed-words") => "dashedWords"
  (s-lower-camel-case "under_scored_words") => "underScoredWords")

(defexamples s-upper-camel-case
  (s-upper-camel-case "some words") => "SomeWords"
  (s-upper-camel-case "dashed-words") => "DashedWords"
  (s-upper-camel-case "under_scored_words") => "UnderScoredWords")

(defexamples s-snake-case
  (s-snake-case "some words") => "some_words"
  (s-snake-case "dashed-words") => "dashed_words"
  (s-snake-case "camelCasedWords") => "camel_cased_words")

(defexamples s-dashed-words
  (s-dashed-words "some words") => "some-words"
  (s-dashed-words "under_scored_words") => "under-scored-words"
  (s-dashed-words "camelCasedWords") => "camel-cased-words")

(defexamples s-capitalized-words
  (s-capitalized-words "some words") => "Some Words"
  (s-capitalized-words "under_scored_words") => "Under Scored Words"
  (s-capitalized-words "camelCasedWords") => "Camel Cased Words")
