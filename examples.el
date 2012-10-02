;; -*- lexical-binding: t -*-

;; Only the first three examples per function are shown in the docs,
;; so make those good.

(require 's)

(defexamples s-trim
  (s-trim "trim ") => "trim"
  (s-trim " this") => "this"
  (s-trim " only  trims beg and end  ") => "only  trims beg and end")

(defexamples s-trim-left
  (s-trim-left "trim ") => "trim "
  (s-trim-left " this") => "this")

(defexamples s-trim-right
  (s-trim-right "trim ") => "trim"
  (s-trim-right " this") => " this")

(defexamples s-collapse-whitespace
  (s-collapse-whitespace "only   one space   please") => "only one space please"
  (s-collapse-whitespace "collapse \n all \t sorts of \r whitespace") => "collapse all sorts of whitespace")

(defexamples s-chop-suffix
  (s-chop-suffix "-test.js" "penguin-test.js") => "penguin"
  (s-chop-suffix "!" "exclamation marks!") => "exclamation marks"
  (s-chop-suffix "!" "exclamation marks!!") => "exclamation marks!"
  (s-chop-suffix "\n" "no newlines\n") => "no newlines")

(defexamples s-ends-with-p
  (s-ends-with-p ".md" "readme.md") => t
  (s-ends-with-p ".md" "readme.txt") => nil
  (s-ends-with-p ".md" "md") => nil)

(defexamples s-starts-with-p
  (s-starts-with-p "lib/" "lib/file.js") => t
  (s-starts-with-p "test/" "lib/file.js") => nil
  (s-starts-with-p "lib/" "lib") => nil)

(defexamples s-contains-p
  (s-contains-p "file" "lib/file.js") => t
  (s-contains-p "^a" "its not ^a regexp, is it?") => t)

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
