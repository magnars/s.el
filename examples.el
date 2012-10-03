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

(defexamples s-lines
  (s-lines "abc\ndef\nghi") => '("abc" "def" "ghi"))

(defexamples s-join
  (s-join "+" '("abc" "def" "ghi")) => "abc+def+ghi"
  (s-join "\n" '("abc" "def" "ghi")) => "abc\ndef\nghi")

(defexamples s-repeat
  (s-repeat 10 " ") => "          "
  (concat (s-repeat 8 "Na") " Batman!") => "NaNaNaNaNaNaNaNa Batman!")

(defexamples s-truncate
  (s-truncate 6 "This is too long") => "Thi..."
  (s-truncate 16 "This is also too long") => "This is also ..."
  (s-truncate 16 "But this is not!") => "But this is not!")

(defexamples s-word-wrap
  (s-word-wrap 10 "This is too long") => "This is\ntoo long"
  (s-word-wrap 10 "This is way way too long") => "This is\nway way\ntoo long"
  (s-word-wrap 10 "It-wraps-words-but-does-not-break-them") => "It-wraps-words-but-does-not-break-them")

(defexamples s-left
  (s-left 3 "lib/file.js") => "lib"
  (s-left 3 "li") => "li")

(defexamples s-right
  (s-right 3 "lib/file.js") => ".js"
  (s-right 3 "li") => "li")

(defexamples s-suffix-p
  (s-suffix-p "-test.js" "penguin-test.js") => t
  (s-suffix-p "-test.js" "penguin-test.js" t) => t
  (s-suffix-p "" "some string") => t
  (s-suffix-p "sensitive" "CASE SENSITIVE" nil) => nil
  (s-suffix-p "sensitive" "CASE SENSITIVE" t) => t
  (s-suffix-p "verylong" "short") => nil)

(defexamples s-chop-suffix
  (s-chop-suffix "-test.js" "penguin-test.js") => "penguin"
  (s-chop-suffix "\n" "no newlines\n") => "no newlines"
  (s-chop-suffix "verylong" "short") => "short"
  (s-chop-suffix "\n" "some newlines\n\n") => "some newlines\n")

(defexamples s-chomp
  (s-chomp "no newlines\n") => "no newlines"
  (s-chomp "some newlines\n\n") => "some newlines\n")

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
  (s-contains-p "nope" "lib/file.js") => nil
  (s-contains-p "^a" "it's not ^a regexp") => t)

(defexamples s-replace
  (s-replace "file" "nope" "lib/file.js") => "lib/nope.js"
  (s-replace "^a" "\\1" "it's not ^a regexp") => "it's not \\1 regexp")

(defexamples s-split-words
  (s-split-words "under_score") => '("under" "score")
  (s-split-words "some-dashed-words") => '("some" "dashed" "words")
  (s-split-words "evenCamelCase") => '("even" "Camel" "Case")
  (s-split-words "!map (fn list)") => '("map" "fn" "list"))

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
