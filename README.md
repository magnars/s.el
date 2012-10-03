# s.el [![Build Status](https://secure.travis-ci.org/magnars/s.el.png)](http://travis-ci.org/magnars/s.el)

The long lost Emacs string manipulation library.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](http://melpa.milkbox.net/):

    M-x package-install s

Or you can just dump `s.el` in your load path somewhere.

## Functions

* [s-trim](#s-trim-s) `(s)`
* [s-trim-left](#s-trim-left-s) `(s)`
* [s-trim-right](#s-trim-right-s) `(s)`
* [s-collapse-whitespace](#s-collapse-whitespace-s) `(s)`
* [s-lines](#s-lines-s) `(s)`
* [s-join](#s-join-separator-strings) `(separator strings)`
* [s-concat](#s-concat-rest-strings) `(&rest strings)`
* [s-repeat](#s-repeat-num-s) `(num s)`
* [s-truncate](#s-truncate-len-s) `(len s)`
* [s-word-wrap](#s-word-wrap-len-s) `(len s)`
* [s-left](#s-left-len-s) `(len s)`
* [s-right](#s-right-len-s) `(len s)`
* [s-chop-suffix](#s-chop-suffix-suffix-s) `(suffix s)`
* [s-chomp](#s-chomp-s) `(s)`
* [s-ends-with-p](#s-ends-with-p-suffix-s) `(suffix s)`
* [s-starts-with-p](#s-starts-with-p-prefix-s) `(prefix s)`
* [s-contains-p](#s-contains-p-needle-s) `(needle s)`
* [s-replace](#s-replace-old-new-s) `(old new s)`
* [s-split-words](#s-split-words-s) `(s)`
* [s-lower-camel-case](#s-lower-camel-case-s) `(s)`
* [s-upper-camel-case](#s-upper-camel-case-s) `(s)`
* [s-snake-case](#s-snake-case-s) `(s)`
* [s-dashed-words](#s-dashed-words-s) `(s)`
* [s-capitalized-words](#s-capitalized-words-s) `(s)`

## Documentation and examples

### s-trim `(s)`

Remove whitespace at the beginning and end of `s`.

```cl
(s-trim "trim ") ;; => "trim"
(s-trim " this") ;; => "this"
(s-trim " only  trims beg and end  ") ;; => "only  trims beg and end"
```

### s-trim-left `(s)`

Remove whitespace at the beginning of `s`.

```cl
(s-trim-left "trim ") ;; => "trim "
(s-trim-left " this") ;; => "this"
```

### s-trim-right `(s)`

Remove whitespace at the end of `s`.

```cl
(s-trim-right "trim ") ;; => "trim"
(s-trim-right " this") ;; => " this"
```

### s-collapse-whitespace `(s)`

Convert all adjacent whitespace characters to a single space.

```cl
(s-collapse-whitespace "only   one space   please") ;; => "only one space please"
(s-collapse-whitespace "collapse \n all \t sorts of \r whitespace") ;; => "collapse all sorts of whitespace"
```

### s-lines `(s)`

Splits `s` into a list of strings on newline characters.

```cl
(s-lines "abc\ndef\nghi") ;; => '("abc" "def" "ghi")
```

### s-join `(separator strings)`

Join all the strings in `strings` with `separator` in between.

```cl
(s-join "+" '("abc" "def" "ghi")) ;; => "abc+def+ghi"
(s-join "\n" '("abc" "def" "ghi")) ;; => "abc\ndef\nghi"
```

### s-concat `(&rest strings)`

Join all the string arguments into one string.

```cl
(s-concat "abc" "def" "ghi") ;; => "abcdefghi"
```

### s-repeat `(num s)`

Make a string of `s` repeated `num` times.

```cl
(s-repeat 10 " ") ;; => "          "
(s-concat (s-repeat 8 "Na") " Batman!") ;; => "NaNaNaNaNaNaNaNa Batman!"
```

### s-truncate `(len s)`

If `s` is longer than `len`, cut it down and add ... at the end.

```cl
(s-truncate 6 "This is too long") ;; => "Thi..."
(s-truncate 16 "This is also too long") ;; => "This is also ..."
(s-truncate 16 "But this is not!") ;; => "But this is not!"
```

### s-word-wrap `(len s)`

If `s` is longer than `len`, wrap the words with newlines.

```cl
(s-word-wrap 10 "This is too long") ;; => "This is\ntoo long"
(s-word-wrap 10 "This is way way too long") ;; => "This is\nway way\ntoo long"
(s-word-wrap 10 "It-wraps-words-but-does-not-break-them") ;; => "It-wraps-words-but-does-not-break-them"
```

### s-left `(len s)`

Returns up to the `len` first chars of `s`.

```cl
(s-left 3 "lib/file.js") ;; => "lib"
(s-left 3 "li") ;; => "li"
```

### s-right `(len s)`

Returns up to the `len` last chars of `s`.

```cl
(s-right 3 "lib/file.js") ;; => ".js"
(s-right 3 "li") ;; => "li"
```

### s-chop-suffix `(suffix s)`

Remove `suffix` if it is at end of `s`.

```cl
(s-chop-suffix "-test.js" "penguin-test.js") ;; => "penguin"
(s-chop-suffix "\n" "no newlines\n") ;; => "no newlines"
(s-chop-suffix "\n" "some newlines\n\n") ;; => "some newlines\n"
```

### s-chomp `(s)`

Remove trailing newline from `s`.

```cl
(s-chomp "no newlines\n") ;; => "no newlines"
(s-chomp "some newlines\n\n") ;; => "some newlines\n"
```

### s-ends-with-p `(suffix s)`

Does `s` end in `suffix`?

```cl
(s-ends-with-p ".md" "readme.md") ;; => t
(s-ends-with-p ".md" "readme.txt") ;; => nil
(s-ends-with-p ".md" "md") ;; => nil
```

### s-starts-with-p `(prefix s)`

Does `s` start with `prefix`?

```cl
(s-starts-with-p "lib/" "lib/file.js") ;; => t
(s-starts-with-p "test/" "lib/file.js") ;; => nil
(s-starts-with-p "lib/" "lib") ;; => nil
```

### s-contains-p `(needle s)`

Does `s` contain `needle`?

```cl
(s-contains-p "file" "lib/file.js") ;; => t
(s-contains-p "nope" "lib/file.js") ;; => nil
(s-contains-p "^a" "it's not ^a regexp") ;; => t
```

### s-replace `(old new s)`

Replaces `old` with `new` in `s`.

```cl
(s-replace "file" "nope" "lib/file.js") ;; => "lib/nope.js"
(s-replace "^a" "\\1" "it's not ^a regexp") ;; => "it's not \\1 regexp"
```

### s-split-words `(s)`

Split `s` into list of words.

```cl
(s-split-words "under_score") ;; => '("under" "score")
(s-split-words "some-dashed-words") ;; => '("some" "dashed" "words")
(s-split-words "evenCamelCase") ;; => '("even" "Camel" "Case")
```

### s-lower-camel-case `(s)`

Convert `s` to lowerCamelCase.

```cl
(s-lower-camel-case "some words") ;; => "someWords"
(s-lower-camel-case "dashed-words") ;; => "dashedWords"
(s-lower-camel-case "under_scored_words") ;; => "underScoredWords"
```

### s-upper-camel-case `(s)`

Convert `s` to UpperCamelCase.

```cl
(s-upper-camel-case "some words") ;; => "SomeWords"
(s-upper-camel-case "dashed-words") ;; => "DashedWords"
(s-upper-camel-case "under_scored_words") ;; => "UnderScoredWords"
```

### s-snake-case `(s)`

Convert `s` to snake_case.

```cl
(s-snake-case "some words") ;; => "some_words"
(s-snake-case "dashed-words") ;; => "dashed_words"
(s-snake-case "camelCasedWords") ;; => "camel_cased_words"
```

### s-dashed-words `(s)`

Convert `s` to dashed-words.

```cl
(s-dashed-words "some words") ;; => "some-words"
(s-dashed-words "under_scored_words") ;; => "under-scored-words"
(s-dashed-words "camelCasedWords") ;; => "camel-cased-words"
```

### s-capitalized-words `(s)`

Convert `s` to Capitalized Words.

```cl
(s-capitalized-words "some words") ;; => "Some Words"
(s-capitalized-words "under_scored_words") ;; => "Under Scored Words"
(s-capitalized-words "camelCasedWords") ;; => "Camel Cased Words"
```


## Contribute

Yes, please do. There's a suite of tests, so remember to add tests for your
specific feature, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/s.el

## Development

Run the tests with

    ./run-tests.sh

Create the docs with

    ./create-docs.sh

I highly recommend that you install these as a pre-commit hook, so that
the tests are always running and the docs are always in sync:

    cp pre-commit.sh .git/hooks/pre-commit

Oh, and don't edit `README.md` directly, it is auto-generated.
Change `readme-template.md` or `examples-to-docs.el` instead.

## License

Copyright (C) 2012 Magnar Sveen

Authors: Magnar Sveen <magnars@gmail.com>
Keywords: strings

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
