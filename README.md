# s.el [![Build Status](https://secure.travis-ci.org/magnars/s.el.png)](http://travis-ci.org/magnars/s.el)

The long lost Emacs string manipulation library.

## Warning

This is so much a work in progress that you should definitely not be using it yet.

## Functions

* [s-trim](#s-trim-s) `(s)`
* [s-chop-suffix](#s-chop-suffix-suffix-s) `(suffix s)`
* [s-ends-with-p](#s-ends-with-p-suffix-s) `(suffix s)`
* [s-starts-with-p](#s-starts-with-p-suffix-s) `(suffix s)`
* [s-split-words](#s-split-words-s) `(s)`
* [s-lower-camel-case](#s-lower-camel-case-s) `(s)`
* [s-upper-camel-case](#s-upper-camel-case-s) `(s)`
* [s-snake-case](#s-snake-case-s) `(s)`
* [s-dashed-words](#s-dashed-words-s) `(s)`
* [s-capitalized-words](#s-capitalized-words-s) `(s)`

## Documentation and examples

### s-trim `(s)`

Remove whitespace at beginning and end of `s`.

```cl
(s-trim "trim ") ;; => "trim"
(s-trim " this") ;; => "this"
(s-trim " only  trims beg and end  ") ;; => "only  trims beg and end"
```

### s-chop-suffix `(suffix s)`

Remove `suffix` if it is at end of `s`.

```cl
(s-chop-suffix "-test.js" "penguin-test.js") ;; => "penguin"
(s-chop-suffix "!" "exclamation marks!") ;; => "exclamation marks"
(s-chop-suffix "!" "exclamation marks!!") ;; => "exclamation marks!"
```

### s-ends-with-p `(suffix s)`

Does `s` end in `suffix`?

```cl
(s-ends-with-p ".md" "readme.md") ;; => t
(s-ends-with-p ".md" "readme.txt") ;; => nil
(s-ends-with-p ".md" "md") ;; => nil
```

### s-starts-with-p `(suffix s)`

Does `s` start with `suffix`?

```cl
(s-starts-with-p "lib/" "lib/file.js") ;; => t
(s-starts-with-p "test/" "lib/file.js") ;; => nil
(s-starts-with-p "lib/" "lib") ;; => nil
```

### s-split-words `(s)`

Split `s` into list of words

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
