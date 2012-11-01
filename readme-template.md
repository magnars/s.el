# s.el [![Build Status](https://secure.travis-ci.org/magnars/s.el.png)](http://travis-ci.org/magnars/s.el)

The long lost Emacs string manipulation library.

## Installation

It's available on [marmalade](http://marmalade-repo.org/) and [Melpa](http://melpa.milkbox.net/):

    M-x package-install s

Or you can just dump `s.el` in your load path somewhere.

## Functions

[[ function-list ]]

## Documentation and examples

[[ function-docs ]]

## What's with the built-in wrappers?

Imagine looking through the function list and seeing `s-ends-with?`, but
`s-starts-with?` is nowhere to be found. Why? Well, because Emacs already has
`string-prefix-p`. Now you're starting out slightly confused, then have to go
somewhere else to dig for the command you were looking for.

The wrapping functions serve as both documentation for existing functions and
makes for a consistent API.

## Contributors

* [Arthur Andersen](https://github.com/leoc) contributed `s-match`

Thanks!

## Contribute

Yes, please do. There's a suite of tests, so remember to add tests for your
specific feature, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/s.el

**Looking for work?** Here are some features we would like:

 - `(s-distance s1 s2)` calculates Levenshtein distance between s1 and s2
 - `(s-shared-start s1 s2)` returns the longest prefix s1 and s2 have in common
 - `(s-shared-end s1 s2)` returns the longest suffix s1 and s2 have in common

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
