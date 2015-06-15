Jumpotron
=========

A layer between you and the web. Jumpotron was created to extend [DuckDuckGo](https://duckduckgo.com/)'s concept of !bangs. It works best when used as your browser's search engine, dropping through to another search engine if there's nothing for Jumpotron to do.

Jumpotron currently contains a way of redirecting you to an address based on a keyword, like DuckDuckGo's !bangs, but it's possible to extend it so that a keyword could trigger a more complicated action. An example of this can be found in `bookmarks.lisp`, which implements (very) basic bookmarking functionality.

Quickstart
----------

1. Run the following code in a REPL:

    ```lisp
    (jumpotron:define-redirect "!gh" "https://github.com/search?&q=~@{~A~^+~}")
    (hunchentoot:start (jumpotron:make-jumpotron))
    ```

2. Now open your browser and go to `http://localhost:5000/jump?q=!gh jumpotron`

Installation
------------

1. Make sure you have [Quicklisp](http://www.quicklisp.org/)
2. Clone this repository somewhere ASDF can find it
3. `(ql:quickload :jumpotron)`

Dependencies
------------

- [Hunchentoot](http://weitz.de/hunchentoot/) (BSD)
- [Alexandria](http://common-lisp.net/project/alexandria/) (Public Domain)
- [YASON](http://common-lisp.net/project/yason/) (BSD)
- [CL-PPCRE](http://weitz.de/cl-ppcre/) (BSD)


Set as browser search engine
----------------------------

You can use [this opensearch generator](http://customsearchprovider.appspot.com/) to add your personal Jumpotron to your browser as a search engine.

The jump protocol
-----------------

### JUMP (class)

Superclass for different types of jumps. It contains one slot:

- `EXCLUDE-TRIGGER-P`. Whether or not the query to pass to this jump should be stripped of the triggering word.
  - initarg `:EXCLUDE-TRIGGER-P`
  - reader `EXCLUDE-TRIGGER-P`
  - default `T`

### JUMP (generic function)

```lisp
(defgeneric jump (jump query-parts))
```

Will be called to do something with a query. `JUMP` will be an instance of JUMP, and `QUERY-PARTS` will be a list of words in the query. If `(EXCLUDE-TRIGGER-P JUMP)` is true this list won't contain the word that triggered this jump to be used, if its false it will.

### SUGGEST (generic function)

```lisp
(defgeneric suggest (jump query-parts))
```

Will be called to give suggestions for a query. The arguments are the same as for `JUMP`. A method on this generic function should return a list of strings which will be returned to the client as suggestions. A default method is implemented that returns a list of all the trigger words defined.


Utilities for jumps
-------------------

### REDIRECT (function)

```lisp
(redirect url)
```

Redirect the current request to `URL`.

### URL-ENCODE (function)

```lisp
(url-encode string)
```

URL-encodes `STRING`.


The rest of the API
-------------------

### MAKE-JUMPOTRON (function)

```lisp
(make-jumpotron &optional (port 5000))
```

Creates a Jumpotron acceptor. You can start it using `(hunchentoot:start (make-jumpotron))`

### ADD-JUMP (function)

```lisp
(add-jump trigger jump)
```

Add `JUMP` to the global hash table of jumps under the key `TRIGGER`. If `TRIGGER` is `NIL`, `JUMP` will be used when no other jump is triggered.

### REDIRECTING-JUMP (class)

This is a subclass of `JUMP` that adds one slot, `TARGET`. This slot contains a string that will be passed as the second argument to FORMAT, which can then consume the words in the query.

### DEFINE-REDIRECT (function)

```lisp
(defjump trigger format-string &optional (exclude-trigger-p t))
```

Defines a new redirecting jump. If a request comes in where a word in the query is `EQUAL` to `PREFIX`, the user will be redirected to the result of calling `FORMAT` with `FORMAT-STRING` and the words in the query. If `EXCLUDE-TRIGGER-P` is `NIL` those words will also still contain `TRIGGER`.

This is equivalent to, and actually does, the following:

```lisp
(add-jump trigger
            (make-instance 'redirecting-jump
                           :target format-string
                           :exclude-trigger-p exclude-trigger-p))"
```

As an example, let's look at the quickstart:

```lisp
(jumpotron:define-redirect "!gh" "https://github.com/search?&q=~@{~A~^+~}")
(jumpotron:start)
```

If you then go to `localhost:5000/jump?q=!gh jumpotron`, the `!gh` is `EQUAL` to the first word in the query, so you'll be redirected to the result of...

```lisp
(format nil "https://github.com/search?&q=~@{~A~^+~}" "jumpotron")
```

...which is `https://github.com/search?&q=jumpotron`.

If you visit `localhost:5000/jump?q=jumpotrong !gh` instead, the result would be the same.

If you specify `NIL` as the `TRIGGER` argument this jump will then be the default jump. You could for example set it to `https://duckduckgo.com/?q=~@{~A~^+~}`, in which case every query without a trigger match would be passed to DuckDuckGo. (You could practically add your own !bangs to DuckDuckGo that way).

If you give `NIL` as a third argument to `DEFINE-REDIRECT`, like this:

```lisp
(jumpotron:define-redirect "!gh" "https://github.com/search?&q=~@{~A~^+~}" nil)
```

...the "!gh" won't be removed from the query, so you'll be looking for "jumpotron !gh" or "!gh jumpotron" on Github.


### Basic bookmarks

(Very) basic bookmarks are implemented in the package `JUMPOTRON/BOOKMARKS`. You can use them like this:

```lisp
(jumpotron:add-jump "!bma" (make-instance 'jumpotron/bookmarks:bookmarking-jump))
(jumpotron:add-jump "!bm" (make-instance 'jumpotron/bookmarks:bookmark-jump))
```

If you then query `!bma jumpotron https://github.com/jorams/jumpotron` a bookmark will be added under the name `jumpotron`. You can then go to the bookmarked page by querying `!bm jumpotron`.

The `BOOKMARK-JUMP` implements bookmark names as suggestions.


License
-------

    Copyright (c) 2015 Joram Schrijver

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
