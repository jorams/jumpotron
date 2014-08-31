Jumpotron
=========

Overly simplistic redirect server.

Quickstart
----------

1. Run the following code in a REPL:

    ```commonlisp
    (jumpotron:defjump "gh" "https://github.com/search?&q=~@{~A~^+~}")
    (jumpotron:start)
    ```

2. Now open your browser and go to `http://localhost:5000/jump?q=gh jumpotron`

   (You can also go to `http://localhost:5000/gh jumpotron`)
3. ????

Installation
------------

1. Make sure you have [Quicklisp](http://www.quicklisp.org/)
2. Clone this repository somewhere ASDF can find it
3. `(ql:quickload :jumpotron)`

API
---

### DEFJUMP

```commonlisp
(defjump prefix format-string)
```

Defines a new jump. If a request comes in where the first word in the query is `EQUAL` to `PREFIX`, the user will be redirected to the result of calling `FORMAT` with `FORMAT-STRING` and the rest of the words in the query.

All jumps are stored in a global hash table.

As an example, let's look at the quickstart:

```commonlisp
(jumpotron:defjump "gh" "https://github.com/search?&q=~@{~A~^+~}")
(jumpotron:start)
```

If you then go to `localhost:5000/jump?q=gh jumpotron`, the `gh` is `EQUAL` to the first word in the query, so the user will be redirected to the result of...

```commonlisp
(format nil "https://github.com/search?&q=~@{~A~^+~}" "jumpotron")
```

...which is `https://github.com/search?&q=jumpotron`.

### START

```commonlisp
(start &optional (port 5000))
```

Starts Jumpotron on `PORT`.

### STOP

```commonlisp
(stop)
```

Stops a running Jumpotron.

License
-------

    Copyright (c) 2014 Joram Schrijver

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
