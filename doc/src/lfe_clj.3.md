% lfe_clj(3)
% Tim Dysinger, Duncan McGreggor, Eric Bailey
% 2015-2016


# NAME

clj - LFE Clojure interface library.


# SYNOPSIS

This module provides Clojure-inpired functions and macros for use in LFE.


# EXPORTS

## Function Composition

**(compose f g)**

Right to left function composition.

**(compose fs x)**

Compose a list of functions ``fs``, right to left, and apply the resulting
function to ``x``.

**(compose f g x)**

Equivalent to ``(funcall (compose f g) x)``.

**(compose fs)**

Compose a list of functions ``fs`` from right to left.

**(compose)**

Equivalent to ``#'identity/1``.


### Usage

The following examples assume ``#'1+/1`` is defined:

```lfe
> (defun 1+ (x) (+ x 1))
1+
```

```lfe
> (funcall (clj:compose #'math:sin/1 #'math:asin/1) 0.5)
0.49999999999999994
> (funcall (clj:compose (list #'1+/1 #'math:sin/1 #'math:asin/1) 0.5))
1.5
```

Or used in another function call:

```lfe
> (lists:filter (compose #'not/1 #'zero?/1)
                '(0 1 0 2 0 3 0 4))
(1 2 3 4)
```

The usage above is best when ``compose`` will be called by higher-order
functions like ``lists:foldl/3`` or ``lists:filter/2``, etc. However, one may
also call ``compose`` in the following manner, best suited for direct usage:

```lfe
> (compose #'math:sin/1 #'math:asin/1 0.5)
0.49999999999999994
> (compose (list #'1+/1 #'math:sin/1 #'math:asin/1) 0.5)
1.5
```


## Partial Application

**(partial f args)**

**(partial f arg-1)**

Partially apply ``f`` to a given argument ``arg-1`` or list of ``args``.


### Usage

```lfe
> (set f (partial #'+/2 1))
#Fun<clj.3.121115395>
> (funcall f 2)
3
> (set f (partial #'+/3 1))
#Fun<clj.3.121115395>
> (funcall f '(2 3))
6
> (set f (partial #'+/3 '(2 3)))
#Fun<clj.3.121115395>
> (funcall f 4)
9
> (set f (partial #'+/4 '(2 3)))
#Fun<clj.3.121115395>
> (funcall f '(4 5))
14
```


## Other Functions

**(identity x)**

Identity function.


## Threading Macros

Note: The original versions were copied from Tim Dysinger's lfesl repo here:

    https://github.com/lfex/lfesl/blob/master/include/thread.lfe

**(-> ...)**

Thread first.

Example usage, demonstrating ordering:

```lfe
> (set o '(#(a 1) #(b 2) #(c 3)))
(#(a 1) #(b 2) #(c 3))
> (-> o
>     (++ '(#(d 4)))
>     (++ '(#(e 5)))
>     (++ '(#(f 6))))
(#(a 1) #(b 2) #(c 3) #(d 4) #(e 5) #(f 6))
```

Note that the use of ``->`` in this example results in each successive value
being *appended* to the input list.

Another example showing how this works:

```lfe
> (lists:sublist
>   (lists:reverse
>     (lists:sort
>       (lists:merge
>         (string:tokens
>           (string:to_upper "a b c d e")
>           " ")
>         '("X" "F" "L"))))
>   2 3)
("L" "F" "E")
```

Can be rewritten as this:

```lfe
> (-> "a b c d e"
>     (string:to_upper)
>     (string:tokens " ")
>     (lists:merge '("X" "F" "L"))
>     (lists:sort)
>     (lists:reverse)
>     (lists:sublist 2 3))
("L" "F" "E")
```

**(->> ...)**

Thread last.

Example usage, demonstrating ordering:

```lfe
> (set o '(#(a 1) #(b 2) #(c 3)))
(#(a 1) #(b 2) #(c 3))
> (->> o
>      (++ '(#(d 4)))
>      (++ '(#(e 5)))
>      (++ '(#(f 6))))
(#(f 6) #(e 5) #(d 4) #(a 1) #(b 2) #(c 3))
```

Note that the use of ``->>`` in this example results in each successive value
being *prepended* to the input list.

Another example showing how this:

```lfe
> (lists:foldl #'+/2 0
>   (take 10
>     (lists:filter
>       (compose #'even?/1 #'round/1)
>       (lists:map
>         (lambda (x)
>           (math:pow x 2))
>         (seq 42)))))
1540.0
```

Can be rewritten as this:

```lfe
> (->> (seq 42)
>      (lists:map (lambda (x) (math:pow x 2)))
>      (lists:filter (compose #'even?/1 #'round/1))
>      (take 10)
>      (lists:foldl #'+/2 0))
1540.0
```
