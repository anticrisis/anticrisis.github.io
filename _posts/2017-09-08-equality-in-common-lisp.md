---
title: Equality in Common Lisp
date: Fri 08 Sep 2017 11:01:31 AM HST
---

Determining the equality of two values in Common Lisp is not as
straightforward as it might seem, and there are a few parts of the
specification that seems inconsistent, especially to those coming
from other high level languages.

## Overview ##

Common Lisp provides a long list of equality functions to choose
from, depending on what you want to accomplish:

``` common-lisp
(=            a b)
(eq           a b)
(eql          a b)
(equal        a b)
(equalp       a b)
(string=      a b)
(string-equal a b)
(char=        a b)
(char-equal   a b)
```

I'm not going to go through all of them, but I do want to highlight
gotchas I've run into.

## Case sensitivity ##

You may have noticed that Common Lisp likes the SHIFT key. For
reasons certainly not lost to history, symbols in Common Lisp are by
default NOT case sensitive. There is some internal chickanery
available to make the Common Lisp reader case sensitive, but I
gather from my own research that this is considered a bit risky. So
let's not go there.

So for example, with symbols:

``` common-lisp
CL-USER> (eq 'a 'A)
T
CL-USER> (eql 'a 'A)
T
CL-USER> (equal 'a 'A)
T
CL-USER> (equalp 'a 'A)
T
CL-USER> 
```

This is fine, because if you have an itchy pinky, Common Lisp will
forgive you if you upper-case your symbols, function calls, etc.

## Equality tower? ##

Does it seem like these particular equality functions are designed
to get more and more general?

``` common-lisp
eq < eql < equal < equalp
```

`eq` tests for identity, as in the same pointer value. `eql` checks
if the values pointed to by different pointers are equal, like two
variables with the same number assigned to them. `equal` extends the
comparisons to recursively check lists and arrays. Finally, `equalp`
extends the comparison to structures and hash tables.

So you might be forgiven to assume (like I did) that `equalp` is a
good general choice if you're not overly concerned about performance
and don't mind letting the runtime pick the most type-appropriate
equality test. That's where the problem begins.

## Strings ##

So, back to Common Lisp's love of the SHIFT key. It turns out this
love extends to strings, as well:

``` common-lisp
CL-USER> (equalp "hello" "HELLO")
T
```

That's the first gotcha. `equalp` seems good for a general-purpose
equality test, especially because it understands lists, arrays and
structures, but it apparently doesn't understand the difference
between whispering and shouting.

Instead, you need one of these two functions if you want to compare
strings the way most people do:

``` common-lisp
CL-USER> (equal "hello" "HELLO")
NIL
CL-USER> (string= "hello" "HELLO")
NIL
```

Watch out for the misleadingly named `string-equal`, however. It
sounds like it should be a string version of `equal`, but it gives a
different result:

``` common-lisp
CL-USER> (equal "hello" "HELLO")
NIL
CL-USER> (string-equal "hello" "HELLO")
T
```

I'm going to let the experts explain that naming choice, because I'm
stumped.

### Unicode ###

I haven't tested the string equality functions on unicode strings
yet. The luxury of being an English-native speaker means I put
unicode off until later. But who knows if `string=`, `string-equal`,
and `equal` will work consistently with Unicode strings?

## Structures ##

Even if `equalp` isn't the right choice for strings, don't write it
off just yet. It's very useful for user-defined structures:

``` common-lisp
CL-USER> (defstruct foo-struct x)
FOO-STRUCT
CL-USER> (equalp (make-foo-struct :x 1) (make-foo-struct :x 1))
T
CL-USER> (equal (make-foo-struct :x 1) (make-foo-struct :x 1))
NIL
```

Two structures with different identities, but the same slot values,
are considered equal by `equalp`. This is generally what I want when
I define structures to hold data.

But hold on, if the data in the structures are strings, we're in a
bit of a pickle.

``` common-lisp
CL-USER> (equalp (make-foo-struct :x "hello") (make-foo-struct :x "HELLO"))
T
CL-USER> (equal (make-foo-struct :x "hello") (make-foo-struct :x "HELLO"))
NIL
CL-USER> (equal (make-foo-struct :x "hello") (make-foo-struct :x "hello"))
NIL
```

`equalp` of course decides that the two strings in the structure are
equal even if they're different case. But our case-sensitive friend
`equal` fails us hard in the third test, because it's not even
looking at the strings. (It's not allowed to look into structures,
which is why we wanted to use `equalp`.

Unfortunately, the only way around this is to define a user function
that walks through structure slots, checks the type of the value
stored there, and uses the appropriate equality function.

## Hash tables ##

With hash tables, `equalp` is your friend. Anything less doesn't
have permission to look at keys and values. So for simple hash
tables that don't use string keys or values, try this:

``` common-lisp
CL-USER> (defvar a (make-hash-table :test 'equalp))
A
CL-USER> (defvar b (make-hash-table :test 'equalp))
B
CL-USER> (setf (gethash :a a) 1)
1
CL-USER> (setf (gethash :a b) 1)
1
CL-USER> (equalp a b)
T
CL-USER> (equal a b)
NIL
```

With string values:

``` common-lisp
CL-USER> (setf (gethash :b a) "hello")
"hello"
CL-USER> (setf (gethash :b b) "hello")
"hello"
CL-USER> (equalp a b)
T
```

But watch out for `equalp`'s case-blindness:

``` common-lisp
CL-USER> (setf (gethash :c a) "hello")
"hello"
CL-USER> (setf (gethash :c b) "HELLO")
"HELLO"
CL-USER> (equalp a b)
T
```

And this case-blindness applies to keys as well, when the hash table
is set to use `equalp` as its test function (when we created it):

``` common-lisp
CL-USER> (setf (gethash "hello" a) "world")
"world"
CL-USER> (setf (gethash "HELLO" a) "WORLD")
"WORLD"
CL-USER> (gethash "hello" a)
"WORLD"
T
CL-USER> 
```

For completeness, two identical hash tables with different test
functions are not considered equal:

``` common-lisp
CL-USER> (equalp (make-hash-table :test 'eql) (make-hash-table :test 'equalp))
NIL
```

## Classes ##

CLOS is amazing. Unfortunately, they forgot to introduce it to
`equalp`:

``` common-lisp
(defclass foo-class () ((x :initarg :x)))
#<STANDARD-CLASS COMMON-LISP-USER::FOO-CLASS>
CL-USER> (equalp (make-instance 'foo-class :x 1) (make-instance 'foo-class :x 1))
NIL
CL-USER> (equal (make-instance 'foo-class :x 1) (make-instance 'foo-class :x 1))
NIL
```

This `foo-class` is basically the same data layout as the
`foo-struct` I showed [above](#structures),
but `equalp` fails even in the simplest non-string case. That's because
`equalp` is not allowed to walk into a class instance. You might say
it's in detention.

## Now what? ##

So, the moral of the story is you can't take equality for granted.
Common Lisp provides nearly a dozen equality functions for different
purposes, with some unfortunate inconsistencies in naming. In my own
code, I define a generic `equal?` function and specialize it for
various structures and classes. I default to using `equalp` unless I
know or suspect strings might appear, in which case I use `equal`.

There's also something called
[CDR-8](https://common-lisp.net/project/cdr/document/8/index.html)
that aims to specify higher-level generic equality and comparison,
but there are some issues with the spec and no fully-functional
implementations available as of this date.

Here's hoping and working toward a more equal world for all of us.

As always comments are invited and appreciated, and are on
[reddit](https://www.reddit.com/r/lisp/comments/6yxzga/equality_in_common_lisp/).
