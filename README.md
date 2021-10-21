# ABNF Match

ABNF Match provides a domain specific language that facilitates the expedient
creation of pattern matchers that mirror the logic described in IETF RFCs.

The library contains a complete implementation of the operators described in [RFC
5234](https://datatracker.ietf.org/doc/html/rfc5234) (Augmented BNF for Syntax Specifications: ABNF). A few macros govern the
composition of these operators into the domain specific language.

A function defined by the DSL is called a 'rule', and operates on an octet
sequence. When called, a rule attempts to match the pattern dictated by its
internal logic against the sequence. Rules may be composed into more complex
rules.

The DSL is powerful enough to declare rules that completely and rigorously match
the syntax of entire RFCs. Writing syntax validators for things like URIs, HTTP
messages, and so forth is easy with this library.

It's also fairly quick, running in linear time, with no consing (dynamic memory
allocation). Macros and inlining are employed to negate function call overhead.

ABNF Match obeys the character encoding defined by the [Trivial US-ASCII](https://git.sr.ht/~pyramidion/trivial-us-ascii)
library.

## Table of Contents

1. [Installation](#installation)
2. [Tutorial](#tutorial)
3. [Reference](#reference)
4. [Performance](#performance)
5. [Links](#links)
6. [Patches](#patches)
7. [License](#license)

## Installation

You'll need to install [Trivial US-ASCII](https://git.sr.ht/~pyramidion/trivial-us-ascii) first.

ABNF Match requires [ASDF](https://common-lisp.net/project/asdf/), the
Common Lisp world's de facto standard build facility. Most Common Lisp
implementations ship with ASDF, so chances are you don't need to install it
yourself.

You'll need to [configure ASDF to find ABNF Match](https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html).

If you're in a hurry, and run a *nix system, just do this:

```bash
$ mkdir -p ~/.local/share/common-lisp/source

$ git clone https://git.sr.ht/~pyramidion/abnf-match \
  ~/.local/share/common-lisp/source/abnf-match
```

ASDF should find the package there and make it available to your Common Lisp
implementation. Subsequently, you will be able to `require` the package in
your REPL, and include it as a dependency to your own projects using ASDF.

At some point, I'll see about including it in [Quicklisp](https://www.quicklisp.org/beta/).

## Tutorial

Load the `abnf-match` package:

```lisp
CL-USER> (require :abnf-match)
NIL

CL-USER> (use-package :abnf-match)
T
```

Let's define a rule and see how this works.

```lisp
CL-USER> (defrule r-null
           (terminal trivial-us-ascii:+NUL+))
R-NULL

CL-USER> (r-null (make-array 10
                             :initial-element trivial-us-ascii:+NUL+
                             :element-type '(unsigned-byte 8))
                             0
                             10)
1

CL-USER> (r-null (make-array 10
                             :initial-element #x01
                             :element-type '(unsigned-byte 8))
                             0
                             10)
NIL
```

What goes on here? `r-null` is our new rule, which matches a single `terminal`:
`NUL`, as defined by US-ASCII.

When we call a rule, we provide it an array of octets, a lower index, and an
upper index. It then attempts to match accordingly, starting at the lower
index and continuing until it reaches the upper index.

A successful match returns the terminal index of the match. A failure returns
`nil`. Thus, rules can be chained together: the terminal index of one rule
becomes the lower index of the next.

We see that `r-null` successfully returns `1` after matching the first octet
of a `NUL` array. Likewise, it returns `nil` when it fails to match the
second array where every element is `1`.

Let's keep going.

```lisp
CL-USER> (defrule r-null-10x
           (concatenation r-null
                          r-null
                          r-null
                          r-null
                          r-null
                          r-null
                          r-null
                          r-null
                          r-null
                          r-null))
R-NULL-10X

CL-USER> (r-null-10x (make-array 10
                                 :initial-element trivial-us-ascii:+NUL+
                                 :element-type '(unsigned-byte 8))
                                 0
                                 10)
10
```

So, `concatenation` does what we expect. We can compose symbols naming rules
inside of other rules. Alternatively, we could have written:

```lisp
CL-USER> (defrule r-null-10x
           (concatenation (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)))
R-NULL-10X

CL-USER> (r-null-10x (make-array 10
                                 :initial-element trivial-us-ascii:+NUL+
                                 :element-type '(unsigned-byte 8))
                                 0
                                 10)
10
```

Or mixed and matched:

```lisp
CL-USER> (defrule r-null-10x
           (concatenation (terminal trivial-us-ascii:+NUL+)
                          r-null
                          (terminal trivial-us-ascii:+NUL+)
                          r-null
                          (terminal trivial-us-ascii:+NUL+)
                          r-null
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)
                          (terminal trivial-us-ascii:+NUL+)))
R-NULL-10X

CL-USER> (r-null-10x (make-array 10
                                 :initial-element trivial-us-ascii:+NUL+
                                 :element-type '(unsigned-byte 8))
                                 0
                                 10)
10
```

Writing long concatenations of terminal values is annoying, though. There's
better syntax for it:

```lisp
CL-USER> (use-package :trivial-us-ascii)
T

CL-USER> (defrule r-null-10x
           (terminals +NUL+ +NUL+ +NUL+ +NUL+ +NUL+
                      +NUL+ +NUL+ +NUL+ +NUL+ +NUL+))
R-NULL-10X

CL-USER> (r-null-10x (make-array 10
                                 :initial-element +NUL+
                                 :element-type '(unsigned-byte 8))
                                 0
                                 10)
10
```

We can clean it up even further.

```lisp
CL-USER> (defrule r-null-10x
           (specific-repetition 10 (terminal +NUL+)))
R-NULL-10X

CL-USER> (r-null-10x (make-array 10
                                 :initial-element +NUL+
                                 :element-type '(unsigned-byte 8))
                                 0
                                 10)
10

CL-USER> (defrule r-null-10x
           (specific-repetition 10 r-null))
R-NULL-10X

CL-USER> (r-null-10x (make-array 10
                                 :initial-element +NUL+
                                 :element-type '(unsigned-byte 8))
                                 0
                                 10)
10
```

Not bad. What about matching against arrays of variable length?

```lisp
CL-USER> (defrule r-null*
           (variable-repetition r-null))
R-NULL*

CL-USER> (r-null* (make-array 10
                              :initial-element +NUL+
                              :element-type '(unsigned-byte 8))
                              0
                              10)
10

CL-USER> (r-null* (make-array 5
                              :initial-element +NUL+
                              :element-type '(unsigned-byte 8))
                              0
                              5)
5
```

How about some binary characters - zero or one?

```lisp
CL-USER> (defrule r-binary
           (alternatives (terminal +#\0+)
                         (terminal +#\1+)))
R-BINARY

CL-USER> (defrule r-binary*
           (variable-repetition r-binary))

CL-USER> (r-binary (make-array 5
                               :initial-contents (list +#\0+ +#\1+
                                                       +#\0+ +#\1+
                                                       +#\0+)
                               :element-type '(unsigned-byte 8))
                               0
                               5)
1

CL-USER> (r-binary* (make-array 5
                                :initial-contents (list +#\0+ +#\1+
                                                        +#\0+ +#\1+
                                                        +#\0+)
                                :element-type '(unsigned-byte 8))
                                0
                                5)
5
```

Binary characters padded by `NUL`?

```lisp
CL-USER> (defrule r-binary*-padded
           (concatenation (optional-sequence (terminal +NUL+))
                          r-binary*
                          (optional-sequence (terminal +NUL+))))
R-BINARY*-PADDED

CL-USER> (r-binary*-padded (make-array 10
                                       :initial-contents (list +NUL+ +NUL+
                                                               +NUL+ +NUL+
                                                               +#\0+ +#\1+
                                                               +#\0+ +#\1+
                                                               +#\0+ +NUL+)
                                       :element-type '(unsigned-byte 8))
                                       0
                                       10)
10
```

As you can see, it's fairly easy to begin constructing more complex rules.

The syntax is more readable than, say, regular expressions - and it also
has a 1-1 relation with the ABNF used in IETF RFCs, making it easy and safe
to directly translate their rules and forms into Common Lisp.

For example:

```
https-URI = "https:" "//" authority path-abempty [ "?" query ] [ "#" fragment ]
```

```lisp
CL-USER> (defrule r-https-uri
           (concatenation (terminals +#\h+ +#\t+ +#\t+ +#\p+ +#\s+ +#\:+ +#\/+ +#\/+)
                          r-authority
                          r-path-abempty
                          (optional-sequence (terminal +#\?+)
                                             r-query)
                          (optional-sequence (terminal +#\#+)
                                             r-fragment)))
R-HTTPS-URI
```

With `r-authority`, `r-path-abempty`, `r-query`, and `r-fragment` defined
elsewhere.

## Reference

The core of the library's domain specific language is defined by a small group
of operators and pre-written rules.

The DSL may be extended with `defrule`.

Primitive operators:
* `terminal`
* `terminals`
* `concatenation`
* `alternatives`
* `value-range-alternatives`
* `sequence-group`
* `variable-repetition`
* `specific-repetition`
* `optional-sequence`

Additional documentation of these operators is available in [RFC 5234, Section 3](https://datatracker.ietf.org/doc/html/rfc5234#section-3).

Core rules:
* `r-alpha`
* `r-bit`
* `r-char`
* `r-cr`
* `r-crlf`
* `r-ctl`
* `r-digit`
* `r-dquote`
* `r-hexdig`
* `r-htab`
* `r-lf`
* `r-lwsp`
* `r-octet`
* `r-sp`
* `r-vchar`
* `r-wsp`

Additional documentation of the core rules is available in [RFC 5234, Appendix B](https://datatracker.ietf.org/doc/html/rfc5234#appendix-B).

### defrule

*Macro* **DEFRULE**

**Syntax:**

```
defrule rule-name &rest body => rule
```

**Arguments and Values:**

```
rule-name---a symbol naming the rule.

body---one or more (but typically only one) forms governing the rule's logic.

rule---a function bound to the toplevel encapsulating the rule.
```

**Description:**

Creates and binds a new rule function as per defun named by *rule-name*, with
internal logic declared in *body* according to the syntax of the ABNF Match
domain specific language.

The resultant *rule* has the following signature:

```
rule octets lower upper => matched-to

octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

The resultant *rule* may be included in the definitions of other rules.

### terminal

Within the *body* of a `defrule`, `terminal` is used as follows:

```lisp
(defrule rule-name
  (terminal value))
```

Where a `value` is always a single `(unsigned-byte 8)` literal.

`terminal` always results in a match of one octet, or `nil`.

### terminals

Within the *body* of a `defrule`, `terminals` is used as follows:

```lisp
(defrule rule-name
  (terminals value-0 value-1 ... value-n))
```

Where a `value` is always a single `(unsigned-byte 8)` literal.

`terminals` always results in a match of n octets, or `nil`.

### concatenation

Within the *body* of a `defrule`, `concatenation` is used as follows:

```lisp
(defrule rule-name
  (concatenation form-0 form-1 ... form-n))
```

Where a `form` is always either the invocation of another primitive or
a symbol naming another `rule`.

`concatenation` results in a match of variable length, depending on the results
of each component `form`. Alternatively, it may result in `nil`.

### alternatives

Within the *body* of a `defrule`, `alternatives` is used as follows:

```lisp
(defrule rule-name
  (alternatives form-0 form-1 ... form-n))
```

Where a `form` is always either the invocation of another primitive or
a symbol naming another `rule`.

`alternatives` results in a match of variable length, depending on the results
of the first successfully matched component `form`. Alternatively, it may
result in `nil`.

### value-range-alternatives

Within the *body* of a `defrule`, `value-range-alternatives` is used as follows:

```lisp
(defrule rule-name
  (value-range-alternatives minimum-value maximum-value))
```

Where an inclusive range of alternatives to match is bounded by `minimum-value`
and `maximum-value`, each of which are literals of type `(unsigned-byte 8)`.

`value-range-alternatives` always results in a match of one octet, or `nil`.

### sequence-group

Within the *body* of a `defrule`, `sequence-group` is used as follows:

```lisp
(defrule rule-name
  (sequence-group form-0 form-1 ... form-n))
```

Where a `form` is always either the invocation of another primitive or
a symbol naming another `rule`.

`sequence-group` results in a match of variable length, depending on the results
of each component `form`. Alternatively, it may result in `nil`.

As implemented, `sequence-group` is synonymous with `concatenation`.

### variable-repetition

Within the *body* of a `defrule`, `variable-repetition` is used as follows:

```lisp
(defrule rule-name
  (variable-repetition form &key minimum maximum))
```

Where `form` is always either the invocation of another primitive or
a symbol naming another `rule`.

`variable-repetition` results in a match of variable length, depending on
how many times it matches `form`, and what `form` tries to match. Alternatively,
it may result in `nil`.

The keyword arguments `minimum` and `maximum` establish an inclusive range of
matches. So, for example, if you must match 1-3 instances of `form`, you would
specify:

```lisp
(defrule rule-name
  (variable-repetition form &key 1 3))
```

The default value of `minimum` is `0`. The default value of `maximum` is
the number of octets remaining before the `upper` bound is reached.

### specific-repetition

Within the *body* of a `defrule`, `specific-repetition` is used as follows:

```lisp
(defrule rule-name
  (specific-repetition n form))
```

Where `form` is always either the invocation of another primitive or
a symbol naming another `rule`.

`specific-repetition` results in a match of variable length, depending on what
`form` tries to match. Alternatively, it may result in `nil`.

`n` determines the exact number of times `specific-repetition` attempts to match
`form`.

### optional-sequence

Within the *body* of a `defrule`, `optional-sequence` is used as follows:

```lisp
(defrule rule-name
  (optional-sequence &rest forms))
```

Where a `form` in `forms` is always either the invocation of another primitive
or a symbol naming another `rule`.

`optional-repetition` results in a match of variable length, depending on what
`forms` tries to match. Alternatively, it may result in `nil`.

Because `optional-sequence` is optional, `forms` does not need to match at all
for it to succeed. Likewise, `forms`, as a group, will never match more than
once.

### r-alpha

*Rule* **R-ALPHA**

**Syntax:**

```
r-alpha octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match an alphabetical character (A-Z, a-z) encoded with US-ASCII
to the octet at index *lower* in *octets*.

### r-bit

*Rule* **R-BIT**

**Syntax:**

```
r-bit octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match either the character `0` or the character `1` encoded with
US-ASCII to the octet at index *lower* in *octets*.

### r-char

*Rule* **R-CHAR**

**Syntax:**

```
r-char octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match any character encoded with US-ASCII to the octet at index
*lower* in *octets*.

### r-cr

*Rule* **R-CR**

**Syntax:**

```
r-cr octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match the carriage return character encoded with US-ASCII to the
octet at index *lower* in *octets*.

### r-crlf

*Rule* **R-CRLF**

**Syntax:**

```
r-crlf octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match the carriage return character, then the line feed character,
both encoded with US-ASCII, to the first two octets at index *lower* in *octets*.

### r-ctl

*Rule* **R-CTL**

**Syntax:**

```
r-ctl octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match any control character encoded with US-ASCII to the octet
at position *lower* in *octets*.

### r-digit

*Rule* **R-DIGIT**

**Syntax:**

```
r-digit octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match any digit character encoded with US-ASCII to the octet
at position *lower* in *octets*.

### r-dquote

*Rule* **R-DQUOTE**

**Syntax:**

```
r-dquote octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match the double quote character (") encoded with US-ASCII to the
octet at position *lower* in *octets*.

### r-hexdig

*Rule* **R-HEXDIG**

**Syntax:**

```
r-hexdig octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match hexadecimal digit character (0-9, A-F) encoded with US-ASCII
to the octet at position *lower* in *octets*.

### r-htab

*Rule* **R-HTAB**

**Syntax:**

```
r-htab octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match the tab character encoded with US-ASCII to the octet at
position *lower* in *octets*.

### r-lf

*Rule* **R-LF**

**Syntax:**

```
r-lf octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match the line feed character encoded with US-ASCII to the octet at
position *lower* in *octets*.

### r-lwsp

*Rule* **R-LWSP**

**Syntax:**

```
r-lwsp octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match contiguous, linear whitespace encoded with US-ASCII starting
at position *lower* in *octets*. This whitespace includes line breaks.

### r-octet

*Rule* **R-OCTET**

**Syntax:**

```
r-octet octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match the any octet to the octet at position *lower* in *octets*.
Will always match successfully, by definition.

### r-sp

*Rule* **R-SP**

**Syntax:**

```
r-sp octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match the space character encoded with US-ASCII to the octet at
position *lower* in *octets*.

### r-vchar

*Rule* **R-VCHAR**

**Syntax:**

```
r-vchar octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match any visible, printing character encoded with US-ASCII to the
octet at index *lower* in *octets*.

### r-wsp

*Rule* **R-WSP**

**Syntax:**

```
r-wsp octets lower upper => matched-to
```

**Arguments and Values:**

```
octets---an octet sequence of type (simple-array (unsigned-byte 8) (*))

lower---a lower index of type fixnum; the first position of octets to be matched

upper---an upper index of type fixnum; the last position of octets to be matched

matched-to---either the terminal index of a successful match, or nil if it fails
```

**Description:**

Attempts to match a whitespace character (space, tab) encoded with US-ASCII to
the octet at index *lower* in *octets*.

## Performance

The library is fairly quick. Here's a trivial example. More complex logic
can slow it down.

```lisp
CL-USER> (defparameter *alphabet*
                       (make-array 52
                                   :initial-contents (list trivial-us-ascii:+#\A+ trivial-us-ascii:+#\B+ trivial-us-ascii:+#\C+ trivial-us-ascii:+#\D+
                                                           trivial-us-ascii:+#\E+ trivial-us-ascii:+#\F+ trivial-us-ascii:+#\G+ trivial-us-ascii:+#\H+
                                                           trivial-us-ascii:+#\I+ trivial-us-ascii:+#\J+ trivial-us-ascii:+#\K+ trivial-us-ascii:+#\L+
                                                           trivial-us-ascii:+#\M+ trivial-us-ascii:+#\N+ trivial-us-ascii:+#\O+ trivial-us-ascii:+#\P+
                                                           trivial-us-ascii:+#\Q+ trivial-us-ascii:+#\R+ trivial-us-ascii:+#\S+ trivial-us-ascii:+#\T+
                                                           trivial-us-ascii:+#\U+ trivial-us-ascii:+#\V+ trivial-us-ascii:+#\W+ trivial-us-ascii:+#\X+
                                                           trivial-us-ascii:+#\Y+ trivial-us-ascii:+#\Z+
                                                           trivial-us-ascii:+#\a+ trivial-us-ascii:+#\b+ trivial-us-ascii:+#\c+ trivial-us-ascii:+#\d+
                                                           trivial-us-ascii:+#\e+ trivial-us-ascii:+#\f+ trivial-us-ascii:+#\g+ trivial-us-ascii:+#\h+
                                                           trivial-us-ascii:+#\i+ trivial-us-ascii:+#\j+ trivial-us-ascii:+#\k+ trivial-us-ascii:+#\l+
                                                           trivial-us-ascii:+#\m+ trivial-us-ascii:+#\n+ trivial-us-ascii:+#\o+ trivial-us-ascii:+#\p+
                                                           trivial-us-ascii:+#\q+ trivial-us-ascii:+#\r+ trivial-us-ascii:+#\s+ trivial-us-ascii:+#\t+
                                                           trivial-us-ascii:+#\u+ trivial-us-ascii:+#\v+ trivial-us-ascii:+#\w+ trivial-us-ascii:+#\x+
                                                           trivial-us-ascii:+#\y+ trivial-us-ascii:+#\z+)
                                   :element-type '(unsigned-byte 8)))
*ALPHABET*

CL-USER> (defrule r-alphabet
           (terminals trivial-us-ascii:+#\A+ trivial-us-ascii:+#\B+ trivial-us-ascii:+#\C+ trivial-us-ascii:+#\D+
                      trivial-us-ascii:+#\E+ trivial-us-ascii:+#\F+ trivial-us-ascii:+#\G+ trivial-us-ascii:+#\H+
                      trivial-us-ascii:+#\I+ trivial-us-ascii:+#\J+ trivial-us-ascii:+#\K+ trivial-us-ascii:+#\L+
                      trivial-us-ascii:+#\M+ trivial-us-ascii:+#\N+ trivial-us-ascii:+#\O+ trivial-us-ascii:+#\P+
                      trivial-us-ascii:+#\Q+ trivial-us-ascii:+#\R+ trivial-us-ascii:+#\S+ trivial-us-ascii:+#\T+
                      trivial-us-ascii:+#\U+ trivial-us-ascii:+#\V+ trivial-us-ascii:+#\W+ trivial-us-ascii:+#\X+
                      trivial-us-ascii:+#\Y+ trivial-us-ascii:+#\Z+
                      trivial-us-ascii:+#\a+ trivial-us-ascii:+#\b+ trivial-us-ascii:+#\c+ trivial-us-ascii:+#\d+
                      trivial-us-ascii:+#\e+ trivial-us-ascii:+#\f+ trivial-us-ascii:+#\g+ trivial-us-ascii:+#\h+
                      trivial-us-ascii:+#\i+ trivial-us-ascii:+#\j+ trivial-us-ascii:+#\k+ trivial-us-ascii:+#\l+
                      trivial-us-ascii:+#\m+ trivial-us-ascii:+#\n+ trivial-us-ascii:+#\o+ trivial-us-ascii:+#\p+
                      trivial-us-ascii:+#\q+ trivial-us-ascii:+#\r+ trivial-us-ascii:+#\s+ trivial-us-ascii:+#\t+
                      trivial-us-ascii:+#\u+ trivial-us-ascii:+#\v+ trivial-us-ascii:+#\w+ trivial-us-ascii:+#\x+
                      trivial-us-ascii:+#\y+ trivial-us-ascii:+#\z+))
R-ALPHABET

CL-USER> (time (dotimes (c 100000)
                 (r-alphabet *alphabet* 0 (length *alphabet*))))
Evaluation took:
  0.006 seconds of real time
  0.007758 seconds of total run time (0.007758 user, 0.000000 system)
  133.33% CPU
  27,935,820 processor cycles
  0 bytes consed

NIL
```

## Links

* [Repository](https://sr.ht/~pyramidion/abnf-match/)

## Patches

Patches are welcome.

## License

ABNF Match is licensed under the GNU Affero General Public License v3.

See LICENSE and AGPLv3.
