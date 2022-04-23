# ABNF Match

## Table of Contents

1. [Overview](#overview)
2. [Example Use Case](#example-use-case)
3. [Installation](#installation)
4. [Reference](#reference)
5. [Performance](#performance)
6. [Links](#links)
7. [Patches](#patches)
8. [License](#license)

## Overview

A domain specific language for writing parsers of grammars expressed in IETF
ABNF. It contains a complete implementation of [RFC 5234](https://datatracker.ietf.org/doc/html/rfc5234) (Augmented BNF for Syntax Specifications: ABNF).

Instead of writing bespoke, informal, error-prone parsers for various things
defined in RFCs (email, uri, etc.) you may use this library to do so easily and
rigorously.

The pattern-matching engine at the heart of this library also runs in linear
time and does not cons (i.e. perform dynamic memory allocation), so it's fast.

## Example Use Case

Great. What does any of that mean? Well, it's easier to show than tell, in this
case. We'll write an email address validator.

"Just use a regex," someone might say. That someone now has two problems.
Validating email addresses is deceptively hard; all of the following are valid
email addresses according to internet standards:

```
simple@example.com
very.common@example.com
disposable.style.email.with+symbol@example.com
other.email-with-hyphen@example.com
fully-qualified-domain@example.com
user.name+tag+sorting@example.com
x@example.com
example-indeed@strange-example.com
test/test@test.com
admin@mailserver1
example@s.example
" "@example.org
"john..doe"@example.org
mailhost!username@example.org
"very.(),:;<>[]\".VERY.\"very@\\ \"very\".unusual"@strange.example.com
user%example.com@example.org
user-@example.org
postmaster@[123.123.123.123]
postmaster@[IPv6:2001:0db8:85a3:0000:0000:8a2e:0370:7334]
```

So, regex-based validation. I would prefer not to.

It turns out that the grammar for an email address is formally specified in an
internet standard called [RFC 5321](https://www.rfc-editor.org/rfc/rfc5321.html) (Simple Mail Transfer Protocol).

The internet standards published by the IETF in these RFCs all rely on the same
language to express the grammars of all sorts of things, such as email
addresses, URIs, and even the entire HTTP protocol. That language is called
Augmented Backus-Naur Form, or ABNF.

ANBF rigorously expresses these grammars. If you can write them in ABNF, you
have captured the totality of all the things they might match, with no weird
edge cases.

That's what this library lets you do: express grammars in a Lispy version of
ABNF. Then, it automatically constructs relevant pattern matchers, which makes
writing validators and parsers simple.

So we can look at ABNF like this:

```
Mailbox        = Local-part "@" ( Domain / address-literal )
```

Quickly rewrite it as this:

```lisp
(defrule r-mailbox
    (concatenation r-local-part
                   (terminal +#\@+)
                   (alternatives r-domain
                                 r-address-literal)))
```

To do things like this:

```lisp
CL-USER> (r-mailbox (trivial-us-ascii:ascii-string-code
                      '(simple-array (unsigned-byte 8) (*))
                      "simple@example.com")
                    0
                    (length "simple@example.com"))
18
```

Or, for an invalid address:

```lisp
CL-USER> (r-mailbox (trivial-us-ascii:ascii-string-code
                      '(simple-array (unsigned-byte 8) (*))
                      "Abc.example.com")
                    0
                    (length "Abc.example.com"))
NIL
```

There are a few missing pieces. We'd need to rewrite all of the ABNF that
defines an email address, and not just `mailbox`. But that's the gist of it.
A complete implementation of an email address parser using this library
can be found [here](https://git.sr.ht/~pyramidion/email-parse).

## Installation

ABNF Match is available on [Ultralisp](https://ultralisp.org/) and easy to
install using [Quicklisp](https://www.quicklisp.org/beta/).

Add the Ultralisp repository:

```lisp
CL-USER> (ql-dist:install-dist "http://dist.ultralisp.org/")
```

Install ABNF Match:

```lisp
CL-USER> (ql:quickload :abnf-match)
```

## Reference

The library's domain specific language is defined by a small group of operators
and pre-written rules.

The DSL may be extended with [`defrule`](#defrule).

Primitive operators:
* [`terminal`](#terminal)
* [`terminals`](#terminals)
* [`concatenation`](#concatenation)
* [`alternatives`](#alternatives)
* [`value-range-alternatives`](#value-range-alternatives)
* [`sequence-group`](#sequence-group)
* [`variable-repetition`](#variable-repetition)
* [`specific-repetition`](#specific-repetition)
* [`optional-sequence`](#optional-sequence)

Additional documentation of these operators is available in [RFC 5234, Section 3](https://datatracker.ietf.org/doc/html/rfc5234#section-3).

Core rules:
* [`r-alpha`](#r-alpha)
* [`r-bit`](#r-bit)
* [`r-char`](#r-char)
* [`r-cr`](#r-cr)
* [`r-crlf`](#r-crlf)
* [`r-ctl`](#r-ctl)
* [`r-digit`](#r-digit)
* [`r-dquote`](#r-dquote)
* [`r-hexdig`](#r-hexdig)
* [`r-htab`](#r-htab)
* [`r-lf`](#r-lf)
* [`r-lwsp`](#r-lwsp)
* [`r-octet`](#r-octet)
* [`r-sp`](#r-sp)
* [`r-vchar`](#r-vchar)
* [`r-wsp`](#r-wsp)

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

The library is fairly quick, though more complex logic can slow it down.

```lisp
CL-USER> (require :abnf-match) (use-package :abnf-match) (use-package :trivial-us-ascii)
T

CL-USER> (defparameter *alphabet*
                       (make-array 52
                                   :initial-contents (list +#\A+ +#\B+ +#\C+ +#\D+
                                                           +#\E+ +#\F+ +#\G+ +#\H+
                                                           +#\I+ +#\J+ +#\K+ +#\L+
                                                           +#\M+ +#\N+ +#\O+ +#\P+
                                                           +#\Q+ +#\R+ +#\S+ +#\T+
                                                           +#\U+ +#\V+ +#\W+ +#\X+
                                                           +#\Y+ +#\Z+
                                                           +#\a+ +#\b+ +#\c+ +#\d+
                                                           +#\e+ +#\f+ +#\g+ +#\h+
                                                           +#\i+ +#\j+ +#\k+ +#\l+
                                                           +#\m+ +#\n+ +#\o+ +#\p+
                                                           +#\q+ +#\r+ +#\s+ +#\t+
                                                           +#\u+ +#\v+ +#\w+ +#\x+
                                                           +#\y+ +#\z+)
                                   :element-type '(unsigned-byte 8)))
*ALPHABET*

CL-USER> (defrule r-alphabet
             (terminals +#\A+ +#\B+ +#\C+ +#\D+
                        +#\E+ +#\F+ +#\G+ +#\H+
                        +#\I+ +#\J+ +#\K+ +#\L+
                        +#\M+ +#\N+ +#\O+ +#\P+
                        +#\Q+ +#\R+ +#\S+ +#\T+
                        +#\U+ +#\V+ +#\W+ +#\X+
                        +#\Y+ +#\Z+
                        +#\a+ +#\b+ +#\c+ +#\d+
                        +#\e+ +#\f+ +#\g+ +#\h+
                        +#\i+ +#\j+ +#\k+ +#\l+
                        +#\m+ +#\n+ +#\o+ +#\p+
                        +#\q+ +#\r+ +#\s+ +#\t+
                        +#\u+ +#\v+ +#\w+ +#\x+
                        +#\y+ +#\z+))
R-ALPHABET

CL-USER> (time (dotimes (c 100000)
                 (r-alphabet *alphabet* 0 (length *alphabet*))))
Evaluation took:
  0.004 seconds of real time
  0.004721 seconds of total run time (0.004628 user, 0.000093 system)
  125.00% CPU
  16,047,490 processor cycles
  0 bytes consed

NIL
```

## Links

* [Repository](https://sr.ht/~pyramidion/abnf-match/)

## Patches

Patches are welcome.

## License

ABNF Match is licensed under the two-clause BSD license.

See LICENSE.
