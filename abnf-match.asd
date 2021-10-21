(defsystem "abnf-match"
  :version "1.0.0"
  :license "GNU Affero General Public License v3"
  :description "A library defining a DSL that matches IETF ABNF notation against octet sequences."
  :author "Stephen Youts"
  :long-description
  "ABNF Match provides a domain specific language that facilitates the expedient
creation of pattern matchers that mirror the logic described in IETF RFCs.

The library contains a complete implementation of the operators described in RFC
5234 (Augmented BNF for Syntax Specifications: ABNF). A few macros govern the
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
"
  :depends-on ("trivial-us-ascii")
  :components
  ((:static-file "LICENSE")
   (:static-file "AGPLv3")
   (:static-file "README.md")
   (:module "src"
    :components ((:file "abnf-match")))))
