(defsystem "abnf-match"
  :version "1.0.0"
  :license "BSD-2"
  :description "A DSL for writing parsers of grammars expressed in IETF ABNF."
  :author "Stephen Youts"
  :depends-on ("trivial-us-ascii")
  :components
  ((:static-file "LICENSE")
   (:static-file "README.md")
   (:module "src"
    :components ((:file "abnf-match")))))
