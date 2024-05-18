(in-package :cl-user)
(defpackage abnf-match
  (:use
   :cl
   :trivial-us-ascii)
  (:export
   :matched
   :defrule
   :letrules

   :terminal
   :terminals
   :concatenation
   :alternatives
   :value-range-alternatives
   :sequence-group
   :variable-repetition
   :specific-repetition
   :optional-sequence

   :capture

   :r-alpha
   :r-bit
   :r-char
   :r-cr
   :r-crlf
   :r-ctl
   :r-digit
   :r-dquote
   :r-hexdig
   :r-htab
   :r-lf
   :r-lwsp
   :r-octet
   :r-sp
   :r-vchar
   :r-wsp))
(in-package :abnf-match)

;; ------------------------------------------------------------------------------
;;
;; RFC 5234
;; Augmented BNF for Syntax Specifications: ABNF
;;
;; This is a complete implementation of the specification.
;;
;; ------------------------------------------------------------------------------

(deftype matched () '(or fixnum boolean))

;; ------------------------------------------------------------------------------

(defmacro thread-params (form)
  (let ((octets (intern (symbol-name 'octets)))
        (lower (intern (symbol-name 'lower)))
        (upper (intern (symbol-name 'upper)))
        (name (car form))
        (body (cdr form)))
    `(,name ,octets ,lower ,upper ,@body)))

(defmacro wrap-form (form)
  (let ((octets (intern (symbol-name 'octets)))
        (lower (intern (symbol-name 'lower)))
        (upper (intern (symbol-name 'upper))))
    (labels ((wrap-form (form)
               (when form
                 (cond ((and (symbolp form)
                             (not (keywordp form))
                             (not (boundp form)))
                        `(funcall #',form ,octets ,lower ,upper))
                       ((listp form)
                        (let* ((name (car form))
                               (body (cdr form)))
                          (if (equalp name 'capture)
                              (let ((capture-lower (car body))
                                    (capture-upper (cadr body))
                                    (rule (cddr body)))
                                (macroexpand-1 `(thread-params ,(nconc (list name capture-lower capture-upper)
                                                                       (mapcar #'wrap-form rule)))))
                              (macroexpand-1 `(thread-params ,(nconc (list name) (mapcar #'wrap-form body)))))))
                       (t form)))))
      (wrap-form form))))

(defmacro defrule (name &rest body)
  (let* ((octets (intern (symbol-name 'octets)))
         (lower (intern (symbol-name 'lower)))
         (upper (intern (symbol-name 'upper)))
         (lambda-list (list octets lower upper)))
    `(defun ,name ,lambda-list
       (declare (optimize (speed 3) (debug 0) (safety 0)))
       (declare (type (simple-array (unsigned-byte 8) (*)) ,octets))
       (declare (type fixnum ,lower))
       (declare (type fixnum ,upper))
       ,@(mapcar (lambda (form)
                   (macroexpand-1 `(wrap-form ,form)))
                 body))))

(defmacro letrules-rule (rule-def)
  (let* ((rule-name (car rule-def))
         (rule-body (cdr rule-def))
         (octets (intern (symbol-name 'octets)))
         (lower (intern (symbol-name 'lower)))
         (upper (intern (symbol-name 'upper)))
         (rule-lambda-list (list octets lower upper)))
    `(,rule-name ,rule-lambda-list
                 (declare (optimize (speed 3) (debug 0) (safety 0)))
                 (declare (type (simple-array (unsigned-byte 8) (*)) ,octets))
                 (declare (type fixnum ,lower))
                 (declare (type fixnum ,upper))
                 ,@(mapcar (lambda (form)
                             (macroexpand-1 `(wrap-form ,form)))
                           rule-body))))

(defmacro letrules (rule-defs &rest body)
  `(labels ,(mapcar (lambda (rule-def)
                      (macroexpand-1 `(letrules-rule ,rule-def)))
             rule-defs)
     ,@body))

;; ------------------------------------------------------------------------------

(declaim (type matched +match-failure+))
(declaim (inline terminal))
(declaim (inline value-range-alternatives))

(defconstant +match-failure+ nil)

(defun terminal (octets lower upper terminal)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (declare (type (unsigned-byte 8) terminal))
  (when (and (< lower upper)
             (= (aref octets lower) terminal))
    1))

(defmacro terminals (octets lower upper &rest terminals)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type symbol octets))
  (declare (type symbol lower))
  (declare (type symbol upper))
  (declare (type list terminals))
  `(concatenation ,octets
                  ,lower
                  ,upper
                  ,@(mapcar (lambda (terminal)
                              `(terminal ,octets ,lower ,upper ,terminal))
                            terminals)))

(defmacro concatenation (octets lower upper &rest rules)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type symbol octets))
  (declare (type symbol lower))
  (declare (type symbol upper))
  (declare (type list rules))
  (let ((n (intern (symbol-name 'n)))
        (n-rule (intern (symbol-name 'n-rule)))
        (rule-block (intern (symbol-name 'rule-block))))
    `(let ((,lower ,lower)
           (,n-rule 0)
           (,n 0))
       (declare (type (simple-array (unsigned-byte 8) (*)) ,octets))
       (declare (type fixnum ,lower))
       (declare (type fixnum ,upper))
       (declare (type matched ,n-rule))
       (declare (type matched ,n))
       (if (block ,rule-block
             ,@ (reduce #'nconc (mapcar (lambda (rule)
                                          `((when (not (setf ,n-rule ,rule)) (return-from ,rule-block))
                                            (incf ,lower ,n-rule)
                                            (incf ,n ,n-rule)))
                                        rules))
             t)
           ,n
           ,+match-failure+))))

(defmacro alternatives (octets lower upper &rest rules)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type symbol octets))
  (declare (type symbol lower))
  (declare (type symbol upper))
  (declare (type list rules))
  (declare (ignorable octets))
  (declare (ignorable lower))
  (declare (ignorable upper))
  `(or ,@rules))

(defun value-range-alternatives (octets lower upper minimum maximum)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (declare (type (unsigned-byte 8) minimum))
  (declare (type (unsigned-byte 8) maximum))
  (declare (ignorable upper))
  (when (< lower upper)
    (let ((octet (aref octets lower)))
      (when (and (>= octet minimum)
                 (<= octet maximum))
        1))))

(defmacro sequence-group (octets lower upper &rest rules)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type symbol octets))
  (declare (type symbol lower))
  (declare (type symbol upper))
  (declare (type list rules))
  `(concatenation ,octets ,lower ,upper ,@rules))

(defmacro variable-repetition (octets lower upper rule &key
                                                         (minimum nil)
                                                         (maximum nil)
                                                         (padding 0))
  "The padding parameter is a hack intended to work around scenarios where the
rule subsequent to this variable-repetition is a strict subset of the rules
contained in this variable-repetition; that way, we don't inadvertently
match everything and end up with a dangling unmatched rule."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((min (intern (symbol-name 'min)))
        (max (intern (symbol-name 'max)))
        (n (intern (symbol-name 'n)))
        (n-rule (intern (symbol-name 'n-rule))))
    `(let ((,lower ,lower) ;; anaphoric; we're deliberately capturing lower symbols in the inner blocks
           (,min ,(if minimum minimum 0))
           (,max ,(if maximum maximum `(- ,upper ,lower))))
       (declare (type (simple-array (unsigned-byte 8) (*)) ,octets))
       (declare (type fixnum ,lower))
       (declare (type fixnum ,upper))
       (declare (type fixnum ,min))
       (declare (type fixnum ,max))
       (when (and (<= ,min ,upper)
                  (<= ,min ,max))
         (let ((,n-rule 0)
               (,n 0))
           (declare (type matched ,n-rule))
           (declare (type matched ,n))
           (loop for i fixnum from 1 to ,max
                 do (setf ,n-rule ,rule)
                    (when (not ,n-rule)
                      (when (and (<= i ,min) (> ,min 0))
                        (setf ,n ,+match-failure+))
                      (loop-finish))
                    (incf ,lower ,n-rule)
                    (incf ,n ,n-rule))
           (- ,n ,padding))))))

(defmacro specific-repetition (octets lower upper n rule)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  `(variable-repetition ,octets ,lower ,upper ,rule :minimum ,n :maximum ,n))

(defmacro optional-sequence (octets lower upper &rest rules)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type symbol octets))
  (declare (type symbol lower))
  (declare (type symbol upper))
  (declare (type list rules))
  `(variable-repetition ,octets
                        ,lower
                        ,upper
                        (sequence-group ,octets ,lower ,upper ,@rules)
                        :maximum 1))

;; ------------------------------------------------------------------------------

(defmacro capture (octets lower upper lower-captured upper-captured rule)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (ignorable octets))
  (declare (ignorable upper))
  (let ((n-rule (intern (symbol-name 'n-rule)))
        (n-rule-upper (intern (symbol-name 'n-rule-upper))))
    `(let ((,n-rule ,rule)
           (,n-rule-upper nil))
       (declare (type matched ,n-rule))
       (declare (type matched ,n-rule-upper))
       (when ,n-rule
         (setf ,n-rule-upper (+ ,lower ,n-rule))
         (setf ,lower-captured ,lower)
         (setf ,upper-captured ,n-rule-upper)
         ,n-rule))))

;; ------------------------------------------------------------------------------

(declaim (inline r-alpha))
(declaim (inline r-bit))
(declaim (inline r-char))
(declaim (inline r-cr))
(declaim (inline r-lf))
(declaim (inline r-crlf))
(declaim (inline r-ctl))
(declaim (inline r-digit))
(declaim (inline r-dquote))
(declaim (inline r-hexdig))
(declaim (inline r-htab))
(declaim (inline r-wsp))
(declaim (inline r-lwsp))
(declaim (inline r-octet))
(declaim (inline r-sp))
(declaim (inline r-vchar))

(defun r-alpha (octets lower upper)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (when (< lower upper)
    (let ((octet (aref octets lower)))
      (when (or (and (>= octet +#\A+)
                     (<= octet +#\Z+))
                (and (>= octet +#\a+)
                     (<= octet +#\z+)))
        1))))

(defun r-bit (octets lower upper)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (when (< lower upper)
    (let ((octet (aref octets lower)))
      (when (or (= octet +#\0+)
                (= octet +#\1+))
        1))))

(defrule r-char
    (value-range-alternatives +SOH+ +DEL+))

(defrule r-cr
    (terminal +CR+))

(defrule r-lf
    (terminal +LF+))

(defrule r-crlf
    (concatenation r-cr r-lf))

(defun r-ctl (octets lower upper)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (when (< lower upper)
    (let ((octet (aref octets lower)))
      (when (or (and (>= octet +NUL+)
                     (<= octet +US+))
                (= octet +DEL+))
        1))))

(defrule r-digit
    (value-range-alternatives +#\0+ +#\9+))

(defrule r-dquote
    (terminal +#\"+))

(defun r-hexdig (octets lower upper)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (when (< lower upper)
    (let ((octet (aref octets lower)))
      (when (or (and (>= octet +#\0+)
                     (<= octet +#\9+))
                (and (>= octet +#\a+)
                     (<= octet +#\f+))
                (and (>= octet +#\A+)
                     (<= octet +#\F+)))
        1))))

(defrule r-htab
    (terminal +HT+))

(defun r-wsp (octets lower upper)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (declare (type (simple-array (unsigned-byte 8) (*)) octets))
  (declare (type fixnum lower))
  (declare (type fixnum upper))
  (when (< lower upper)
    (let ((octet (aref octets lower)))
      (when (or (= octet +SP+)
                (= octet +HT+))
        1))))

(defrule r-lwsp
    (variable-repetition (alternatives r-wsp
                                       (concatenation r-crlf r-wsp))))

(defrule r-octet
    (value-range-alternatives #x00 #xff))

(defrule r-sp
    (terminal +SP+))

(defrule r-vchar
    (value-range-alternatives +#\!+ +#\~+))
