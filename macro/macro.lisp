; CLISP does not understand japanese characters.
; If you use them, you get an error.

; $ clisp
; > (load "macro.lisp")
; or
; $ clisp -i macro.lisp

; One local variable "let".
; (let1 a 42
;       (+ a a)
;       (* a a))
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
        ,@body))

; (split '(2 3)
;        (format t "This can be split into ~a and ~a." head tail)
;        (format t "This cannot be split."))
; => This can be split into 2 and (3).
(defmacro split (val yes no)
  (let1 g (gensym)
	`(let1 ,g ,val
	       (if ,g
		   (let ((head (car ,g))
			 (tail (cdr ,g)))
		        ,yes)
		   ,no))))

; (pairs '(a b c d e f))
; => ((A . B) (C . D) (E . F))
; (pairs '(a b c d e))
; => ((A . B) (C . D))
(defun pairs (lst)
  (labels ((f (lst acc)
	      (split lst
		     (if tail  ; lst is not empty.
		         (f (cdr tail) (cons (cons head (car tail)) acc))
			 (reverse acc))
		     (reverse acc))))  ; lst is empty.
          (f lst nil)))

; (recurse (n 3)
;   (fresh-line)
;   (if (zerop n)
;       (princ "lift-off!")
;       (progn (princ n)
;              (self (1- n)))))
; => 3
; => 2
; => 1
; => lift-off!
(defmacro recurse (vars &body body)
  (let1 p (pairs vars)  ; ((n . 3)), p is not by gensym because p is invoked on macro expantion.
	; Invoke a local function with n being 3.
	`(labels ((self ,(mapcar #'car p)  ; (mapcar #'car '((n . 3))) -> ((car '(n . 3))) -> (n)
			,@body))
	         (self ,@(mapcar #'cdr p)))))  ; (mapcar #'cdr '((n . 3))) -> ((cdr '(n . 3))) ->(3)

(defun my-length (lst)
  (recurse (lst lst  ; Set lst lst and acc 0 as the initial value then invoke the body.
            acc 0)
	   (split lst
		  (self tail (1+ acc))
		  acc)))

(defun my-length-w-reduce (lst)
  (reduce (lambda (x i)
	          (1+ x))
	  lst
	  :initial-value 0))

(defun car-func (lst)
  (car lst))
(defmacro car-macro (lst)
  (car lst))
; (car       '(2 3))  => 2
; (car        (2 3))  -> Error: EVAL: 2 is not a function name; try using a symbol instead
; (car-func  '(2 3))  => 2
; (car-func   (2 3))  -> Error: EVAL: 2 is not a function name; try using a symbol instead
; (car-macro '(2 3))  -> Error: SYSTEM::READ-EVAL-PRINT: variable QUOTE has no value
; (car-macro  (2 3))  => 2
