; $ clisp -i macro.lisp
; or
; $ clisp

; (let1 a 42
;       (+ a a)
;       (* a a))
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
        ,@body))
