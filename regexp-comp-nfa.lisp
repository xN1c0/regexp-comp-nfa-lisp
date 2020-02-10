#||
Progetto lisp regexp-comp-nfa
||#

(defun is-regexp (RE)
  (cond ((atom RE) t)
        ((and (eq (car RE) '*) (= (length (cdr RE)) 1))
         (is-regexp (cadr RE)))
        ((and (eq (car RE) '+) (= (length (cdr RE)) 1))
         (is-regexp (cadr RE)))
        ((eq (car RE) '/)
         (is-regexp-check-all (cdr RE)))
        ((eq (car RE) '[])
         (is-regexp-check-all (cdr RE)))))

(defun is-regexp-check-all (list)
  (cond ((null list) t)
        ((not (is-regexp (car list))) nil)
        (t (is-regexp-check-all (cdr list)))))
