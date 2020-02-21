#||
Progetto lisp regexp-comp-nfa
||#

(defun is-regexp (RE)
  (cond ((atom RE) t)
        ((consp RE) (is-regexp-consp (car RE) (cdr RE)))))

(defun is-regexp-consp (operator elements)
  (cond ((not (or(eq operator '*) (eq operator '+)
                 (eq operator '/) (eq operator '[]))) t)
        ((and (= (length elements) 1) (or (eq operator '*) (eq operator '+)))
         (is-regexp (car elements)))
        ((or (eq operator '/) (eq operator '[]))
         (is-regexp-check-all elements))))

(defun is-regexp-check-all (list)
  (cond ((null list) t)
        ((not (is-regexp (car list))) nil)
        (t (is-regexp-check-all (cdr list)))))


(defun nfa-regexp-comp (RE)
  (if (is-regexp RE) (nfa-regexp-comp-delta RE)))


(defun nfa-regexp-comp-delta (RE)
  (cond ((atom RE) (nfa-regexp-comp-delta-base RE))
        ((eq (car RE) '*) (nfa-regexp-comp-delta-star (cadr RE)))
        ((eq (car RE) '+) (nfa-regexp-comp-delta-plus (cadr RE)))
        ((eq (car RE) '/) (nfa-regexp-comp-delta-or (cdr RE)))
        ((eq (car RE) '[]) (nfa-regexp-comp-delta-seq (cdr RE)))
        ((consp RE) (nfa-regexp-comp-delta-base RE))))

(defun nfa-regexp-comp-delta-base (RE)
  (let ((q0 (gensym "q-")) (q1 (gensym "q-")))
    (list (list RE q0 q1))))

(defun nfa-regexp-comp-delta-star (RE)
  (let ((start (gensym "q-"))
        (final (gensym "q-"))
        (sub-delta (nfa-regexp-comp-delta RE)))
    (append (list (list 'epsilon start final))
            (list (list 'epsilon start (second (first sub-delta))))
            sub-delta
            (list (list 'epsilon (caddar (last sub-delta)) final)))))

(defun nfa-regexp-comp-delta-plus (RE)
  (nfa-regexp-comp-delta (list '[] RE (list '* RE))))

(defun nfa-regexp-comp-delta-or (RE)
  (let ((start (gensym "q-"))
        (final (gensym "q-")))
    (nfa-regexp-comp-delta-or-helper RE start final)))

(defun nfa-regexp-comp-delta-or-helper (list start final)
  (let ((sub-delta (nfa-regexp-comp (car list))))
    (if (null list) nil
      (append (list (list 'epsilon start (second (first sub-delta))))
              sub-delta
              (list (list 'epsilon (caddar (last sub-delta)) final))
              (nfa-regexp-comp-delta-or-helper (cdr list)
                                               start
                                               final)))))

(defun nfa-regexp-comp-delta-seq (RE)
  (let ((start (gensym "q-"))
        (final (gensym "q-")))
    (nfa-regexp-comp-delta-seq-helper RE start final)))

(defun nfa-regexp-comp-delta-seq-helper (list start final)
  (let ((sub-delta (nfa-regexp-comp (car list))))
    (if (null list) nil
      (append (list (list 'epsilon start (second (first sub-delta))))
              sub-delta
              (nfa-regexp-comp-delta-seq-helper (cdr list)
                                                (caddar (last sub-delta))
                                                final)))))
