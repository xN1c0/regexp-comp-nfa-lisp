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
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-"))))
    (list (list RE start final))))

(defun nfa-regexp-comp-delta-star (RE)
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-")))
        (sub-delta (nfa-regexp-comp-delta RE)))
    (append (list (list 'epsilon
                        start
                        final))
            (list (list 'epsilon
                        start
                        (second (first sub-delta))))
            sub-delta
            (list (list 'epsilon
                        (caddar (last sub-delta))
                        (second (first sub-delta))))
            (list (list 'epsilon
                        (caddar (last sub-delta))
                        final)))))

(defun nfa-regexp-comp-delta-plus (RE)
  (nfa-regexp-comp-delta (list '[] RE (list '* RE))))

(defun nfa-regexp-comp-delta-or (RE)
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-"))))
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
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-"))))
    (nfa-regexp-comp-delta-seq-helper RE start final)))

(defun nfa-regexp-comp-delta-seq-helper (list start final)
  (let ((sub-delta (nfa-regexp-comp (car list))))
    (if (= (length list) 1)
        (append (list (list 'epsilon start (second (first sub-delta))))
                sub-delta
                (list (list 'epsilon (caddar (last sub-delta)) final)))
      (append (list (list 'epsilon start (second (first sub-delta))))
              sub-delta
              (nfa-regexp-comp-delta-seq-helper (cdr list)
                                                (caddar (last sub-delta))
                                                final)))))

(defun nfa-rec (FA Input)
  (let ((start (second (first FA)))
        (final (caddar (last FA))))
    (nfa-rec-helper FA Input start start final)))

(defun nfa-rec-helper (FA input state start final)
  (let ((delta-list (nfa-rec-delta-list FA (car input) state)))
    (if (and (null input) (equal state final)) t
      (nfa-rec-bet FA delta-list input state start final))))

(defun nfa-rec-bet (FA delta-bet input state start final)
  (let ((delta-q (first delta-bet)))
    (cond ((null delta-q) nil)
          ((eq 'epsilon (first delta-q))
           (or (nfa-rec-helper FA input (third delta-q) start final)
               (nfa-rec-bet FA (rest delta-bet) input state start final)))
          ((eq (first input) (first delta-q))
           (or (nfa-rec-helper FA (rest input) (third delta-q) start final)
               (nfa-rec-bet FA (rest delta-bet) (rest input) state start final))))))

(defun nfa-rec-delta-list (FA input state)
  (let ((delta-nfa (first FA))
        (delta-input (list input state (third (first FA))))
        (delta-epsilon (list 'epsilon state (third (first FA)))))
    (cond ((null delta-nfa)
           nil)
          ((equal delta-nfa delta-input)
           (cons delta-nfa (nfa-rec-delta-list (rest FA) input state)))
          ((equal delta-nfa delta-epsilon)
           (cons delta-nfa (nfa-rec-delta-list (rest FA) input state)))
          (t (nfa-rec-delta-list (rest FA) input state)))))
