;;; Controlla se RE e' una regexp
;;; Ritorna "t" se RE e' una regexp
;;;         "nil" se non e' una regexp
(defun is-regexp (RE)
  (if (atom RE)
      (if (null RE) nil
        t)
    (let ((operator (car RE))
           (sr (cdr RE)))
      (cond ((not (or(eq operator '*) (eq operator '+)
                     (eq operator '/) (eq operator '[]))) t)
            ((and (= (length sr) 1)
                  (or (eq operator '*) (eq operator '+)))
             (is-regexp (car sr)))
            ((or (eq operator '/) (eq operator '[]))
             (is-regexp-check-all (cdr RE)))))))

;;; Funzione ausiliaria di is-regexp-consp
;;; Controlla se tutti gli elementi di list siano regexp
(defun is-regexp-check-all (list)
  (if (= (length list) 1)
      (is-regexp (car list))
    (and (is-regexp (car list))
         (is-regexp-check-all (cdr list)))))

;;; Crea un nfa
;;; Ritorna 1) una lista di delta transizioni che costituiscono l' nfa
;;;         2) "nil" se RE non e' una regexp
(defun nfa-regexp-comp (RE)
  (if (is-regexp RE) (sub-nfa-comp RE)))

;;; In base al tipo di rs chiama la corrispettiva funzione di composizione
;;; Ritorna un sub-nfa (una porzione di nfa, alla fine sara' l'intero nfa)
;;; sub-nfa = (list delta1 delta2 delta3 delta4 ... deltaN)
;;; deltaX = (list input start final)
;;; La prima delta del sub-nfa contiene lo stato iniziale del sub-nfa
;;; L' ultima delta del sub-nfa contiene lo stato finale del sub-nfa
;;; @see README
(defun sub-nfa-comp (rs)
  (if (atom rs) (sub-nfa-comp-base rs)
    (let ((operator (car rs)))
      (cond ((eq operator '*) (sub-nfa-comp-star (cadr rs)))
            ((eq operator '+) (sub-nfa-comp-plus (cadr rs)))
            ((eq operator '/) (sub-nfa-comp-or (cdr rs)))
            ((eq operator '[]) (sub-nfa-comp-seq (cdr rs)))
            (t (sub-nfa-comp-base rs))))))

;;; Caso base, compone il sub-nfa per il simbolo dell'alfabeto
;;; Ritorna un nfa con una sola delta (una lista di delta)
(defun sub-nfa-comp-base (atom)
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-"))))
    (list (list atom start final))))

;;; Caso star, compone il sub-nfa-star per la sub-regex
;;; Ritorna il sub-nfa con 4 delta della star + le delta generate dalla sub-regex
(defun sub-nfa-comp-star (rs)
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-")))
        (sub-nfa (sub-nfa-comp rs)))
    (let ((sub-start (second (first sub-nfa)))
          (sub-final (caddar (last sub-nfa))))
      (let ((delta-1 (list (list 'epsilon start final)))
            (delta-2 (list (list 'epsilon start sub-start)))
            (delta-3 (list (list 'epsilon sub-final sub-start)))
            (delta-4 (list (list 'epsilon sub-final final))))
        (append delta-1 delta-2 sub-nfa delta-3 delta-4)))))

;;; Caso plus, rilancia l'esecuzione modificando la struttura
;;; delle regex, (scompone il plus)
;;; Ritorna il sub-nfa-plus per la sub-regex
(defun sub-nfa-comp-plus (rs)
  (sub-nfa-comp (list '[] rs (list '* rs))))

;;; Caso or, compone il sub-nfa-or per la sub-regex
;;; Ritorna N delta per la creazione del sub-nfa-or +
;;; le delta generate dalle sub-regex
(defun sub-nfa-comp-or (rs-list)
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-"))))
    (sub-nfa-comp-or-all rs-list start final)))

;;; Funzione ausiliaria di sub-nfa-comp-or
;;; Crea i sub-nfa-or per ogni sub-regex e li collega per creare un ramo
;;; del sub-nfa-or generale
(defun sub-nfa-comp-or-all (sr-list start final)
  (if (null sr-list) nil
    (let ((sub-nfa (sub-nfa-comp (car sr-list))))
      (let ((sub-start (cadar sub-nfa))
            (sub-final (caddar (last sub-nfa))))
        (let ((delta-1 (list (list 'epsilon start sub-start)))
              (delta-2 (list (list 'epsilon sub-final final))))
          (append delta-1
                  sub-nfa
                  delta-2
                  (sub-nfa-comp-or-all (cdr sr-list) start final)))))))

;;; Caso sequence, compone il sub-nfa-seq per la sub-regex
;;; Ritorna N delta per la creazione del sub-nfa-seq +
;;; le delta generate dalle sub-regex
(defun sub-nfa-comp-seq (sr-list)
  (let ((start (symbol-name (gensym "q-")))
        (final (symbol-name (gensym "q-"))))
    (sub-nfa-comp-seq-all sr-list start final)))


;;; Funzione ausiliaria di sub-nfa-comp-seq
;;; Crea i sub-nfa-seq per ogni sub-regex e li collega per creare una
;;; sequenza dei sub-nfa-seq per generare il sub-nfa-seq completo
(defun sub-nfa-comp-seq-all (sr-list start final)
  (let ((sr-list-len (length sr-list))
        (sub-nfa (sub-nfa-comp (car sr-list))))
    (let ((sub-start (cadar sub-nfa))
          (sub-final (caddar (last sub-nfa))))
      (let ((delta-1 (list (list 'epsilon start sub-start))))
        (if (= sr-list-len 1)
            (let ((delta-2 (list (list 'epsilon sub-final final))))
              (append delta-1 sub-nfa delta-2))
          (append delta-1
                  sub-nfa
                  (sub-nfa-comp-seq-all (cdr sr-list) sub-final final)))))))

;;; Verifica se l'input viene accettato da L(FA)
;;; Ritorna 1) "t" se l'input viene accettato
;;;         2) "nil" se l'input non viene accettato
;;;         3) error se la struttura dell'nfa non e' ben formata
;;; @see README
(defun nfa-rec (FA Input)
  (if (check-nfa-struct FA)
      (if (and (listp Input)
               (check-alphabet FA Input))
          (let ((start (second (first FA)))
                (final (caddar (last FA))))
            (nfa-rec-helper FA Input start start final)))
    (error "~S is not an NFA" FA)))

;;; Funzione ausiliaria di nfa-rec
;;; Verifica se lo stato corrente e' finale e se l'input e' consumato
;;; Se l'input e' consumato e lo stato corrente e' finale ritorna "t"
;;; Altrimenti scommette che una delta della delta-list possa portare ad uno stato finale
(defun nfa-rec-helper (nfa input state start final)
  (let ((delta-list (nfa-rec-delta-list nfa (car input) state)))
    (if (and (null input) (equal state final)) t
      (nfa-rec-bet nfa delta-list input state start final))))

;;; Funzione fondamentale per la nfa-rec-helper
;;; Scommette computazionalmente che una delle delta della delta-bet
;;; possano portare ad uno stato finale
;;; Se nessuna porta ad uno stato finale allora ritorna nil
(defun nfa-rec-bet (nfa delta-bet input state start final)
  (let ((delta-q (first delta-bet)))
    (cond ((null delta-q) nil)
          ((equal 'epsilon (first delta-q))
           (or (nfa-rec-helper nfa input (third delta-q) start final)
               (nfa-rec-bet nfa (rest delta-bet) input state start final)))
          ((equal (first input) (first delta-q))
           (or (nfa-rec-helper nfa (rest input) (third delta-q) start final)
               (nfa-rec-bet nfa (rest delta-bet) (rest input) state start final))))))

;;; Ritorna la lista di tutte le delta che posso percorrere
;;; quando mi trovo in uno stato specifico
(defun nfa-rec-delta-list (nfa input state)
  (let ((delta-nfa (first nfa))
        (delta-input (list input state (third (first nfa))))
        (delta-epsilon (list 'epsilon state (third (first nfa)))))
    (cond ((null delta-nfa)
           nil)
          ((equal delta-nfa delta-input)
           (cons delta-nfa (nfa-rec-delta-list (rest nfa) input state)))
          ((equal delta-nfa delta-epsilon)
           (cons delta-nfa (nfa-rec-delta-list (rest nfa) input state)))
          (t (nfa-rec-delta-list (rest nfa) input state)))))

;;; Funzione supplementare
;;; Controlla se ogni elemento dell'input (della lista in input)
;;; appartenga all'alfabeto
;;; Ritorna "t" se ogni elemetnto dell'input appartiene all'alfabeto
(defun check-alphabet (nfa input)
  (let ((alphabet (get-alphabet nfa)))
    (check-alphabet-helper alphabet input)))

;;; Funzione ausiliaria di check-alphabet
;;; Effettua le chiamate ricorsive per controllare tutti gli elementi dell' input
(defun check-alphabet-helper (alphabet input)
  (if (null input) t
    (let ((element (car input)))
      (if (find element alphabet :test #'equal)
          (check-alphabet-helper alphabet (cdr input))))))

;;; Funzione fondamentale per check-alphabet
;;; Ritorna lista contenente l'alfabeto del linguaggio accettato dell'nfa
(defun get-alphabet (nfa)
  (remove-duplicates (get-alphabet-helper nfa)))

;;; Funzione ausiliaria di get-alphabet
;;; Ritorna la lista contenente l'alfabeto del linguaggio accettato dall'nfa
;;; contenente anche dei duplicati
;;; Effettua chiamate ricorsive per creare la lista consumando l'nfa
(defun get-alphabet-helper (nfa)
  (if (null nfa) nil
    (let ((delta (car nfa)))
      (cons (car delta) (get-alphabet-helper (cdr nfa))))))

;;; Funzione fondamentale per nfa-rec
;;; Verifica che la struttura dell'nfa sia ben definita
;;; Ritorna "t" se e' ben definito
;;;         "nil" se non e' ben definito
(defun check-nfa-struct (nfa)
  (if (listp nfa)
      (check-nfa-struct-helper nfa)))

;;; Funzione ausiliaria di check-nfa-struct
;;; Effettua chiamate ricorsive per controllare la struttura di tutte le delta
;;; andando a consumare l'nfa
(defun check-nfa-struct-helper (nfa)
  (let ((delta (car nfa)))
    (if (= (length nfa) 1)
        (check-nfa-delta delta)
      (and (check-nfa-struct-helper (cdr nfa))))))

;;; Funzione fondamentale per check-nfa-struct-helper
;;; Controlla che la struttura della delta sia ben definita
;;; Ritorna "t" se e' ben definita
;;;         "nil" se non lo e'
(defun check-nfa-delta (delta)
  (if (and (consp delta) (= (length delta) 3))
      (let ((input (car delta))
            (start (cadr delta))
            (final (caddr delta)))
        (if (and (stringp start)
                 (stringp final)
                 (if (atom input) t
                   (let ((fir (car input)))
                     (if (not (or (eq fir '*)
                                  (eq fir '+)
                                  (eq fir '/)
                                  (eq fir '[])))
                         t
                       nil))))
            t
          nil))
    nil))
