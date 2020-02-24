# regexp-comp-nfa-lisp
     
     Lo scopo del progetto è implementare in Lisp un compilatore
     da regexp a NFA.
     Sotto verranno definiti come vengono presi i dati in input e 
     manipolati dalle funzioni sotto elencate
     
     
     Di seguito si riportano delle regole fondamentali per la creazione
     delle delta e degli nfa che verrano generati dalle funzioni.
     
     Piccolo schema per le definizioni successive
         
         1) Per INPUT si intende un elemento dell'alfabeto del linguaggio
            che viene accettato dall' NFA
     
         2) Per START si intende lo stato dal quale una transizione "parte"
     
         3) Per FINAL si intende lo stato in cui si arriva consumando
            l'input 
            
     STATI come vengono definiti in maniera univoca?
     utilizzando (symbol-name (gensym "q-")) si ottiene una stringa
     "univoca" (se non si resetta gensym) la quale rappresenta lo stato
     e puo' essere confrontata con altri stati
     
     DELTA come viene rappresentata?
     In clisp una DELTA puo' essere rappresentata da una lista come segue
     
         DELTA = (list INPUT START FINAL)
     
     NFA come viene rappresentato?
     In clisp un NFA puo' essere rappresentato come una lista di delta
     
         NFA = (list DELTA-1 DELTA-2 ..... DELTA-N)
         
     IMPORTANTE!
     Per la generazione di un NFA sono state definite
     due regola fondamentali:
     
         1) La PRIMA DELTA contiene lo stato iniziale dell'NFA
         2) L' ULTIMA DELTA contiene lo stato finale dell'nfa
         
     QUINDI
     
         NFA (DELTA-1 .... DELTA-N)
         DELTA-1 = (INPUT STATO-INIZIALE-NFA ALTRO-STATO)
         DELTA-N = (INPUT ALTRO-STATO STATO-FINALE-NFA)
         
     E' UTILE VEDERE CHE
     
         NFA = (SUB-NFA-1 ... SUB-NFA-N)
         SUB-NFA = (DELTA-1 ... DELTA-N)
         
     QUINDI
     
         Un NFA E' UN INSIEME DI SUB-NFA
     
     
()===========================================================================()
()                             is-regexp(re)                                 ()
()---------------------------------------------------------------------------()

     Verifica se RE e' un'espressione regolare in base agli standard definiti
     Ritorna "t" se e' una regexp oppure "nil"
     Quindi se RE e' un elemetno dell'alfabeto torna true altrimenti
     invoca chiamate ricorsive che consumano il parametro in input
     
     
()===========================================================================()
()                           is-regexp-check-all(list)                       ()
()---------------------------------------------------------------------------()

     Effettua chiamate ricorsive per verificare se tutti gli elementi
     della lista in input siano delle regexp.
     
     
()===========================================================================()
()                             nfa-regexp-comp (RE)                          ()
()---------------------------------------------------------------------------()
    
    Controlla che RE sia una regexp e invoca sub-nfa-comp che andra'
    a costruire i sub-nfa che genereranno l'nfa completo
    
()===========================================================================()
()                             sub-nfa-comp (rs)                             ()
()---------------------------------------------------------------------------()
    
    In base al tipo di rs invochera' la corrispettiva funzione
    che andra' a generare il sub-nfa relativo a quella sub-regex
    Ritorna sempre un sub-nfa
    Si veda la spiegazione in testa al file per chiarimenti
    
()===========================================================================()
()                          sub-nfa-comp-base (atom)                         ()
()---------------------------------------------------------------------------()

    Caso base, genera il sub-nfa con una sola delta per quel 
    simbolo dell'alfabeto
    Genera anche i 2 stati "start" "final" per questo sub-nfa
    
    
()===========================================================================()
()                          sub-nfa-comp-star (rs)                           ()
()---------------------------------------------------------------------------()

    Caso star, genera il sub-nfa-star e ingloba il relativo sub-nfa
    della sub-regex
    Ritorna il sub-nfa con le 4 delta della star + le delta dei sub-nfa
    conseguenti alla sub-regexp
    
    
()===========================================================================()
()                          sub-nfa-comp-plus (rs)                           ()
()---------------------------------------------------------------------------()

    Modifica la struttura della sub-regex-plus
    andandola a trasformare in una ([] rs (* rs))
    Ritorna il sub-nfa relativo alla suddetta sub-regexp-plus
    
    
()===========================================================================()
()                         sub-nfa-comp-or (rs-list)                         ()
()---------------------------------------------------------------------------()

    Genera il sub-nfa-or per la lista di sub-regexp passata in input
    Questa funzione si serve di sub-nfa-comp-or-all per la creazione dei
    sub-nfa relativi ad ogni sub-regexp
    
    
()===========================================================================()
()               sub-nfa-comp-or-all (sr-list start final)                   ()
()---------------------------------------------------------------------------()

    Funzione ricorsiva che consuma la lista di sub-regexp e 
    crea per ognuna di queste un ramo del sub-nfa-or
    invocando sub-nfa-comp
    Crea anche gli stati iniziali e finali del sub-nfa-or
    
    
()===========================================================================()
()                        sub-nfa-comp-seq (sr-list)                         ()
()---------------------------------------------------------------------------()

    Genera il sub-nfa-seq per la lista di sub-regexp passata in input
    Questa funzione si serve di sub-nfa-comp-seq-all per la creazione dei
    sub-nfa relativi ad ogni sub-regexp
    
    
()===========================================================================()
()               sub-nfa-comp-seq-all (sr-list start final)                  ()
()---------------------------------------------------------------------------()

    Funzione ricorsiva che consuma la lista di sub-regexp e 
    crea per ognuna di queste una concatenazione di sub-nfa
    invocando sub-nfa-comp che generano il sub-nfa-seq totale
    Crea anche gli stati iniziali e finali del sub-nfa-seq
    
    
()===========================================================================()
()                           nfa-rec (FA Input)                              ()
()---------------------------------------------------------------------------()
  
    Funzione di arietà 2 che chiama nfa-rec-helper con gli
    stessi argomenti e gli passa anche lo stato stato iniziale
    e finale dell'NFA.
    Verifica che la struttura dell'nfa sia ben formata secondo le regole
    indicate nella testa del file
    
    
()===========================================================================()
()                nfa-rec-helper (nfa input state start final)               ()
()---------------------------------------------------------------------------()

    Funzione ausiliaria del predicato nfa-rec
    Verifica se lo stato corrente e' finale e se l'input e' consumato
    Se l'input e' consumato e lo stato corrente e' finale ritorna "t"
    Altrimenti scommette che una delta della delta-list possa portare ad uno 
    stato finale
    
    La delta-list (chiamata anche delta-bet) viene ritornata dalla funzione
    nfa-delta-list
    
    
()===========================================================================()
()                  nfa-rec-bet (nfa delta-bet input state start final)      ()
()---------------------------------------------------------------------------()

    Centro nevralgico del funzionamento della rec
    Scommette che la computazione di una delle delta nella delta-bet
    possa portare l'nfa in uno stato accettante.
    Se nessuna delle delta-bet porta in uno stato finale allora torna nil
    
    Effettua una ricorsone che consuma l'input oppure non lo consuma ed
    effettua una transizione con una EPSILON
    
    
()===========================================================================()
()                    nfa-rec-delta-list (nfa input state)                   ()
()---------------------------------------------------------------------------()

    Funzione che dato lo stato corrente ritorna le possibili delta
    dato l'input e l'nfa
    Effettua chiamate ricorsive che consumano l'nfa
    
    
()===========================================================================()
()                        check-alphabet (nfa input)                         ()
()---------------------------------------------------------------------------()

    Chiama la funzione  get-alphabet
    Chiama la funzione check-alphabet-helper
    Verifica che l'input appartenga all'alfabeto del linguaggio dell'nfa
    E' inutile effettuare una nfa-rec se gli elementi dell'input 
    non appartengono all'alfabeto
    
    
()===========================================================================()
()                              get-alphabet (nfa)                           ()
()---------------------------------------------------------------------------()

    Funzione ausiliare arietà 1
    Ritorna una lista contenente l'alfabeto dell'nfa
    
    
()===========================================================================()
()                 check-alphabet-helper (alphabet input)                    ()
()---------------------------------------------------------------------------()

    Effettua chiamate ricorsive consumando l'input verificando se 
    ogni elemento dell'input appartenga all'alfabeto
    
    
()===========================================================================()
()                         get-alphabet-helper (nfa)                         ()
()---------------------------------------------------------------------------()
 
    Ritorna la lista contenente l'alfabeto del linguaggio accettato dall'nfa
    contenente anche dei duplicati
    Effettua chiamate ricorsive per creare la lista consumando l'nfa

    
()===========================================================================()
()                          check-nfa-struct (nfa)                           ()
()---------------------------------------------------------------------------()
   
    Funzione di arieta' 1
    Contolla se la struttura dell' nfa sia ben definita invocando 
    il predicato "check-nfa-struct-helper"
    
    
()===========================================================================()
()                      check-nfa-struct-helper (nfa)                        ()
()---------------------------------------------------------------------------()
    
    Funzione ausiliaria di "check-nfa-struct" di arietà 1.
    Ricorsivamente consuma gli elementi dell'nfa (le delta) e ne verifica 
    la relativa struttura invocando "check-nfa-delta"
    
    
()===========================================================================()
()                         check-nfa-delta (delta)                           ()
()---------------------------------------------------------------------------()
    
    Fuone ausiliaria del predicato check-nfa-struct-helper arietà 1.
    Contolla che la struttura della delta sia ben definita.
