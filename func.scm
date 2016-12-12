; Safe Declaration
(declare (usual-integrations))
; Additional Scheme Library
(load "atomic.scm") ; Atomic-4 Library
(load "harmony.scm") ; Harmony

;----------------------------------------
;  Global Constants
(define subject-generation-num 2)
(define subject-candidate-num 10)
(define subject-offspring-per-candidate-num 10)

(define middle-generation-num 2)
(define middle-candidate-num 10)
(define middle-offspring-per-candidate-num 10)

(define coda-generation-num 2)
(define coda-candidate-num 10)
(define coda-offspring-per-candidate-num 10)

(define voice-range-min '(14 21 28))
(define voice-range-max '(28 35 42))
;----------------------------------------
; General functions

(define aggregate-list
  (lambda (sub-total list-var)
    (if (null? list-var) '()
      (cons (+ sub-total (car list-var))
        (aggregate-list (+ sub-total (car list-var)) (cdr list-var))
      )
    )
  )
)
(define replace-symbol
  (lambda (var bindings)
    (cond
      ((list? var)
        (cond
          ((null? var) '())
          (else (cons (replace-symbol (car var) bindings) (replace-symbol (cdr var) bindings)))
        )
      )
      ((symbol? var)
        (let ((possible-list 
                (apply append     
                  (map (lambda (x)
                    (if (eqv? var (car x)) (list (cadr x)) '()))
                    bindings
                  )
                )
             ))
             (if (null? possible-list)
               var
               (car possible-list)
             )
        )
      )
      (else var)
    )
  )
)

(define replace-list
  (lambda (var bindings)
    (cond
      ((list? var)
        (cond
          ((null? var) '())
          (else 
            (let 
              ((possible-list 
                (apply append     
                  (map (lambda (x)
                    (if (equal? var (car x)) (list (cadr x)) '()))
                    bindings
                  )
                )
              ))
              (if (null? possible-list)
                (cons (replace-list (car var) bindings) (replace-list (cdr var) bindings))
                (car possible-list)
              )
            )
          )
        )
      )
      (else var)
    )
  )
)

(define eval!
  (lambda (s-expression)
    (eval s-expression user-initial-environment)
  )
)
(define eval-bindings
  (lambda (s-expression bindings)
    (eval s-expression (extend-top-level-environment user-initial-environment (map car bindings) (map cadr bindings)))
  )
)

(define even-cond
  (lambda condition-list
    (let ((c-length (length condition-list)))
      (if (> c-length 0)
        (list-ref condition-list (floor->exact (* (random 1.0) c-length))) 
        #f
      )
    )
  )
)
    
(define zipf-cond
  (lambda condition-list
    (let ((c-length (length condition-list)))
      (if (> c-length 0)
        (list-ref condition-list (- (floor->exact (expt (+ c-length 1) (random 1.0))) 1))
        #f
      )
    )
  )
)


(define alist-append
  (lambda (primary-alist secondary-alist)
    (if (null? secondary-alist)
      primary-alist
      (let
        ((primary-match-tuple (assoc (caar secondary-alist) primary-alist)))
        (if primary-match-tuple
          (if (equal? primary-match-tuple (car secondary-alist))
            (alist-append primary-alist (cdr secondary-alist))
            #f
          )
          (alist-append (append primary-alist (list (car secondary-alist))) (cdr secondary-alist))
        )
      )
    )
  )
)

(define sequence
  (lambda (begin-var end-var)
    (cond
      ((> begin-var end-var) '())
      (else (cons begin-var (sequence (+ begin-var 1) end-var)))
    )
  )
)

;----------------------------------------
; Note-related functions

; "AAA#" --> #\A
; "cc"   --> #\c
(define Note-degree
  (lambda (kern-note-var)
    (let
      (
        (first-character (string-ref kern-note-var 0))
      )
      (if (memq first-character '(#\A #\B #\C #\D #\E #\F #\G #\a #\b #\c #\d #\e #\f #\g))
        first-character
        #f
      )
    )
  )
)
; #\E --> 2
; #\c --> 0
(define Note-degree-to-integer
  (lambda (Note-degree-var)
    (let
      (
        (result-tuple (assoc Note-degree-var '((#\A 5)(#\B 6)(#\C 0)(#\D 1)(#\E 2)(#\F 3)(#\G 4)
                             (#\a 5)(#\b 6)(#\c 0)(#\d 1)(#\e 2)(#\f 3)(#\g 4)))
        )
      )
      (if result-tuple (cadr result-tuple) #f)
    )
  )
)
; 0 #t --> #\C
; 1 #f --> #\d
(define integer-to-Note-degree
  (lambda (int-var cap)
    (let
      (
        (result-tuple (assoc int-var '((5 #\A)(6 #\B)(0 #\C)(1 #\D)(2 #\E)(3 #\F)(4 #\G))))
      )
      (if result-tuple 
        (if cap
          (char-upcase (cadr result-tuple))
          (char-downcase (cadr result-tuple))
        )
        #f
      )
    )
  )
)

; "cc" --> 5
; "D#" --> 3
(define Note-octave
  (lambda (kern-note-var)
    (let 
      (
        (length-offset 
          (if 
            (memq (string-ref kern-note-var (- (string-length kern-note-var) 1))
              '(#\# #\-)
            )
            -1
            0
          )
        )
        (pseudo-length (string-length kern-note-var))
        (upper-case (char-upper-case? (string-ref kern-note-var 0)))
      )
      (if upper-case
        (- 4 pseudo-length length-offset)
        (+ 3 pseudo-length length-offset)
      )
    )
  )
)
; "AAA#" -> "AAA"
; "cc"   -> "cc"
(define Note-degree-octave
  (lambda (kern-note-var)
    (let 
      (
        ( last-character 
          (string-ref kern-note-var (- (string-length kern-note-var) 1))
        )
      )
      (if 
        (or 
          (eqv? last-character #\#)
          (eqv? last-character #\-)
        )
        (substring kern-note-var 0 (- (string-length kern-note-var) 1))
        kern-note-var
      )
    )
  )
)
; "AAA#" --> #\#
; "dd-"  --> #\-
; "C"    --> #f
(define Note-quality
  (lambda (kern-note-var)
    (let 
      (
        (last-character 
          (string-ref 
            kern-note-var 
            (- (string-length kern-note-var) 1)
          )
        )
      )
      (if 
        (or 
          (eqv? last-character #\#)
	  (eqv? last-character #\-)
	)
	last-character
	#f
      )
    )
  )
)

(define Note-quality-append
  (lambda (kern-note-var quality-var)
    (string-append kern-note-var (char->name quality-var))
  )
)
; "c"  --> 28
; "D"  --> 22
; "ee#" --> 37
(define Note-to-integer
  (lambda (Note-var)
    (let
      (
        (Note-octave-val (Note-octave Note-var))
        (Note-degree-val (Note-degree-to-integer (Note-degree Note-var)))
      )
      (if (and Note-octave-val Note-degree-val)
        (+ (* 7 Note-octave-val) Note-degree-val)
        #f
      )
    )
  )
)
; 28 --> "c"
; 22 --> "D"
; 37 --> "ee"
(define integer-to-Note
  (lambda (int-var)
    (let
      (
        (degree-char (integer-to-Note-degree (modulo int-var 7) (< int-var 28)))
        (Note-string-length
          (if (< int-var 28)
            (+ (quotient (- 27 int-var) 7) 1)
            (+ (quotient (- int-var 28) 7) 1)
          )
        )
      )
      (make-string Note-string-length degree-char)
    )
  )
)
; "c"  1        --> "d"
; "BB" '(0 #\-) --> "BB-"
(define Note-offset
  (lambda (Note-var Note-offset-var)
    (let
      (
        (Note-offset-val
          (if (list? Note-offset-var)
            (car Note-offset-var)
            Note-offset-var
          )
        )
        (Note-quality-val
          (if (list? Note-offset-var)
            (cadr Note-offset-var)
            #f
          )
        )
      )
      (let*
        (
          (Note-degree-octave-val (integer-to-Note (+ (Note-to-integer Note-var) Note-offset-val)))
          (Note-degree-val (Note-degree Note-degree-octave-val))
          (exempted-quality
            (if Note-quality-val
              (member (string-append (char->name Note-degree-val) (char->name Note-quality-val))
                '("A#" "a#" "G-" "g-" "F-" "f-" "E#" "e#" "D#" "d#" "D-" "d-" "C-" "c-" "B#" "b#")
              )
              #t
            )
          )
        )
        (if exempted-quality
          Note-degree-octave-val
          (string-append Note-degree-octave-val (char->name Note-quality-val))
        )
      )
    )
  )
)

; "c"  1        --> "b"
; "BB" '(0 #\-) --> "BB"
(define Note-offset-reverse
  (lambda (Note-var Note-offset-var)
    (let
      (
        (Note-offset-val
          (if (list? Note-offset-var)
            (car Note-offset-var)
            Note-offset-var
          )
        )
      )
      (integer-to-Note (- (Note-to-integer Note-var) Note-offset-val))
    )
  )
)
          
      
; "Attack" represents an attack in a specific timestamp.
(define Attack 'k)

(define attack?
  (lambda (Vertex-candidate)
    (eqv? Attack Vertex-candidate)
  )
)
; "Rest" represents a rest in a specific timestamp.
(define Rest 'r)

(define rest?
  (lambda (Vertex-candidate)
    (eqv? Rest Vertex-candidate)
  )
)
; "Unknown" represents no information is available regarding a specific timestamp.
(define Unknown '?)

(define unknown?
  (lambda (Vertex-candidate)
    (eqv? Unknown Vertex-candidate)
  )
)
(define sub-note? 
  (lambda (first-character rest-string)
    (if
      (memq first-character '(#\A #\B #\C #\D #\E #\F #\G #\a #\b #\c #\d #\e #\f #\g))
      (if
        (member rest-string '("#" "-" ""))
        #t
	(if
          (char=? first-character (string-ref rest-string 0))
          (sub-note? (string-ref rest-string 0) (substring rest-string 1 (string-length rest-string)))
	  #f
	)
      )
      #f
    )
  )
)
(define note?
  (lambda (Vertex-candidate)
    (if
      (string? Vertex-candidate)
      (sub-note? (string-ref Vertex-candidate 0) (substring Vertex-candidate 1 (string-length Vertex-candidate)))
      #f
    )
  )
)
(define general-attack?
  (lambda (Vertex-candidate)
    (or (note? Vertex-candidate) (attack? Vertex-candidate))
  )
)
;----------------------------------------
; Vertex-related functions

(define Vertex-match
  (lambda (primary-vertex secondary-vertex)
    (cond
      ((note? primary-vertex) primary-vertex)
      ((rest? primary-vertex) Rest)
      ((note? secondary-vertex) secondary-vertex)
      ((attack? primary-vertex) Attack)
      ((attack? secondary-vertex) Attack)
      ((rest? secondary-vertex) Rest)
      (else Unknown)
    )
  )
)

(define strict-Vertex-match
  (lambda (primary-vertex secondary-vertex)
    (cond
      ((and (unknown? primary-vertex) (unknown? secondary-vertex)) Unknown)
      ((and (unknown? primary-vertex) (attack? secondary-vertex)) Attack)
      ((and (unknown? primary-vertex) (rest? secondary-vertex)) Rest)
      ((and (unknown? primary-vertex) (note? secondary-vertex)) secondary-vertex)
      ((and (attack? primary-vertex) (unknown? secondary-vertex)) Attack)
      ((and (attack? primary-vertex) (attack? secondary-vertex)) Attack)
      ((and (attack? primary-vertex) (rest? secondary-vertex)) #f)
      ((and (attack? primary-vertex) (note? secondary-vertex)) secondary-vertex)
      ((and (rest? primary-vertex) (unknown? secondary-vertex)) Rest)
      ((and (rest? primary-vertex) (attack? secondary-vertex)) #f)
      ((and (rest? primary-vertex) (rest? secondary-vertex)) Rest)
      ((and (rest? primary-vertex) (note? secondary-vertex)) #f)
      ((and (note? primary-vertex) (unknown? secondary-vertex)) primary-vertex)
      ((and (note? primary-vertex) (attack? secondary-vertex)) primary-vertex)
      ((and (note? primary-vertex) (rest? secondary-vertex)) #f)
      ((and (note? primary-vertex) (note? secondary-vertex))
        (if (string=? primary-vertex secondary-vertex) primary-vertex #f)
      )
      (else #f)
    )
  )
)

(define strict-Ring-match
  (lambda (primary-ring secondary-ring)
    (if
      (and
        (or (attack? (car primary-ring))(note? (car primary-ring)))
        (or (attack? (car secondary-ring))(note? (car secondary-ring)))
      )
      (let
        (
          (Vertex-match-result (strict-Vertex-match (car primary-ring) (car secondary-ring)))
          (Edge-match-result (alist-append (cdr primary-ring) (cdr secondary-ring)))
        )
        (if Vertex-match-result
          (if Edge-match-result
            (cons
              Vertex-match-result
              Edge-match-result
            )
            #f
          )
          #f
        )
      )
      (let
        ((Vertex-match-result (strict-Vertex-match (car primary-ring) (car secondary-ring))))
        (if Vertex-match-result
          (cons
            Vertex-match-result
            (append
              (cdr primary-ring)
              (cdr secondary-ring)
            )
          )
          #f
        )
      )
    )
  )
)

(define strict-Graph-match
  (lambda (primary-graph secondary-graph)
    (if (= (length primary-graph) (length secondary-graph))
      (let
        ((new-graph (map strict-Ring-match primary-graph secondary-graph)))
        (if (member #f new-graph)
          #f
          new-graph
        )
      )
      #f
    )
  )
)
(define vertex?
  (lambda (Vertex-candidate)
    (or
      (attack? Vertex-candidate)
      (rest? Vertex-candidate)
      (unknown? Vertex-candidate)
      (note? Vertex-candidate)
    )
  )
)


;----------------------------------------
; Graph-related functions

(define adjacent-known
  (lambda (Graph-var num direction)
    (if
      (or
        (< num 0)
        (>= num (length Graph-var))
      )
      #f
      (if (or 
            (unknown? (car (list-ref Graph-var num)))
          )
        (adjacent-known Graph-var (+ num direction) direction)
        num
      )
    )
  )
)
(define adjacent-attack
  (lambda (Graph-var num direction)
    (if
      (or
        (< num 0)
        (>= num (length Graph-var))
      )
      #f
      (if (or 
            (rest? (car (list-ref Graph-var num)))
            (unknown? (car (list-ref Graph-var num)))
          )
        (adjacent-attack Graph-var (+ num direction) direction)
        num
      )
    )
  )
)

(define last-attack
  (lambda (Graph-var num)
    (adjacent-attack Graph-var (- num 1) -1)
  )
)
(define last-attack-edge
  (lambda (Graph-var num)
    (let
      ((ring-var (last-attack Graph-var num)))
      (if ring-var
        (assoc 1 (cdr ring-var))
        #f
      )
    )
  )
)

(define next-attack
  (lambda (Graph-var num)
    (adjacent-attack Graph-var (+ num 1) 1)
  )
)
; An intermediate-list is comprised of:
; ((<ring-containing-edge-location1> <edge-offset-variable1> <affected-vertex-location1> <note-var1>) ...)
(define Graph-delete-edge
  (lambda (Graph-var intermediate-list)
    (if (null? intermediate-list)
      Graph-var
      (Graph-delete-edge
        (let
          (
            (ring-num (caar intermediate-list))
            (edge-index (cadar intermediate-list))
          )
          (append
            (list-head Graph-var ring-num)
            (let
              (
                (chosen-ring (list-ref Graph-var ring-num))
              )
              (list
                (cons
                  (car chosen-ring)
                  (del-assoc edge-index (cdr chosen-ring))
                )
              )
            )
            (list-tail Graph-var (+ ring-num 1))
          )
        )
        (cdr intermediate-list)
      )
    )
  )
)
(define Graph-delete-external-edge
  (lambda (Graph-var)
    (if Graph-var
      (let
        ((pseudo-intermediate-list
          (apply append
            (map
              (lambda (Ring-index)
                (apply append
                  (map
                    (lambda (edge-var)
                      (if (= 1 (car edge-var))
                        ; Next Attack
                        (if (next-attack Graph-var Ring-index)
                          '()
                          (list (list Ring-index 1))
                        )
                        (if (>= (+ (car edge-var) Ring-index) (length Graph-var))
                          (list (list Ring-index (car edge-var)))
                          '()
                        )
                      )
                    )
                    (cdr (list-ref Graph-var Ring-index))
                  )
                )
              )
              (sequence 0 (- (length Graph-var) 1))
            )
          )
        ))
        (Graph-delete-edge Graph-var pseudo-intermediate-list)
      )
      #f
    )
  )
)
(define Graph-delete-external-edge-modify-first-attack-edge
  (lambda (Graph-var edge-offset)
    (Graph-modify-first-attack-edge (Graph-delete-external-edge Graph-var) edge-offset)
  )
)
(define Graph-delete-external-edge-modify-last-attack-edge
  (lambda (Graph-var edge-offset)
    (Graph-modify-last-attack-edge (Graph-delete-external-edge Graph-var) edge-offset)
  )
)
(define Graph-get-edge
  (lambda (Graph-var ring-num edge-index)
    ; Boundary check
    (if 
      (or 
        (not (list? Graph-var))
        (< ring-num 0)
        (>= ring-num (length Graph-var))
      )
      #f
      (assoc edge-index (cdr (list-ref Graph-var ring-num)))
    )
  )
)

; Generate assignment-list
(define Graph-assignment-list
  (lambda (intermediate-list)
    (map cddr intermediate-list)
  )
)
; '( (k (2 1)) (r) (k (1 2)) (k)) '(2 "c")
; -->
; '((0 2 0 "B")(2 1 3 "e"))
; For test only

(define Graph-intermediate-list
  (lambda (Graph-var assignment-tuple)
    (if (rest? (cadr assignment-tuple))
      '()
      (append
        ; has the ring 16th,8th,4th,2nd ahead got an edge for this ring?
        (apply append
          (map
            (lambda (time-offset) ; e.g. 16
              (let
                ((edge-var (Graph-get-edge Graph-var (- (car assignment-tuple) time-offset) time-offset)))   ; e.g. '(16 (3 #\#))
                (if edge-var
                  (list 
                    (list
                      (- (car assignment-tuple) time-offset)
                      time-offset
                      (- (car assignment-tuple) time-offset)
                      (Note-offset-reverse (cadr assignment-tuple) (cadr edge-var))
                    )
                  )
                  '()
                )
              )
            )
            '(16 8 4 2)
          )
        )
        ; What about last attack?
        (let*
          (
            (ring-num (last-attack Graph-var (car assignment-tuple)))
            (edge-var (if ring-num (assoc 1 (cdr (list-ref Graph-var ring-num))) #f))
          )
          (if edge-var
            (list
              (list
                ring-num
                1
                ring-num
                (Note-offset-reverse (cadr assignment-tuple) (cadr edge-var))
              )
            )
            '()
          )
        )
        ; Processing edges in the current Ring
        (apply append
          (map
            (lambda (edge-var)
              (if (= 1 (car edge-var))
                ; Find next attack
                (let
                  (
                    (ring-num (next-attack Graph-var (car assignment-tuple)))
                  )
                  (if ring-num
                    (list
                      (list
                        (car assignment-tuple)
                        1
                        ring-num
                        (Note-offset (cadr assignment-tuple) (cadr edge-var))
                      )
                    )
                    '()
                  )
                )
                (if 
                  (and
                    (< (+ (car assignment-tuple) (car edge-var)) (length Graph-var))
                    (>= (+ (car assignment-tuple) (car edge-var)) 0)
                  )
                  (list 
                    (list
                      (car assignment-tuple)
                      (car edge-var)
                      (+ (car assignment-tuple) (car edge-var))
                      (Note-offset (cadr assignment-tuple) (cadr edge-var))
                    )
                  )
                  '()
                )
              )
            )
            (cdr (list-ref Graph-var (car assignment-tuple)))
          )
        )
      )
    )
  )
)

; Assign an value
(define Graph-assign
  (lambda (Graph-var . assignment-list-tail)
    ;(display "Graph: ")
    ;(display Graph-var)
    ;(newline)
    ;(display "Assignment:")
    ;(display assignment-list-tail)
    ;(newline)
    (if (null? assignment-list-tail)
      Graph-var
      (if Graph-var
        (let
          (
            (new-Graph-var
              (if 
                (or
                  (< (caar assignment-list-tail) 0)
                  (>= (caar assignment-list-tail) (length Graph-var))
                )
                #f
                (let*
                  (
                    (old-Vertex-val (car (list-ref Graph-var (caar assignment-list-tail))))
                    (new-Vertex-val (Vertex-match old-Vertex-val (cadar assignment-list-tail)))
                  )
                  (if new-Vertex-val
                    (append
                      (list-head Graph-var (caar assignment-list-tail))
                      (list
                        (cons
                          new-Vertex-val
                          (cdr (list-ref Graph-var (caar assignment-list-tail)))
                        )
                      )
                      (list-tail Graph-var (+ (caar assignment-list-tail) 1))
                    )
                    #f
                  )
                )
              )
            )
            (intermediate-list
              (Graph-intermediate-list
                Graph-var
                (car assignment-list-tail)
              )
            )
          )
          (apply Graph-assign
            (cons 
              (Graph-delete-edge new-Graph-var intermediate-list)
              (append 
                (cdr assignment-list-tail)
                (Graph-assignment-list intermediate-list)
              )
            )
          )
        )
        #f
      )
    )
  )
)
(define sub-Graph-reassign
  (lambda (Graph-var ring-num)
    (if (>= ring-num (length Graph-var))
      Graph-var
      (if (note? (car (list-ref Graph-var ring-num)))
        (sub-Graph-reassign
          (apply Graph-assign (list Graph-var (list ring-num (car (list-ref Graph-var ring-num)))))
          (+ ring-num 1)
        )
        (sub-Graph-reassign Graph-var (+ ring-num 1))
      )
    )
  )
)
(define Graph-assign-first-attack
  (lambda (Graph-var Vertex-var)
    (if Graph-var
      (let
        ((begin-attack-ring-num (adjacent-attack Graph-var 0 1)))
        (if begin-attack-ring-num
          (Graph-assign Graph-var (list begin-attack-ring-num Vertex-var))
          Graph-var
        )
      )
      Graph-var
    )
  )
)
(define Graph-reassign
  (lambda (Graph-var)
    (if Graph-var
      (sub-Graph-reassign Graph-var 0)
      #f
    )
  )
)

(define Graph-fill-rest
  (lambda (Graph-var)
    (if Graph-var
      (apply Graph-assign
        (cons
          Graph-var
          (apply append
            (map 
              (lambda (ring-var ring-index)
                (if (unknown? (car ring-var))
                  (list (list ring-index 'r))
                  '()
                )
              )
              Graph-var
              (let
                (
                  (begin-ring-index (adjacent-known Graph-var 0 1))
                  (end-ring-index (adjacent-known Graph-var (- (length Graph-var) 1) -1))
                )
                (if (and begin-ring-index end-ring-index)
                  (sequence begin-ring-index end-ring-index)
                  '()
                )
              )
            )
          )
        )
      )
      #f
    )
  )
)

(define Graph-append
  (lambda (Graph-var . Graph-tail)
    (if (null? Graph-tail)
      Graph-var
      (if Graph-var
        (let
          (
            (Graph-appendix (car Graph-tail))
          )
          (if (and 
                (memq (length Graph-appendix) '(16 8 4 2))
                (>= (length Graph-var) (/ (length Graph-appendix) 2))
              )
            (let*
              (
                (common-length (/ (length Graph-appendix) 2))
                (Graph-match-result 
                  (strict-Graph-match 
                    (list-tail 
                      Graph-var 
                      (- (length Graph-var) common-length)
                    )
                    (list-head Graph-appendix common-length)
                  )
                )
              )
              (if Graph-match-result
                (apply Graph-append
                  (cons 
                    (Graph-reassign
                      (append
                        (list-head Graph-var (- (length Graph-var) common-length))
                        Graph-match-result
                        (list-tail Graph-appendix common-length)
                      )
                    )
                    (cdr Graph-tail)
                  )
                )
                (apply Graph-append
                  (cons
                    (Graph-reassign (append Graph-var Graph-appendix))
                    (cdr Graph-tail)
                  )
                )
              )
            )
            (apply Graph-append
              (cons
                (Graph-reassign (append Graph-var Graph-appendix))
                (cdr Graph-tail)
              )
            )
          )
        )
        #f
      )
    )
  )
)
(define Graph-simple-append
  (lambda (Graph-var . Graph-tail)
    (if (null? Graph-tail)
      Graph-var
      (if Graph-var
        (apply Graph-append
          (cons
            (Graph-reassign (append Graph-var (car Graph-tail)))
            (cdr Graph-tail)
          )
        )
        #f
      )
    )
  )
)

(define Graph-repeat
  (lambda (Graph-var repetition)
    (if (<= repetition 1)
      Graph-var
      (if Graph-var
        (Graph-append
          (Graph-repeat Graph-var (- repetition 1))
          Graph-var
        )
        #f
      )
    )
  )
)
(define Graph-simple-repeat
  (lambda (Graph-var repetition)
    (cond 
      ((<= repetition 1) Graph-var)
      (Graph-var
        (Graph-simple-append
          (Graph-simple-repeat Graph-var (- repetition 1))
          Graph-var
        )
      )
      (else #f)
    )
  )
)
(define Ring-add-edge
  (lambda (Ring-var edge-var)
    (let
      ((targeted-edge (assoc (car edge-var) (cdr Ring-var))))
      (if targeted-edge
        (if (equal? targeted-edge edge-var)
          Ring-var
          #f
        )
        (append
          Ring-var
          (list edge-var)
        )
      )
    )
  )
)

(define Ring-modify-edge
  (lambda (Ring-var edge-var)
    (append
      (list (car Ring-var))
      (del-assoc (car edge-var) (cdr Ring-var))
      (list edge-var)
    )
  )
)

(define Graph-add-edge
  (lambda (Graph-var ring-num edge-var)
    (if Graph-var
      (if (and
            (>= ring-num 0)
            (< ring-num (length Graph-var))
          )
        (let
          ((targeted-ring (Ring-add-edge (list-ref Graph-var ring-num) edge-var)))
          (if targeted-ring
            (append
              (list-head Graph-var ring-num)
              (list targeted-ring)
              (list-tail Graph-var (+ ring-num 1))
            )
            #f
          )
        )
        #f
      )
      #f
    )
  )
)

(define Graph-modify-edge
  (lambda (Graph-var ring-num edge-var)
    (if Graph-var
      (if (and
            (>= ring-num 0)
            (< ring-num (length Graph-var))
          )
        (append
          (list-head Graph-var ring-num)
          (list (Ring-modify-edge (list-ref Graph-var ring-num) edge-var))
          (list-tail Graph-var (+ ring-num 1))
        )
        #f
      )
      #f
    )
  )
)
(define Ring-clear-edge
  (lambda (Ring-var)
    (list (car Ring-var))
  )
)
(define Graph-clear-edge
  (lambda (Graph-var)
    (if Graph-var
      (map Ring-clear-edge Graph-var)
      #f
    )
  )
)
(define Graph-modify-first-attack-edge
  (lambda (Graph-var edge-offset)
    (if Graph-var
      (let
        ((begin-attack-ring-num (adjacent-attack Graph-var 0 1)))
        (if begin-attack-ring-num
          (Graph-modify-edge Graph-var begin-attack-ring-num (list (Graph-length Graph-var) edge-offset))
          Graph-var
        )
      )
      #f
    )
  )
)
(define Graph-modify-last-attack-edge
  (lambda (Graph-var edge-offset)
    (if Graph-var
      (let
        ((end-attack-ring-num (adjacent-attack Graph-var (- (length Graph-var) 1) -1)))
        (if end-attack-ring-num
          (Graph-modify-edge Graph-var end-attack-ring-num (list 1 edge-offset))
          Graph-var
        )
      )
      #f
    )
  )
)

(define Graph-length
  (lambda (Graph-var)
    (if Graph-var
      (let
        (
          (begin-ring-index (adjacent-known Graph-var 0 1))
          (end-ring-index (adjacent-known Graph-var (- (length Graph-var) 1) -1))
        )
        (if (and begin-ring-index end-ring-index)
          (+ end-ring-index (* -1 begin-ring-index) 1)
          #f
        )
      )
      #f
    )
  )
)
(define Graph-random-create-repetition
  (lambda (Graph-length-var)
    (let
      ((Graph-var-with-external-edge
        (cond
          (
            (= Graph-length-var 8)
            (even-cond ; 1 * 2
              (G* (Gml (Graph-random-create 4) -1) 2)
              (G* (Gml (Graph-random-create 4) +1) 2)
              (G* (Gmf (Graph-random-create 4) -1) 2)
              (G* (Gmf (Graph-random-create 4) +1) 2)
              (G* (Gmf (Graph-random-create 4)  0) 2)
            )
          )
          (
            (= Graph-length-var 12)
            (even-cond ; 1 * 3
              (G* (Gml (Graph-random-create 4) -1) 3)
              (G* (Gml (Graph-random-create 4) +1) 3)
              (G* (Gmf (Graph-random-create 4) -1) 3)
              (G* (Gmf (Graph-random-create 4) +1) 3)
              (G* (Gmf (Graph-random-create 4)  0) 3)
            )
          )
          (
            (= Graph-length-var 16)
            (even-cond ; 2 * 2
              (G* (Gml (Graph-random-create 8) -1) 2)
              (G* (Gml (Graph-random-create 8) +1) 2)
              (G* (Gmf (Graph-random-create 8) -1) 2)
              (G* (Gmf (Graph-random-create 8) +1) 2)
              (G* (Gmf (Graph-random-create 8)  0) 2)
            )
          )
          (
            (= Graph-length-var 24)
            (even-cond ; 2 * 3
              (G* (Gml (Graph-random-create 8) -1) 3)
              (G* (Gml (Graph-random-create 8) +1) 3)
              (G* (Gmf (Graph-random-create 8) -1) 3)
              (G* (Gmf (Graph-random-create 8) +1) 3)
              (G* (Gmf (Graph-random-create 8)  0) 3)
            )
          )
          ( else #f )
        )
      ))
      (if Graph-var-with-external-edge
        (Gdx Graph-var-with-external-edge)
        #f
      )
    )
  )
)

;----------------------------------------
; GMS-related functions

(define Graph-Meta-Structure->Graph-Structure
  (lambda (GMS-var)
    (replace-symbol (car GMS-var) (map (lambda (x) (list (car x) (caddr x))) (cadr GMS-var)))
  )
)

(define var-symbol
  (lambda (num)
    (string->symbol (string-append "var" (number->string num)))
  )
)

(define Graph-Meta-Structure-var-length
  (lambda (GMS-var)
    (if GMS-var
      (length (cadr GMS-var))
      #f
    )
  )
)

(define Glue-create-offset
  (lambda (initial-var-num)
    (zipf-cond
      (even-cond
        `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) glu-ofst -1)))
        `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) glu-ofst +1)))
      )
      (even-cond
        `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) glu-ofst -2)))
        `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) glu-ofst +2)))
      )
    )
  )
)

(define Glue-type-create
  (lambda (initial-var-num)
    (even-cond
      `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) glu-type Gmf)))
      `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) glu-type Gml)))
    )
  )
)

(define Repetition-create-offset
  (lambda (initial-var-num)
    (even-cond
      `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) rep-ofst -1)))
      `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) rep-ofst +1)))
      `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) rep-ofst  0)))
    )
  )
)

(define Subject-begin-note-create
  (lambda (initial-var-num)
    `(,(var-symbol initial-var-num) ((,(var-symbol initial-var-num) subj-begin-note
    ,(zipf-conf "c" "g")
    )))
  )
)

(define Graph-Meta-Structure-random-create-repetition
  (lambda (Graph-length-var)
    (let
      ((Graph-var-with-external-edge
        (cond
          (
            (or 
              (= Graph-length-var 8)
              (= Graph-length-var 16)
              (= Graph-length-var 32)
            )
            (let* 
              (
                (GMS-var (Graph-Meta-Structure-random-create (/ Graph-length-var 2)))
              )
              (GMS* GMS-var 2)
            )
          )
          (
            (or 
              (= Graph-length-var 12)
              (= Graph-length-var 24)
              (= Graph-length-var 48)
            )
            (let* 
              (
                (GMS-var (Graph-Meta-Structure-random-create (/ Graph-length-var 3)))
              )
              (GMS* GMS-var 3)
            )
          )
          (else #f)
        )
      ))
      (if Graph-var-with-external-edge
        (list (list 'Gdx (car Graph-var-with-external-edge)) (cadr Graph-var-with-external-edge))
        #f
      )
    )
  )
)
(define Graph-Meta-Structure-smart-append
  (lambda (GMS-var . GMS-tail)
    (if (null? GMS-tail)
      GMS-var
      (if GMS-var
        (let*
          (
            (first-GMS-var-length (Graph-Meta-Structure-var-length GMS-var))
            (second-GMS-var (car GMS-tail))
            (second-GMS-var-length (Graph-Meta-Structure-var-length second-GMS-var))
            (modulated-second-GMS-var (Graph-Meta-Structure-modulate second-GMS-var (+ first-GMS-var-length 1)))
            (glu-type-1 (Glue-type-create (+ first-GMS-var-length second-GMS-var-length 1)))
            (glu-ofst-1 (Glue-create-offset (+ first-GMS-var-length second-GMS-var-length 2)))
          )
          (apply Graph-Meta-Structure-smart-append
            (cons
              (list
                (list 'G+ (list (car glu-type-1) (car GMS-var) (car glu-ofst-1) ) (car modulated-second-GMS-var))
                (append
                  (cadr GMS-var)
                  (cadr modulated-second-GMS-var)
                  (cadr glu-type-1)
                  (cadr glu-ofst-1)
                )
              )
              (cdr GMS-tail)
            )
          )
        )
        #f
      )
    )
  )
)
(define Graph-Meta-Structure-append
  (lambda (GMS-var . GMS-tail)
    (if (null? GMS-tail)
      GMS-var
      (if GMS-var
        (let*
          (
            (first-GMS-var-length (Graph-Meta-Structure-var-length GMS-var))
            (second-GMS-var (car GMS-tail))
          )
          (apply Graph-Meta-Structure-append            
            (cons
              (list
                (list 'G+ (list (var-symbol (+ first-GMS-var-length 1)) (car GMS-var) (var-symbol (+ first-GMS-var-length 2)) ) (car second-GMS-var))
                (append
                  (cadr GMS-var)
                  (list (list (var-symbol (+ first-GMS-var-length 1)) 'glu-type 'Gml))
                  (list (list (var-symbol (+ first-GMS-var-length 2)) 'glu-ofst 1))
                )
              )
              (cdr GMS-tail)
            )
          )
        )
        #f
      )
    )
  )
)
(define Graph-Meta-Structure-repeat
  (lambda (GMS-var repetition)
    (if GMS-var
      (let*
        (
          (GMS-var-length (Graph-Meta-Structure-var-length GMS-var))
          (glu-type-1 (Glue-type-create (+ GMS-var-length 1)))
          (rep-ofst-1 (Repetition-create-offset (+ GMS-var-length 2)))
        )
        (list
          (list 'G* (list (car glu-type-1) (car GMS-var) (car rep-ofst-1)) repetition)
          (append (cadr GMS-var) (cadr glu-type-1) (cadr rep-ofst-1))
        )
      )
      #f
    )
  )
)
(define Graph-Meta-Structure-random-create
  (lambda (Graph-length-var)
    (let 
      ((Graph-with-external-edge
        (cond
          (
            (= Graph-length-var 4)
            (list (var-symbol 1) (list (list (var-symbol 1) 'atm4 (apply zipf-cond atm4-list))))
          )
          (
            (= Graph-length-var 8)
            (zipf-cond
              (GMS+ 
                (Graph-Meta-Structure-random-create 4)
                (Graph-Meta-Structure-random-create 4)
              )
              (Graph-Meta-Structure-random-create-repetition 8)
            )
          )
          (
            (= Graph-length-var 16)
            (zipf-cond
              (Graph-Meta-Structure-random-create-repetition 16) ; 2 * 2
              (let ; A B A C
                ((A-segment (Graph-Meta-Structure-random-create 4))
                (B-segment (Graph-Meta-Structure-random-create 4))
                (C-segment (Graph-Meta-Structure-random-create 4)))
                (GMS+ (GMS_+ (GMS+ A-segment B-segment) A-segment) C-segment)
              )
              (let ; 1 * 3 +1
                (
                  (atm12-1 (Graph-Meta-Structure-random-create-repetition 12))
                  (atm4-1 (Graph-Meta-Structure-random-create 4))
                )
                (GMS+ atm12-1 atm4-1)
              )
              (let ; 1 + 1 * 3
                (
                  (atm4-1 (Graph-Meta-Structure-random-create 4))
                  (atm12-1 (Graph-Meta-Structure-random-create-repetition 12))
                )
                (GMS+ atm4-1 atm12-1)
              )
              (let ; 1 * 2 + 1 * 2
                (
                  (atm8-1 (Graph-Meta-Structure-random-create-repetition 8))
                  (atm8-2 (Graph-Meta-Structure-random-create-repetition 8 ))
                )
                (GMS+ atm8-1 atm8-2)
              )
              (let ; 1 * 2 + 1 + 1
                (
                  (atm8-1 (Graph-Meta-Structure-random-create-repetition 8))
                  (atm4-1 (Graph-Meta-Structure-random-create 4))
                  (atm4-2 (Graph-Meta-Structure-random-create 4))
                )
                (GMS+ atm8-1 atm4-1 atm4-2)
              )
            )
          )
          (
            (= Graph-length-var 32)
            (zipf-cond
              (let ; A B A C
                ((A-segment (Graph-Meta-Structure-random-create 8))
                (B-segment (Graph-Meta-Structure-random-create 8))
                (C-segment (Graph-Meta-Structure-random-create 8)))
                (GMS+ (GMS_+ (GMS+ A-segment B-segment) A-segment) C-segment)
              )
              (let ; 2 * 2 + 2 + 2
                (
                  (atm16-1 (Graph-Meta-Structure-random-create-repetition 16))
                  (atm8-1 (Graph-Meta-Structure-random-create 8))
                  (atm8-2 (Graph-Meta-Structure-random-create 8))
                )
                (GMS+ atm16-1 atm8-1 atm8-2)
              )
              (let ; 2 * 3 + 2
                (
                  (atm24-1 (Graph-Meta-Structure-random-create-repetition 24))
                  (atm8-1 (Graph-Meta-Structure-random-create 8))
                )
                (GMS+ atm24-1 atm8-1)
              )
              (let ; 2 + 2 + 2 * 2
                (
                  (atm16-1 (Graph-Meta-Structure-random-create-repetition 16))
                  (atm8-1 (Graph-Meta-Structure-random-create 8))
                  (atm8-2 (Graph-Meta-Structure-random-create 8))
                )
                (GMS+ atm8-1 atm8-2 atm16-1)
              )
              (let ; 2 * 2 + 2 * 2
                (
                  (atm16-1 (Graph-Meta-Structure-random-create-repetition 16))
                  (atm16-2 (Graph-Meta-Structure-random-create-repetition 16))
                )
                (GMS+ atm16-1 atm16-2)
              )
              (Graph-Meta-Structure-random-create-repetition 32) ; 4 * 2
              (let ; 2 + 2 * 3
                (
                  (atm8-1 (Graph-Meta-Structure-random-create 8))
                  (atm24-1 (Graph-Meta-Structure-random-create-repetition 24))
                )
                (GMS+ atm8-1 atm24-1)
              )
            )
          )
          (else #f)
        )
      ))
      (if Graph-with-external-edge
        `((Gdx ,(car Graph-with-external-edge)) ,(cadr Graph-with-external-edge))
        #f
      )
    )
  )
)

(define Graph-Meta-Structure-special-random-create
  (lambda (GMS-var Graph-symbol var-type var-value)
    (let ((existing-var-num (length (cadr GMS-var))))
      (list
        (list Graph-symbol (car GMS-var) (var-symbol (+ existing-var-num 1)))
        (append
          (cadr GMS-var)
          (list (list
            (var-symbol (+ existing-var-num 1))
            var-type
            var-value)))
      )
    )
  )
)
 
(define Graph-Meta-Structure-subject-first-assign-random-create
  (lambda (GMS-var)
    (Graph-Meta-Structure-special-random-create GMS-var 'Gf= 'subj-first-note (zipf-cond "c" "g"))
  )
)
(define Graph-Meta-Structure-first-assign-random-create
  (lambda (GMS-var voice-number)
    (cond
      ((= voice-number 0)
        (Graph-Meta-Structure-special-random-create 
          GMS-var 
          'Gf= 
          '("CC" "DD" "EE" "FF" "GG" "AA" "BB"
            "C"  "D"  "E"  "F"  "G"  "A"  "B" "c")
          (apply even-cond
            '("CC" "DD" "EE" "FF" "GG" "AA" "BB"
              "C"  "D"  "E"  "F"  "G"  "A"  "B" "c")
          )
        )
      )
      ((= voice-number 1)
        (Graph-Meta-Structure-special-random-create 
          GMS-var 
          'Gf= 
          '("C"  "D"  "E"  "F"  "G"  "A"  "B"
            "c"  "d"  "e"  "f"  "g"  "a"  "b")
          (apply even-cond
            '("C"  "D"  "E"  "F"  "G"  "A"  "B"
              "c"  "d"  "e"  "f"  "g"  "a"  "b")
          )
        )
      )
      ((= voice-number 2)
        (Graph-Meta-Structure-special-random-create 
          GMS-var 
          'Gf= 
          '("c"  "d"  "e"  "f"  "g"  "a"  "b"
            "cc" "dd" "ee" "ff" "gg" "aa" "bb")
          (apply even-cond
            '("c"  "d"  "e"  "f"  "g"  "a"  "b"
              "cc" "dd" "ee" "ff" "gg" "aa" "bb")
          )
        )
      )
      (else #f)
    )
  )
)


(define Graph-Meta-Structure-modulate
  (lambda (GMS-var new-initial-var-num)
    (if (null? (cadr GMS-var))
      GMS-var
      (let (
          (var-length (length (cadr GMS-var))))
        (let* (
            (first-var-string (symbol->string (caaadr GMS-var)))
            (first-var-string-length (string-length first-var-string))
            (old-initial-var-num (string->number (substring first-var-string 3 first-var-string-length)))
            (initial-offset (- new-initial-var-num old-initial-var-num))
            (replace-symbol-list 
              (map 
                (lambda (current-old-var-num)
                  (list (string->symbol (string-append "var" (number->string current-old-var-num)))
                        (string->symbol (string-append "var" (number->string (+ current-old-var-num initial-offset))))
                  )
                )
                (sequence old-initial-var-num (+ old-initial-var-num var-length))
              )
            ))
          (replace-symbol GMS-var replace-symbol-list)
        )
      )
    )
  )
)
    
(define Graph-Meta-Structure-mutate-atm4 
  (lambda (GMS-var selected-mutate-var background-info)
    (let 
      ((new-atm4-var
        (apply zipf-cond atm4-list)
      ))
      (replace-list GMS-var `((,selected-mutate-var (,(car selected-mutate-var) ,(cadr selected-mutate-var) ,new-atm4-var))))
    )
  )
)
(define Graph-Meta-Structure-mutate-glu-ofst
  (lambda (GMS-var selected-mutate-var background-info)
    (let 
      ((new-glu-ofst-var
        (zipf-cond
          (even-cond 1 -1)
          (even-cond 1 -1)
          (even-cond 1 -1)
          (zipf-cond 0 +2 -2 +3 -3)
        )
      ))
      (replace-list GMS-var `((,selected-mutate-var (,(car selected-mutate-var) ,(cadr selected-mutate-var) ,new-glu-ofst-var))))
    )
  )
)

(define Graph-Meta-Structure-mutate-rep-ofst
  (lambda (GMS-var selected-mutate-var background-info)
    (let 
      ((new-rep-ofst-var
        (zipf-cond
          (even-cond 1 -1)
          (even-cond 1 -1 0)
          (even-cond 1 -1 0)
          (even-cond 0 +2 -2)
        )
      ))
      (replace-list GMS-var `((,selected-mutate-var (,(car selected-mutate-var) ,(cadr selected-mutate-var) ,new-rep-ofst-var))))
    )
  )
)

(define Graph-Meta-Structure-mutate-glu-type
  (lambda (GMS-var selected-mutate-var background-info)
    (let 
      ((new-glu-type-var
        (even-cond 'Gml
          (even-cond 'Gml
            (even-cond 'Gml
              (even-cond 'Gml 'Gmf))))
      ))
      (replace-list GMS-var `((,selected-mutate-var (,(car selected-mutate-var) ,(cadr selected-mutate-var) ,new-glu-type-var))))
    )
  )
)

(define Graph-Meta-Structure-mutate-subj-first-note
  (lambda (GMS-var selected-mutate-var background-info)
    (let 
      ((new-subj-first-note-var
        (even-cond "c" "g")
      ))
      (replace-list GMS-var `((,selected-mutate-var (,(car selected-mutate-var) ,(cadr selected-mutate-var) ,new-subj-first-note-var))))
    )
  )
)
(define Graph-Meta-Structure-mutate
  (lambda (GMS-var background-info)
    (if GMS-var
      (let 
        ((var-length (Graph-Meta-Structure-var-length GMS-var)))
        (if (> var-length 0)
          (let
            ((selected-mutate-var
              (apply even-cond
                (map (lambda (x)
                    (list-ref (cadr GMS-var) x)
                  ) 
                  (sequence 0 (- var-length 1))
                )
              )
            ))
            (cond
              ((list? (cadr selected-mutate-var))
                (replace-list GMS-var `((,selected-mutate-var (,(car selected-mutate-var) ,(cadr selected-mutate-var) ,(apply even-cond (cadr selected-mutate-var))))))
              )
              ((procedure? (cadr selected-mutate-var))
                (let 
                  ((new-var
                    (cadr selected-mutate-var)
                  ))
                  (replace-list GMS-var `((,selected-mutate-var (,(car selected-mutate-var) ,(cadr selected-mutate-var) ,new-var))))
                )
              )
              ((equal? (cadr selected-mutate-var) 'atm4)
                (Graph-Meta-Structure-mutate-atm4 GMS-var selected-mutate-var background-info))
              ((equal? (cadr selected-mutate-var) 'glu-ofst)
                (Graph-Meta-Structure-mutate-glu-ofst GMS-var selected-mutate-var background-info))
              ((equal? (cadr selected-mutate-var) 'rep-ofst)
                (Graph-Meta-Structure-mutate-rep-ofst GMS-var selected-mutate-var background-info))
              ((equal? (cadr selected-mutate-var) 'glu-type)
                (Graph-Meta-Structure-mutate-glu-type GMS-var selected-mutate-var background-info))
              ((equal? (cadr selected-mutate-var) 'subj-first-note)
                (Graph-Meta-Structure-mutate-subj-first-note GMS-var selected-mutate-var background-info))
              (else GMS-var)
            )
          )
          GMS-var
        )
      )
    )
  )
)
(define Graph-Meta-Structure-bundle-mutate
  (lambda (GMS-bundle-var background-info)
    (let*
      ((GMS-bundle-var-num
        (map
          Graph-Meta-Structure-var-length
          GMS-bundle-var
        )
      )
      (sum-var (apply + GMS-bundle-var-num))
      (GMS-bundle-var-aggregate
        (aggregate-list 0 GMS-bundle-var-num)
      )
      (modified-voice
        (cond
          ((< (random sum-var) (list-ref GMS-bundle-var-aggregate 0)) 0)
          ((< (random sum-var) (list-ref GMS-bundle-var-aggregate 1)) 1)
          (else 2)
        )
      ))
      (map
        (lambda (voice-index)
          (if (= modified-voice voice-index)
            (Graph-Meta-Structure-mutate (list-ref GMS-bundle-var voice-index) background-info)
            (list-ref GMS-bundle-var voice-index)
          )
        )
        '(0 1 2)
      )
    )
  )
)


(define Graph-Meta-Structure-mutate-frequency
  (lambda (GMS-var frequency background-info)
    (if GMS-var
      (if (> frequency 0)
        (Graph-Meta-Structure-mutate-frequency (Graph-Meta-Structure-mutate GMS-var background-info) (- frequency 1) background-info)
        GMS-var
      )
      #f
    )
  )
)

(define Graph-Meta-Structure-mutate-portion
  (lambda (GMS-var mutate-rate background-info)
    (Graph-Meta-Structure-mutate-frequency (floor->exact (* (Graph-Meta-Structure-var-length GMS-var) mutate-rate)) background-info)
  )
)

(define Graph-Meta-Structure-bundle-mutate-frequency 
  (lambda (GMS-bundle-var frequency background-info)
    (if GMS-bundle-var
      (if (> frequency 0)
        (Graph-Meta-Structure-bundle-mutate-frequency (Graph-Meta-Structure-bundle-mutate GMS-bundle-var background-info) (- frequency 1) background-info)
        GMS-bundle-var
      )
      #f
    )
  )
)

(define Graph-Meta-Structure-bundle-mutate-portion
  (lambda (GMS-bundle-var mutate-rate background-info)
    (Graph-Meta-Structure-bundle-mutate-frequency 
      (floor->exact 
        (* 
          (+
            (Graph-Meta-Structure-var-length (list-ref GMS-bundle-var 0))
            (Graph-Meta-Structure-var-length (list-ref GMS-bundle-var 1))
            (Graph-Meta-Structure-var-length (list-ref GMS-bundle-var 2))
          ) 
          mutate-rate
        )
      ) 
      background-info
    )
  )
)
;----------------------------------------
; Evaluator
(define Graph-Evaluator-no-7?
  (lambda (Graph-var)
    (fold-left ; no-7-degree bonus
      (lambda (no-7-bonus element)
        (if 
          (and
            (note? (car element))
            (= (Note-degree-to-integer (Note-degree (car element))) 6)
          )
          #f
          no-7-bonus
        )
      )
      #t
      Graph-var
    )
  )
)
(define Graph-note-length
  (lambda (Graph-var Ring-index)
    (if Graph-var
      (if (< Ring-index (length Graph-var))
        (if (or 
              (= (+ Ring-index 1) (length Graph-var))
              (general-attack? (car (list-ref Graph-var (+ Ring-index 1))))
            )
            1
            (+ 1 (Graph-note-length Graph-var (+ Ring-index 1)))
        )
        0
      )
      #f
    )
  )
)

(define mini-metpos
  (lambda (Graph-var-length)
    (cond
      ((= Graph-var-length 1)  '(1))
      ((= Graph-var-length 2)  '(2 1))
      ((= Graph-var-length 4)  '(3 1 2 1))
      ((= Graph-var-length 8)  '(4 1 2 1 3 1 2 1))
      ((= Graph-var-length 16) '(5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1))
      ((= Graph-var-length 32) '(6 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 5 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1))
      (else #f)
    )
  )
)
(define sub-Graph-ditto
  (lambda (Graph-var last-Vertex)
    (if (null? Graph-var) '()
      (if (rest? (caar Graph-var))
        (cons (list last-Vertex) (sub-Graph-ditto (cdr Graph-var) last-Vertex))
        (cons (car Graph-var) (sub-Graph-ditto (cdr Graph-var) (caar Graph-var)))
      )
    )
  )
)
(define Graph-ditto
  (lambda (Graph-var)
    (sub-Graph-ditto Graph-var 'r)
  )
)

(define Graph-evaluator-single-voice  ;Deprecated
  (lambda (Graph-var background-info show-score)
    (let*
      (
        (last-Graph-var (cadr (assoc 'last-Graph background-info)))
        (current-voice (cadr (assoc 'current-voice background-info))) ;NEW
        (adhoc-var 
          (begin 
            (if show-score
              (begin
                (display "Voice ")
                (display current-voice)
                (display ":")
                (newline)
                #f
              )
              #f
            )))
        (current-last-attack-index
          (last-attack
            Graph-var
            (length Graph-var)
          )
        )
        (last-last-attack-index
          (last-attack
            last-Graph-var
            (length last-Graph-var)
          )
        )
        (attack-num
          (fold-left 
            (lambda (sum element)
              (if (note? (car element)) (+ sum 1) sum)
            )
            0
            Graph-var
          )
        )
        (extreme-Note
          (fold-left
            (lambda (extreme-Note-list element)
              (if 
                (note? (car element))
                (list
                  (if 
                    (< (Note-to-integer (car element)) (car extreme-Note-list))
                    (Note-to-integer (car element))
                    (car extreme-Note-list)
                  )
                  (if
                    (> (Note-to-integer (car element)) (cadr extreme-Note-list))
                    (Note-to-integer (car element))
                    (cadr extreme-Note-list)
                  )
                )
                extreme-Note-list
              )
            )
            '(100 0)
            Graph-var
          )
        )
        (Chord-score
          ;----------
          ; Chord
          ; 1. Split into four segments
          (let
            ((Graph-segments 
              (list
                (sublist Graph-var 0 8)
                (sublist Graph-var 8 16)
                (sublist Graph-var 16 24)
                (sublist Graph-var 24 32)
              ))
            (degree-score-lists
              (cdadr (assoc 'current-DKSP background-info))
            ))
            (/ 
              (fold-left
                (lambda (sub-score element)
                  (+ sub-score
                    (Graph-Evaluator-highlight-wrapper
                      (list-ref Graph-segments element)
                      ;degree-score
                      (car (list-ref degree-score-lists element))
                    )
                  )
                )
                0
                '(0 1 2 3)
              )
              4. ; For average
            )
          )
        )
        (adhoc-var 
          (begin 
            (if show-score
              (begin
                (display "Chord-score: ")
                (display Chord-score)
                (newline)
                #f
              )
              #f
            )))
        (Pitch-span-score
          ; Pitch-span
          (if  (>= Chord-score 0.25)
            (if (or
                  (= (cadr extreme-Note) 0) 
                  (= (car extreme-Note) 100)
                  (> 6  (- (cadr extreme-Note) (car extreme-Note)))
                  (< 10 (- (cadr extreme-Note) (car extreme-Note)))
                )
                0 1)
            0
          )
        )
        (adhoc-var 
          (begin 
            (if show-score
              (begin
                (display "Pitch-span-score: ")
                (display Pitch-span-score)
                (newline)
                #f
              )
              #f
            )))
        (Linkage-score
          ; Linkage between two segments
          (if (= Pitch-span-score 1)
            (cond 
              ((not current-last-attack-index) 0)
              ((not last-last-attack-index) 0)
              (else
                (let
                  ((current-last-attack (car (list-ref Graph-var current-last-attack-index)))
                  (last-last-attack (car (list-ref last-Graph-var last-last-attack-index)))
                  )
                  (cond 
                    ((attack? current-last-attack) -1)
                    ((attack? last-last-attack) -1)
                    (else
                      (let
                        ((note-difference
                          (abs 
                            (- (Note-to-integer current-last-attack)
                              (Note-to-integer last-last-attack)))
                        ))
                        (cond 
                          ((= 0 note-difference) 0.5)
                          ((= 1 note-difference) 1)
                          ((<= note-difference 4) 0.75)
                          ((<= note-difference 7) 0.5)
                          ((<= note-difference 11) 0.25)
                          (else 0)
                        )
                      )
                    )
                  )
                )
              )
            )
            0
          )
        )
        (adhoc-var 
          (begin 
            (if show-score
              (begin
                (display "Linkage-score: ")
                (display Linkage-score)
                (newline)
                #f
              )
              #f
            )))
        (Density-score
          ; Density
          (if (>= Linkage-score 0.5)
            (cond
              ((> 0.25 (/ attack-num (length Graph-var))) 0)
              ((> 0.5 (/ attack-num (length Graph-var))) 0.5)
              ((> 0.75 (/ attack-num (length Graph-var))) 1)
              (else 0.5)
            )
            0
          )
        )
        (adhoc-var 
          (begin 
            (if show-score
              (begin
                (display "Density-score: ")
                (display Density-score)
                (newline)
                #f
              )
              #f
            )))
      )
      (+
        Chord-score
        Density-score
        Pitch-span-score
        Linkage-score
      )
    )
  )
)
(define Graph-evaluator-single-voice-Chord
  (lambda (Graph-var current-DKSP)
    ;----------
    ; Chord
    ; 1. Split into four segments
    (let
      ((Graph-segments 
        (list
          (sublist Graph-var 0 8)
          (sublist Graph-var 8 16)
          (sublist Graph-var 16 24)
          (sublist Graph-var 24 32)
        ))
      (degree-score-lists (cdr current-DKSP)))
      (/ 
        (fold-left
          (lambda (sub-score element)
            (+ sub-score
              (Graph-Evaluator-highlight-wrapper
                (list-ref Graph-segments element)
                ;degree-score
                (car (list-ref degree-score-lists element))
              )
            )
          )
          0
          '(0 1 2 3)
        )
        4. ; For average
      )
    )
  )
)
(define Graph-evaluator-single-voice-Pitch-Span
  (lambda (extreme-Note)
    (if (or
          (= (cadr extreme-Note) 0) 
          (= (car extreme-Note) 100)
          (> 6  (- (cadr extreme-Note) (car extreme-Note)))
          (< 10 (- (cadr extreme-Note) (car extreme-Note)))
        )
        0 1)
  )
)

(define Graph-evaluator-single-voice-Linkage
  (lambda (Graph-var last-Graph-var)
    (cond 
      ((not current-last-attack-index) 0)
      ((not last-last-attack-index) 0)
      (else
        (let
          ((current-last-attack (car (list-ref Graph-var current-last-attack-index)))
          (last-last-attack (car (list-ref last-Graph-var last-last-attack-index)))
          )
          (cond 
            ((attack? current-last-attack) -1)
            ((attack? last-last-attack) -1)
            (else
              (let
                ((note-difference
                  (abs 
                    (- (Note-to-integer current-last-attack)
                      (Note-to-integer last-last-attack)))
                ))
                (cond 
                  ((= 0 note-difference) 0.5)
                  ((= 1 note-difference) 1)
                  ((<= note-difference 4) 0.75)
                  ((<= note-difference 7) 0.5)
                  ((<= note-difference 11) 0.25)
                  (else 0)
                )
              )
            )
          )
        )
      )
    )
  )
)
(define Graph-evaluator-single-voice-Density
  (lambda (Graph-var attack-num)
    (cond
      ((> 0.25 (/ attack-num (length Graph-var))) 0)
      ((> 0.5 (/ attack-num (length Graph-var))) 0.5)
      ((> 0.75 (/ attack-num (length Graph-var))) 1)
      (else 0.5)
    )
  )
)
(define Graph-evaluator-single-voice-Range
  (lambda (Graph-var voice-number)
  '()
  )
)
(define Graph-evaluator-multiple-voice-Overlap
  (lambda (higher-Graph-var lower-Graph-var)
  '()
  )
)
(define Graph-evaluator-multiple-voice-ultimate
  (lambda (Graph-bundle-var last-Graph-bundle-var background-info show-score)
  '()
  )
)
(define Graph-adapt-key
  (lambda (Graph-var adapt-table)
    (if Graph-var
      (map
        (lambda (x)
          (if (note? (car x))
            (let
              ((accidental-tuple (assoc (Note-degree-to-integer (Note-degree(car x))) adapt-table)))
              (if accidental-tuple
                (cons (Note-quality-append (car x) (cadr accidental-tuple)) (cdr x))
                x
              )
            )
            x
          )
        )
        Graph-var
      )
      #f
    )
  )
)

(define Chord-degree-score-plus
  (lambda (old-Chord-var new-Chord-var)
    (list 
      (map
        (lambda (x)
          (let*
            ((accidental-tuple (assoc x (cadr new-Chord-var)))
             (accidental-var (if accidental-tuple (cadr accidental-tuple) #f)))
            (list x
              (+
                ; new-Chord-var
                (let
                  ( (new-Chord-length (length (car new-Chord-var))))
                  (cond
                    ((and (< 0 new-Chord-length) (= (caar new-Chord-var) x)) 4)
                    ((and (< 1 new-Chord-length) (= (cadar new-Chord-var) x)) 3)
                    ((and (< 2 new-Chord-length) (= (caddar new-Chord-var) x)) 1)
                    ((and (< 3 new-Chord-length) (= (cadddr (car new-Chord-var)) x)) 2)
                    (else 0)
                  )
                )
                ; old-Chord-var
                (if (and old-Chord-var (not (equal? (list-head (car old-Chord-var) 3) (list-head (car new-Chord-var) 3))))     
                  (let*
                    ( (old-accidental-tuple (assoc x (cadr old-Chord-var)))
                      (old-accidental-var (if old-accidental-tuple (cadr old-accidental-tuple) #f))
                      (old-Chord-length (length (car old-Chord-var)))
                    )
                    (cond
                      ((not (eq? accidental-var old-accidental-var)) 0)
                      ((and (< 0 old-Chord-length) (= (caar old-Chord-var) x)) -4)
                      ((and (< 1 old-Chord-length) (= (cadar old-Chord-var) x)) -3)
                      ((and (< 2 old-Chord-length) (= (caddar old-Chord-var) x)) -1)
                      ((and (< 3 old-Chord-length) (= (cadddr (car old-Chord-var)) x)) -2)
                      (else 0)
                    )
                  )
                  0
                )
              )
            )
          )
        )
        (sequence 0 7)
      )
      (cadr new-Chord-var)
    )
  )
)
(define CPRC->DKSP ;Chord Progression Random Create -> Degree Key Signature Progression
  (lambda (cprc-var)
    (map
      (lambda (tonal-key-index)
        (let
          ((tonal-key-section
            (list-ref cprc-var tonal-key-index)
          ))
          (cons
            (car tonal-key-section)
            (map
              (lambda (chord-progression-index)
                (cond
                  ((not (= 1 chord-progression-index))
                    (Chord-degree-score-plus
                      (list-ref tonal-key-section
                        (- chord-progression-index 1))
                      (list-ref tonal-key-section
                        chord-progression-index)
                    ))
                  ((not (= 0 tonal-key-index))
                    (let
                      ((last-tonal-key-section
                        (list-ref cprc-var (- tonal-key-index 1))
                      ))
                      (Chord-degree-score-plus
                        (list-ref last-tonal-key-section (- (length last-tonal-key-section) 1) )
                        (list-ref tonal-key-section 1)
                      )
                    )
                  )
                  (else (Chord-degree-score-plus #f (list-ref tonal-key-section 2)))
                )
              )
              (sequence 1 (- (length tonal-key-section) 1) )
            )
          )
        )
      )
      (sequence 0 (- (length cprc-var) 1))
    )
  )
)

(define Graph-Evaluator-highlight
  (lambda (Graph-var degree-score)
  (let*
      (
        (padding-Graph-var (Graph-ditto Graph-var))
        (shifted-padding-Graph-var
          (cons (list #f) (list-head padding-Graph-var (- (length padding-Graph-var) 1))))
        (metpos (mini-metpos (length Graph-var)))
        (shifted-metpos
          (cons (list #f) (list-head metpos (- (length Graph-var) 1))))
      )
      (fold-left
        (lambda (total-score element)
          (let
            ((Vertex-var (caar element))
             (padding-Vertex-var (caadr element)))
            (cond ((note? Vertex-var)
                    (+ 
                      total-score
                      (* 
                        (cadr (assoc (Note-degree-to-integer (Note-degree Vertex-var)) degree-score))
                        (caddr element)
                      )
                    )
                  )
                  ((rest? Vertex-var)
                    (if (note? padding-Vertex-var)
                      (+ 
                        total-score
                        (* 
                          (cadr (assoc (Note-degree-to-integer (Note-degree padding-Vertex-var)) degree-score))
                          (cadddr element)
                          0.5
                        )
                      )
                      total-score
                    )
                  )
                  (else total-score)
            )
          )
        )
        0
        (map (lambda (x y z w) (list x y z w)) Graph-var shifted-padding-Graph-var metpos shifted-metpos)
      )
    )
  )
)
(define Graph-evaluator-highlight-wrapper   
  (lambda (Graph-var degree-score)
    (let*
      ((raw-score (Graph-evaluator-highlight Graph-var degree-score))
      (sum-of-degree-score
        (fold-left ; no-7-degree bonus
          (lambda (sum-var element)
            (+
              sum-var
              (cadr element)
            )
          )
          0
          degree-score
        )
      )
      (normalized-score (exact->inexact (- (/ raw-score (length Graph-var)) (/ sum-of-degree-score 7))))
      (final-score
        (cond
          ((>= normalized-score 4) 1)
          ((>= normalized-score 2) 0)
          (else -1)
        )
      ))
      final-score
    )
  )
)
      


;----------------------------------------
; Genetic Algorithm related functions

(define general-Genetic-Algorithm
  (lambda (generator-proc mutator-proc evaluator-proc generation-num candidate-num offspring-per-candidate-num background-info)
    (display 'generation-num:)
    (display generation-num)
    (newline)
    (display "Generating Corpus...")
    (newline)
    (let
      ((corpus
        (map (lambda (x) (generator-proc background-info)) (sequence 1 candidate-num))
      ))
      (sub-general-Genetic-Algorithm corpus mutator-proc evaluator-proc 1 generation-num candidate-num offspring-per-candidate-num background-info)
    )
  )
)

(define sub-general-Genetic-Algorithm 
  (lambda (corpus mutator-proc evaluator-proc current-generation generation-num candidate-num offspring-per-candidate-num background-info)
    (if (< current-generation generation-num)
      (let*
        (
          (Just-to-display 
            (begin 
              (display "Generating Candidates...")
              (newline)
            )
          )
          (offsprings
            (with-timings
              (lambda ()
                (apply append
                  (map
                    (lambda (candidate-var)
                      (apply append
                        (map
                          (lambda (x)
                            (list (mutator-proc candidate-var background-info))
                          )
                          (sequence 1 offspring-per-candidate-num)
                        )
                      )
                    )
                    corpus
                  )
                )
              )
              (lambda (run-time gc-time real-time)
                (display "Mutation Time Used: ")
                (write (internal-time/ticks->seconds real-time))
                (display "s")
                (newline)
              )
            )
          )
          (Just-to-display 
            (begin 
              (display "Evaluating Candidates...")
              (newline)
            )
          )
          (score-board
            (with-timings
              (lambda ()
                (apply append
                  (map 
                    (lambda (offspring-var)
                      (list `(,offspring-var ,(evaluator-proc offspring-var background-info #f)))
                    )
                    offsprings
                  )
                )
              )
              (lambda (run-time gc-time real-time)
                (display "Evaluation Time Used: ")
                (write (internal-time/ticks->seconds real-time))
                (display "s")
                (newline)
              )
            )
          )
          (new-corpus-board
            (begin
              (list-head (quick-sort score-board (lambda (first-element second-element) (> (cadr first-element) (cadr second-element)))) candidate-num)
            )
          )
          (new-corpus
            (map car new-corpus-board)
          )
        )
        (display 'Generation:)
        (display current-generation)
        (newline)
        (display (map cadr new-corpus-board))
        (newline)
        ; Show best score details
        (display "Best candidate:")
        (newline)
        (display (evaluator-proc (car new-corpus) background-info #t))
        (newline)
        (sub-general-Genetic-Algorithm new-corpus mutator-proc evaluator-proc (+ current-generation 1) generation-num candidate-num offspring-per-candidate-num background-info)
      )
      corpus
    )
  )
)
(define subject-generator
  (lambda (background-info)
    (general-Genetic-Algorithm
      (lambda (bk-info);generator-proc 
        (Graph-Meta-Structure-subject-first-assign-random-create (Graph-Meta-Structure-random-create 32))
      )
      (lambda (GMS-var bk-info);mutator-proc 
        (Graph-Meta-Structure-mutate-frequency GMS-var 5 bk-info)
      )
      
      (lambda (GMS-var bk-info show-score);evaluator-proc
        (let*
          (
            (Graph-var
              (eval! (GMS->GS GMS-var))
            )
            (current-last-attack-index
              (last-attack
                Graph-var
                (length Graph-var)
              )
            )
            (attack-num
              (fold-left 
                (lambda (sum element)
                  (if (note? (car element)) (+ sum 1) sum)
                )
                0
                Graph-var
              )
            )
            (extreme-Note
              (fold-left
                (lambda (extreme-Note-list element)
                  (if 
                    (note? (car element))
                    (list
                      (if 
                        (< (Note-to-integer (car element)) (car extreme-Note-list))
                        (Note-to-integer (car element))
                        (car extreme-Note-list)
                      )
                      (if
                        (> (Note-to-integer (car element)) (cadr extreme-Note-list))
                        (Note-to-integer (car element))
                        (cadr extreme-Note-list)
                      )
                    )
                    extreme-Note-list
                  )
                )
                '(100 0)
                Graph-var
              )
            )
            (No-7-score
              ; No-7 Score
              (if (Graph-Evaluator-no-7? (list-head Graph-var (quotient (length Graph-var) 2))) 1 0))
            (adhoc-var 
              (begin 
                (if show-score
                  (begin
                    (display "No-7-score: ")
                    (display No-7-score)
                    (newline)
                    #f
                  )
                  #f
                )))
            (Chord-score
              ;----------
              ; Chord
              ; 1. Split into four segments
              (let
                ((Graph-segments 
                  (list
                    (sublist Graph-var 0 8)
                    (sublist Graph-var 8 16)
                    (sublist Graph-var 16 24)
                    (sublist Graph-var 24 32)
                  ))
                (degree-score-lists
                  (cdadr (assoc 'current-DKSP background-info))
                ))
                (let*
                  ((basic-Chord-score                    
                    (/ 
                      (fold-left
                        (lambda (sub-score element)
                          (+ sub-score
                            (Graph-Evaluator-highlight-wrapper
                              (list-ref Graph-segments element)
                              ;degree-score
                              (car (list-ref degree-score-lists element))
                            )
                          )
                        )
                        0
                        '(0 1 2 3)
                      )
                      4. ; For average
                    )
                  )
                  (adhoc-var 
                    (begin 
                      (if show-score
                        (begin
                          (display "Basic-Chord-score: ")
                          (display basic-Chord-score)
                          (newline)
                          #f
                        )
                        #f
                      )))
                  (last-attack-bonus
                    (if (>= basic-Chord-score 0.5)
                      ;----------
                      ; Last attack bonus
                      (if current-last-attack-index
                        (let
                          ((current-last-attack (car (list-ref Graph-var current-last-attack-index)))
                          )
                          (cond 
                            ((attack? current-last-attack) -1)
                            (else
                              (/ (cadr (assoc (Note-degree-to-integer (Note-degree current-last-attack)) (car (list-ref degree-score-lists 3)))) 8.)
                            )
                          )
                        )
                        -1
                      )
                      0
                    )
                  )
                  (adhoc-var 
                    (begin 
                      (if show-score
                        (begin
                          (display "last-attack-bonus: ")
                          (display last-attack-bonus)
                          (newline)
                          #f
                        )
                        #f
                      )))
                  )
                  (+ basic-Chord-score last-attack-bonus)
                )
              )
            )
            (Density-score
              ;----------
              ; Predicates

              ; Density
              (cond
                ((> 0.25 (/ attack-num (length Graph-var))) 0)
                ((> 0.5 (/ attack-num (length Graph-var))) 0.5)
                ((> 0.75 (/ attack-num (length Graph-var))) 1)
                (else 0.5)
              )
            )
            (adhoc-var 
              (begin 
                (if show-score
                  (begin
                    (display "Density-score: ")
                    (display Density-score)
                    (newline)
                    #f
                  )
                  #f
                )))
            (Pitch-span-score
              ; Pitch-span
              (if (or
                    (= (cadr extreme-Note) 0) 
                    (= (car extreme-Note) 100)
                    (> 6  (- (cadr extreme-Note) (car extreme-Note)))
                    (< 10 (- (cadr extreme-Note) (car extreme-Note)))
                  )
                  0 1)

            )
            (adhoc-var 
              (begin 
                (if show-score
                  (begin
                    (display "Pitch-span-score: ")
                    (display Pitch-span-score)
                    (newline)
                    #f
                  )
                  #f
                )))
          )
          (+
            Chord-score
            Density-score
            Pitch-span-score
            No-7-score
          )
        )
      )

      subject-generation-num
      subject-candidate-num
      subject-offspring-per-candidate-num
      background-info
    )
  )
)
(define GLOBAL-subject-bundle-generator
  (lambda (subject)
    `(
      (
        '( 
          (r) (r) (r) (r) (r) (r) (r) (r)
          (r) (r) (r) (r) (r) (r) (r) (r)
          (r) (r) (r) (r) (r) (r) (r) (r)
          (r) (r) (r) (r) (r) (r) (r) (r)
        )
        ()
      )
      ,subject
      (
        '( 
          (r) (r) (r) (r) (r) (r) (r) (r)
          (r) (r) (r) (r) (r) (r) (r) (r)
          (r) (r) (r) (r) (r) (r) (r) (r)
          (r) (r) (r) (r) (r) (r) (r) (r)
        )
        ()
      )
    )
  )
)
(define subject-generator-wrapper
  (lambda (background-info)
    (let
      ((original-subject-GMS
        (list-ref (subject-generator background-info) 0))
      )
      (list
        (list 'Gf=
          (GMS->GS (list (cadar original-subject-GMS) (list-head (cadr original-subject-GMS) (- (length (cadr original-subject-GMS)) 1)))) 'VAR1)
        (list (list 'VAR1 'fixed-subj-first-note (caddr (list-ref (cadr original-subject-GMS) (- (length (cadr original-subject-GMS)) 1)))))
      ) 
    )
  )
)

(define subject-modulate
  (lambda (G-Subject transposition-pitch-class voice-number)
    (let*
      ((original-first-note
         (caddr (caadr G-subject))
      )
      (new-first-note
        (cond
          ((equal? original-first-note "c")
            (cond
              ((equal? transposition-pitch-class "C")
                (integer-to-Note (+ (* voice-number 7) 21)))
              ((equal? transposition-pitch-class "G")
                (integer-to-Note (+ (* voice-number 7) 18)))
              ((equal? transposition-pitch-class "Dm")
                (integer-to-Note (+ (* voice-number 7) 22)))
              ((equal? transposition-pitch-class "Am")
                (integer-to-Note (+ (* voice-number 7) 19)))
              ((equal? transposition-pitch-class "Em")
                (integer-to-Note (+ (* voice-number 7) 23)))
              (else 
                (integer-to-Note (+ (* voice-number 7) 21)))
            )
          )
          ((equal? original-first-note "g")
            (cond
              ((equal? transposition-pitch-class "C")
                (integer-to-Note (+ (* voice-number 7) 18)))
              ((equal? transposition-pitch-class "G")
                (integer-to-Note (+ (* voice-number 7) 22)))
              ((equal? transposition-pitch-class "Dm")
                (integer-to-Note (+ (* voice-number 7) 19)))
              ((equal? transposition-pitch-class "Am")
                (integer-to-Note (+ (* voice-number 7) 23)))
              ((equal? transposition-pitch-class "Em")
                (integer-to-Note (+ (* voice-number 7) 20)))
              (else 
                (integer-to-Note (+ (* voice-number 7) 21)))
            )
          )
          (else
            (integer-to-Note (+ (* voice-number 7) 21)))
        )
      ))
      (list 
        (car G-subject)
        (list
          (list
            'VAR1 'ignored new-first-note
          )
        )
      )
    )
  )
)
                

(define second-bundle-generator
  (lambda (background-info)
    (let*
      ((last-bundle (cadr (assoc 'last-bundle background-info)))
      (last-Graph-bundle
        (map
          (lambda (GMS-var)
            (eval! (GMS->GS GMS-var))
          )
          last-bundle
        )
      )
      (global-DKSP (cadr (assoc 'global-DKSP background-info)))
      (global-subject (cadr (assoc 'global-subject background-info))))
      (begin

        (newline)
        (display "Segments:")
        (display 1)
        (display '/)
        (display (- (length global-DKSP) 1))
        (newline)
        (newline)

        (general-Genetic-Algorithm
          (lambda (bk-info);generator-proc
            ; Determined position of the voice containing first subject
            (let*
              ((voice-of-last-subject (caddar (cadr (assoc 'last-DKSP bk-info))))
              (voice-of-this-answer (caddar (cadr (assoc 'current-DKSP bk-info))))
              (GLOBAL-subject (cadr (assoc 'GLOBAL-subject bk-info)))
              (this-answer (subject-modulate (cadr (assoc 'GLOBAL-subject bk-info)) (cadar (cadr (assoc 'current-DKSP bk-info))) voice-of-this-answer));?????
              (voice-of-free-rest-segment
                (cond 
                  ((not (or (= 0 voice-of-last-subject) (= 0 voice-of-this-answer))) 0)
                  ((not (or (= 1 voice-of-last-subject) (= 1 voice-of-this-answer))) 1)
                  (else 2)
                )
              )
              (bundle-assoc-list
                `(
                  (,voice-of-last-subject
                    ,(Graph-Meta-Structure-first-assign-random-create (Graph-Meta-Structure-random-create 32) voice-of-last-subject)
                  )
                  (,voice-of-this-answer ,this-answer)
                  (,voice-of-free-rest-segment
                    (
                      '( 
                        (r) (r) (r) (r) (r) (r) (r) (r)
                        (r) (r) (r) (r) (r) (r) (r) (r)
                        (r) (r) (r) (r) (r) (r) (r) (r)
                        (r) (r) (r) (r) (r) (r) (r) (r)
                      )
                      ()
                    )
                  )
                )
              ))
              (list
                (cadr (assoc 0 bundle-assoc-list))
                (cadr (assoc 1 bundle-assoc-list))
                (cadr (assoc 2 bundle-assoc-list))
              )
            )
          )
          (lambda (GMS-bundle-var bk-info);mutator-proc 
            (Graph-Meta-Structure-bundle-mutate-frequency GMS-bundle-var 2 bk-info)
          )
          
          (lambda (GMS-bundle-var bk-info show-score);evaluator-proc
            (if #f ;By-passer
              1
              (let*
                (
                  (Graph-bundle-var
                    (list
                      (eval! (GMS->GS (list-ref GMS-bundle-var 0)))
                      (eval! (GMS->GS (list-ref GMS-bundle-var 1)))
                      (eval! (GMS->GS (list-ref GMS-bundle-var 2)))
                    )
                  )
                )
                (if #f ; By-passer
                  1
                  (+
                    (fold-left
                      (lambda (sum-var x)
                        (+
                          sum-var
                          (Graph-evaluator-single-voice 
                            (list-ref Graph-bundle-var x) 
                            (append 
                              bk-info
                              (list 
                                (list 'last-graph (list-ref last-Graph-bundle x))
                                (list 'current-voice x)
                              )
                            )
                            show-score
                          )
                        )
                      )
                      0
                      '(0 1 2)
                    )
                    
                    ; Additional evaluators for Multiple Voice
                  )
                )
              )
            )
          )

          middle-generation-num
          middle-candidate-num
          middle-offspring-per-candidate-num
          background-info
        )
      )
    )
  )
)
(define second-bundle-generator-wrapper
  (lambda (background-info)
    (list-ref (second-bundle-generator background-info) 0)
  )
)
    
(define middle-ware-generator
  (lambda (background-info)
    (let
      (
        (current-DKSP (cadr (assoc 'current-DKSP background-info)))
        (last-bundle (cadr (assoc 'last-bundle background-info)))
        (last-Graph-bundle (cadr (assoc 'last-Graph-bundle background-info)))
      )
      (general-Genetic-Algorithm
       
        (lambda (bk-info);generator-proc
          (if (equal? (caar current-DKSP) "subject")
            (let*
              ((subject-voice (caddar current-DKSP))
              (subject-transposition (cadar current-DKSP))
              (this-subject (subject-modulate (cadr (assoc 'GLOBAL-subject bk-info)) subject-transposition subject-voice)))
              (map
                (lambda (x)
                  (if (= x subject-voice)
                    this-subject;TODO
                    (Graph-Meta-Structure-first-assign-random-create (Graph-Meta-Structure-random-create 32) x)
                  )
                )
                '(0 1 2)
              )
            )
            (map
              (lambda (x)
                (Graph-Meta-Structure-first-assign-random-create (Graph-Meta-Structure-random-create 32) x)
              )
              '(0 1 2)
            )
          )
        )

        (lambda (GMS-bundle-var bk-info);mutator-proc 
          (Graph-Meta-Structure-bundle-mutate-frequency GMS-bundle-var 5 bk-info)
        )
      
        (lambda (GMS-bundle-var bk-info show-score);evaluator-proc
          (let*
            (
              (Graph-bundle-var
                (list
                  (eval! (GMS->GS (list-ref GMS-bundle-var 0)))
                  (eval! (GMS->GS (list-ref GMS-bundle-var 1)))
                  (eval! (GMS->GS (list-ref GMS-bundle-var 2)))
                )
              )
            )
            (+
              (fold-left
                (lambda (sum-var x)
                  (+ sum-var
                    (Graph-evaluator-single-voice 
                      (list-ref Graph-bundle-var x) 
                      (append 
                        bk-info
                        (list 
                          (list 'last-Graph (list-ref last-Graph-bundle x))
                          (list 'current-voice x)
                        )
                      )
                      show-score
                    )
                  )
                )
                0
                '(0 1 2)
              )
              ; Additional evaluators for Multiple Voice
            )
            ;(Graph-evaluator-multiple-voice-ultimate Graph-bundle-var last-bundle-var bk-info show-score)
          )
        )
        middle-generation-num
        middle-candidate-num
        middle-offspring-per-candidate-num
        background-info
      )
    )
  )
)
    
(define middle-ware-generator-wrapper 
  (lambda (current-bundle-index background-info)
    (let
      (
        (global-DKSP (cadr (assoc 'global-DKSP background-info)))
        (global-subject (cadr (assoc 'global-subject background-info)))
      )
      (newline)
      (display "Segments:")
      (display current-bundle-index)
      (display '/)
      (display (- (length global-DKSP) 1))
      (newline)
      (newline)
      (if (> current-bundle-index (- (length global-DKSP) 2))
        '()
        (let*
          ((last-bundle (cadr (assoc 'last-bundle background-info)))
            (last-Graph-bundle
              (map
                (lambda (GMS-var)
                  (eval! (GMS->GS GMS-var))
                )
                last-bundle
              )
            )
            (current-bundle
              (list-ref 
                (middle-ware-generator
                  `(
                    (last-bundle ,last-bundle)
                    (last-Graph-bundle ,last-Graph-bundle)
                    (global-DKSP ,Global-DKSP)
                    (current-DKSP ,(list-ref global-DKSP current-bundle-index))
                    (GLOBAL-subject ,GLOBAL-subject)
                  )
                )
                0
              )
            )
          )
          (cons 
            current-bundle 
            (middle-ware-generator-wrapper
              (+ current-bundle-index 1)
              `(
                (last-bundle ,current-bundle)
                (global-DKSP ,Global-DKSP)
                (GLOBAL-subject ,GLOBAL-subject)
              )
            )
          )
        )
      )
    )
  )
)
(define coda-generator
  (lambda (background-info)
    (let*
      (
        (current-DKSP (cadr (assoc 'current-DKSP background-info)))
        (last-bundle (cadr (assoc 'last-bundle background-info)))
        (last-Graph-bundle
          (map
            (lambda (GMS-var)
              (eval! (GMS->GS GMS-var))
            )
            last-bundle
          )
        )
      )
      (general-Genetic-Algorithm
       
        (lambda (bk-info);generator-proc
          (map
            (lambda (x)
              (let
                ((first-16-units
                  (Graph-Meta-Structure-random-create 16))
                (second-16-units
                  '('((k)(r)(r)(r)(r)(r)(r)(r)(r)(r)(r)(r)(r)(r)(r)(r)) ())))
                (Graph-Meta-Structure-first-assign-random-create (GMS+ first-16-units second-16-units) x)
              )
            )
            '(0 1 2)
          )
        )

        (lambda (GMS-bundle-var bk-info);mutator-proc 
          (Graph-Meta-Structure-bundle-mutate-frequency GMS-bundle-var 4 bk-info)
        )
      
        (lambda (GMS-bundle-var bk-info show-score);evaluator-proc
          (let*
            (
              (Graph-bundle-var
                (list
                  (eval! (GMS->GS (list-ref GMS-bundle-var 0)))
                  (eval! (GMS->GS (list-ref GMS-bundle-var 1)))
                  (eval! (GMS->GS (list-ref GMS-bundle-var 2)))
                )
              )
            )
            (+
              (fold-left
                (lambda (sum-var x)
                  (+ sum-var
                    (Graph-evaluator-single-voice 
                      (list-ref Graph-bundle-var x) 
                      (append 
                        bk-info
                        (list 
                          (list 'last-Graph (list-ref last-Graph-bundle x))
                          (list 'current-voice x)
                        )
                      )
                      show-score
                    )
                  )
                )
                0
                '(0 1 2)
              )
              ; Additional evaluators for Multiple Voice
            )
          )
        )
        coda-generation-num
        coda-candidate-num
        coda-offspring-per-candidate-num
        background-info
      )
    )
  )
)
    
(define coda-generator-wrapper
  (lambda (background-info)
    (list-ref (coda-generator background-info) 0)
  )
)
(define GLOBAL-fugue-composer
  (lambda ()
    (let*
      ((RAW-CPRC (cprc))
;      (GLOBAL-CPRC (append (list-head RAW-CPRC 4) (list (list-ref RAW-CPRC (- (length RAW-CPRC) 1))))) ;DEBUG
      (GLOBAL-CPRC RAW-CPRC) ;DEBUG
      (GLOBAL-DKSP (CPRC->DKSP GLOBAL-CPRC)))
        ;Subject
      (let*
        
        ((current-bundle-index 0)
        (GLOBAL-subject-information
          (list-ref GLOBAL-DKSP 0))
        (GLOBAL-subject
          (subject-generator-wrapper
            ;Unified Background Information
            `(
              (global-DKSP ,GLOBAL-DKSP)
              (current-DKSP ,GLOBAL-subject-information)
              (current-index ,current-bundle-index)
            )
          )
        )
        (adhoc-var
          (begin
            (display "Selected Subject:")
            (newline)
            (pp GLOBAL-subject)
            (pp (eval! (GMS->GS GLOBAL-subject)))
            (newline)
            #f
          )
        )
        (GLOBAL-subject-bundle
          (GLOBAL-subject-bundle-generator GLOBAL-subject)
        )
        (last-bundle GLOBAL-subject-bundle)
        (GMS-bundles-result
          (cons
            last-bundle
            (let*
              ((current-bundle-index 1)
              (second-segment-information
                (list-ref GLOBAL-DKSP 1))
              (second-bundle
                (second-bundle-generator-wrapper
                  `(
                    (last-DKSP ,GLOBAL-subject-information)
                    (last-bundle ,last-bundle)
                    (global-DKSP ,GLOBAL-DKSP)
                    (GLOBAL-subject ,GLOBAL-subject)
                    (current-DKSP ,second-segment-information)
                    (current-index ,current-bundle-index)
                  )
                )
              )
              (last-bundle second-bundle))
              (cons
                last-bundle
                (let*
                  ((middle-bundles 
                    (middle-ware-generator-wrapper 
                      (+ current-bundle-index 1)
                      `(
                        (last-bundle ,last-bundle)
                        (global-DKSP ,GLOBAL-DKSP)
                        (GLOBAL-subject ,GLOBAL-subject)
                      )
                    )
                  ))
                  (append
                    middle-bundles
                    (list
                      (let*
                        ((last-bundle
                          (list-ref middle-bundles (- (length middle-bundles) 1))
                        )
                        (coda-bundle
                          (coda-generator-wrapper
                            `(
                              (last-bundle ,last-bundle)
                              (global-DKSP ,GLOBAL-DKSP)
                              (current-DKSP ,(list-ref GLOBAL-DKSP (- (length GLOBAL-DKSP) 1)))
                            )
                          )
                        ))
                        coda-bundle
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (Final-Graph-bundles
          (map
            (lambda (GMS-bundle-var local-DKSP-var)
              (map
                (lambda (GMS-var)
                  (let*
                    (
                      (Graph-var
                        (eval! (GMS->GS GMS-var)))
                    );TODO
                    (let
                      ((Graph-segments 
                        (list
                          (sublist Graph-var 0 8)
                          (sublist Graph-var 8 16)
                          (sublist Graph-var 16 24)
                          (sublist Graph-var 24 32)
                        )))
                      (apply append 
                        (map
                          (lambda (segment accidental-tuples)
                            (Graph-adapt-key segment accidental-tuples)
                          )
                          Graph-segments
                          (map cadr (cdr local-DKSP-var))
                        )
                      )
                    )
                  )
                )
                GMS-bundle-var
              )
            )
            GMS-bundles-result
            GLOBAL-DKSP
          )
        ))
        (pp GLOBAL-CPRC)
        (pp GLOBAL-DKSP)
        `(
          (GLOBAL-CPRC ,GLOBAL-CPRC)
          (GLOBAL-DKSP ,GLOBAL-DKSP)
          (GMS-bundles-result ,GMS-bundles-result)
          (Final-Graph-bundles ,Final-Graph-bundles)
        )
      )
    )
  )
)
(define GLOBAL-fugue-composer-wrapper
  (lambda raw-file-name
    (let*
      (
        (file-name 
          (if (null? raw-file-name) 
            (number->string (get-universal-time)) 
            (if (number? (car raw-file-name))
              (number->string (car raw-file-name))
              (car raw-file-name)
            )
          )
        )
        (fugue-info (GLOBAL-fugue-composer))
        (fugue (cadr (assoc 'Final-Graph-bundles fugue-info)))
        (fugue-kern (Graph-bundles->kern fugue))
        (kern-file-port (open-output-file (string-append "fugues/fugue-" file-name ".krn")))
        (misc-file-port (open-output-file (string-append "fugues/fugue-" file-name ".misc")))
      )
      (begin
        (write-string fugue-kern kern-file-port)
        (close-port kern-file-port)
        (write-string
          (with-output-to-string
            (lambda ()
              (display "CPRC:")
              (newline)
              (newline)
              (pp (cadr (assoc 'GLOBAL-CPRC fugue-info)))
              (newline)
              (display "DKSP:")
              (newline)
              (newline)
              (pp (cadr (assoc 'GLOBAL-DKSP fugue-info)))
              (newline)
              (display "GMS-bundles:")
              (newline)
              (newline)
              (pp (cadr (assoc 'GMS-bundles-result fugue-info)))
            )
          )
          misc-file-port
        )
        (close-port misc-file-port)
      )
    )
  )
)

(define Graph->kern
  (lambda (Graph-var)
    (display "**kern")
    (newline)
    (display "*C:")
    (newline)
    (map
      (lambda (index)
        (if (= (modulo index 16) 0)
          (begin
            (display '=)
            (display (+ (quotient index 16) 1))
            (newline)
          )
          #f
        )
        (display 16)
        (display (car (list-ref Graph-var index)))
        (newline)
      )
      (sequence 0 (- (length Graph-var) 1))
    )
    (display "==")
    (newline)
    (display "*-")
    (newline)
    #f
  )
)
(define GMS-Bundles->Graph-Bundles
  (lambda (GMS-Bundles-var)
    (map
      (lambda (GMS-Bundle)
        (map
          (lambda (GMS-var)
            (eval! (GMS->GS GMS-var))
          )
          GMS-Bundle
        )
      )
      GMS-Bundles-var
    )
  )
)
(define Graph-Bundles->kern
  (lambda (Graph-Bundles-var)
    (with-output-to-string
      (lambda ()
        (display "**kern")
        (write-char #\tab)
        (display "**kern")
        (write-char #\tab)
        (display "**kern")
        (newline)
        (display "*C:")
        (write-char #\tab)
        (display "*C:")
        (write-char #\tab)
        (display "*C:")
        (newline)
        (map
          (lambda (Graph-Bundles-index)
            (map
              (lambda (inside-index)
                (if (= (modulo inside-index 16) 0)
                  (begin
                    (display '=)
                    (display (+ (quotient inside-index 16) (* Graph-Bundles-index 2) 1))
                    (write-char #\tab)
                    (display '=)
                    (display (+ (quotient inside-index 16) (* Graph-Bundles-index 2) 1))
                    (write-char #\tab)
                    (display '=)
                    (display (+ (quotient inside-index 16) (* Graph-Bundles-index 2) 1))
                    (newline)
                  )
                  #f
                )
                (display 16)
                (display (car (list-ref (list-ref (list-ref Graph-Bundles-var Graph-Bundles-index) 0) inside-index)))
                (write-char #\tab)
                (display 16)
                (display (car (list-ref (list-ref (list-ref Graph-Bundles-var Graph-Bundles-index) 1) inside-index)))
                (write-char #\tab)
                (display 16)
                (display (car (list-ref (list-ref (list-ref Graph-Bundles-var Graph-Bundles-index) 2) inside-index)))
                (newline)
              )
              (sequence 0 31)
            )
          )
          (sequence 0 (- (length Graph-Bundles-var) 1))
        )
        (display "==")
        (write-char #\tab)
        (display "==")
        (write-char #\tab)
        (display "==")
        (newline)
        (display "*-")
        (write-char #\tab)
        (display "*-")
        (write-char #\tab)
        (display "*-")
        (newline)
        #f
      )
    )
  )
)

;----------------------------------------
; Aliases

; Graph ops (to build Graph-Structure)
(define G_+ Graph-append)
(define G+ Graph-simple-append)
(define G_* Graph-repeat)
(define G* Graph-simple-repeat)

; Vertex ops
(define G= Graph-assign)
(define Gf= Graph-assign-first-attack)
(define G?=r Graph-fill-rest)
(define Glen Graph-length)

; Edge ops
(define Gc Graph-clear-edge)
(define Ga Graph-add-edge)
(define Gm Graph-modify-edge)
(define Gmf Graph-modify-first-attack-edge)
(define Gml Graph-modify-last-attack-edge)
(define Gdx Graph-delete-external-edge)
(define Gdxmf Graph-delete-external-edge-modify-first-attack-edge)
(define Gdxml Graph-delete-external-edge-modify-last-attack-edge)

; GMS ops
(define GMSrc Graph-Meta-Structure-random-create)
(define GMSrcr Graph-Meta-Structure-random-create-repetition)
(define GMSvl Graph-Meta-Structure-var-length)
(define GMS->GS Graph-Meta-Structure->Graph-Structure)
(define GMS+ Graph-Meta-Structure-smart-append)
(define GMS_+ Graph-Meta-Structure-append)
(define GMS* Graph-Meta-Structure-repeat)
(define GMSM Graph-Meta-Structure-modulate)

(define f Global-fugue-composer-wrapper)
