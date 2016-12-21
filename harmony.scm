(define key-center-graph
  '("exp" "C" ("G" (1 2)) "C" "exp" ("Am" (1 2 3)) ("Em" (1 3)) ("C" (1 2)) "Dm" "G" "C")
)

(define subject-chord-progression
  '(0 3 4 0)
)

(define key-change-alist
  '(
    (("C"  "G")  (("C"  "Am" "D" "D7") ("C"  "D7" "Em" "D7")))
    (("Am" "Em") (("Am" "D7" "B" "B7") ("Am" "D"  "C"  "B7")))
    (("C"  "Dm") (("C"  "G"  "A" "A7") ("C"  "F"  "A"  "A7") ("C" "F" "C7" "A7")))
    (("G"  "Am") (("G"  "D"  "E" "E7") ("G"  "C"  "E"  "E7") ("G" "C" "G7" "E7")))
    (("Am" "Dm") (("Am" "E"  "A" "A7") ("Am" "C"  "C7" "A7")))
    (("Dm" "G")  (("F"  "A"  "D" "D7") ("Dm" "Am" "D"  "D7")))
    (("G"  "C")  (("G"  "A7" "D" "G7") ("G"  "G7" "Dm" "G7")))
    (("Am" "C")  (("Am" "D"  "G" "G7") ("Am" "Dm" "G"  "G7")))
    (("Em" "G")  (("Em" "A"  "D" "D7") ("Em" "Am" "D"  "D7")))
    (("C"  "Am") (("C"  "G"  "D" "E7") ("C"  "D"  "E"  "E7")))
    (("Em" "C")  (("Em" "A"  "D" "G7") ("C7" "A7" "Dm" "G7")))
  )
)

(define key-center-key-sig
  '(
    ("C" ())
    ("G" ((3 #\#)))
    ("Dm" ((6 #\-)))
    ("Am" ())
    ("Em" ((3 #\#)))
  )
)

(define chord-key-sig
  '(
    ("F" ())
    ("C" ())
    ("G" ())
    ("D" ((3 #\#)))
    ("A" ((0 #\#)))
    ("E" ((4 #\#)))
    ("B" ((1 #\#) (3 #\#)))

    ("Gm" ((6 #\-)))
    ("Dm" ())
    ("Am" ())
    ("Em" ())
    
    ("C7" ((6 #\-)))
    ("G7" ())
    ("D7" ((3 #\#)))
    ("A7" ((0 #\#)))
    ("E7" ((3 #\#) (4 #\#)))
    ("B7" ((0 #\#) (1 #\#) (3 #\#)))
  )
)

(define key-center-chain-random-create
  (lambda (kcg)
    (if (null? kcg)
      '()
      (if (list? (car kcg))
	(cons (caar kcg) (key-center-chain-random-create (list-tail kcg (apply even-cond (cadar kcg)))))
      	(cons (car kcg) (key-center-chain-random-create (cdr kcg)))
      )
    )
  )
)

(define chord->deg-list
  (lambda (chord)
    (map
      (lambda (x) (modulo (+ x (Note-degree-to-integer (string-ref chord 0))) 7))
      (if (eqv? #\7 (string-ref chord (- (string-length chord) 1))) '(0 2 4 6) '(0 2 4))
    )
  )
)

(define kc-chord->key-sig-list
  (lambda (kc chord)
    (alist-append
      (cadr (assoc kc key-center-key-sig))
      (cadr (assoc chord chord-key-sig))
    )
  )
)

(define kcl->dksl
  (lambda (kc chordlist)
    (map
      (lambda (ch) (list (chord->deg-list ch) (kc-chord->key-sig-list kc ch)))
      chordlist
    )
  )
)

(define sub-subject-chords
  (lambda (kc minor scp)
    (if (null? scp)
      '()
      (let
        (
          (rootnote (string-upcase (Note-offset kc (car scp))))
        )
        (if (and minor (or (= (car scp) 0) (= (car scp) 3) ))
          (cons (string-append rootnote "m") (sub-subject-chords kc minor (cdr scp)))
          (cons rootnote (sub-subject-chords kc minor (cdr scp)))
        )
      )
    )
  )
)

(define subject-chords
  (lambda (key)
    (let*
      (
        (minor
          (eqv? #\m (string-ref key (- (string-length key) 1)))
        )
        (kc
          (if minor
            (substring key 0 (- (string-length key) 1))
            key
          )
        )
      )
      (sub-subject-chords kc minor subject-chord-progression)
    )
  )
)
(define sub-chord-progression-random-create
  (lambda (kcc voice)
    (if (null? kcc)
      '()
      (let*
        (
          (expose (string=? (car kcc) "exp"))
          (tkc (if expose (cadr kcc) (car kcc)))
	  (sbjlist
            (if expose
              (let
                (  
                  (dkc
                    (if (eqv? #\m (string-ref tkc (- (string-length tkc) 1)))
                      (string-append (string-upcase (Note-offset (substring tkc 0 (- (string-length tkc) 1)) 4)) "m")
                      (string-upcase (Note-offset tkc 4))
                    )
                  )
                )
                (list
                  (cons `("subject" ,tkc ,voice) (kcl->dksl tkc (subject-chords tkc)))
                  (cons `("subject" ,dkc ,(modulo (+ voice 1) 3)) (kcl->dksl tkc (subject-chords dkc)))
                  (cons `("subject" ,tkc ,(modulo (+ voice 2) 3)) (kcl->dksl tkc (subject-chords tkc)))
                )
              )
              (list (cons `("subject" ,tkc ,voice) (kcl->dksl tkc (subject-chords tkc))))
            )
          )
          (freemat
            (if (null? (cdr kcc))
              (cons '("free" "C") (kcl->dksl "C" '("F" "G" "C" "C")))
              (let*
                (
                  (nextkcc (if expose (cddr kcc) (cdr kcc)))
                  (targetkc (if (string=? (car nextkcc) "exp") (cadr nextkcc) (car nextkcc)))
                )
                (cons `("free" ,targetkc) (kcl->dksl targetkc (apply even-cond (cadr (assoc (list tkc targetkc) key-change-alist)))))
              )
            )
          )
          (taillist
            (if expose
              (sub-chord-progression-random-create (cddr kcc) voice)
              (sub-chord-progression-random-create (cdr kcc) (modulo (+ voice 1) 3))
            )        
          )
        )
        (append sbjlist (cons freemat taillist))
      )
    )
  )
)

(define (union a b)
  (cond ((null? b) a)
        ((memq (car b) a)
         (union a (cdr b)))
        (#t (union (cons (car b) a) (cdr b)))))

(define chord-progression-random-create
  (lambda ()
    (sub-chord-progression-random-create (key-center-chain-random-create key-center-graph) 1)
  )
)

(define cprc chord-progression-random-create)
