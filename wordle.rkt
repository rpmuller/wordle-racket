#lang racket

;; Todo: coloring of multiple letters, when only a single one is in the answer
;;   Also explore how this is manifest in the keyboard printout.

(require rackunit
         2htdp/image)

(define (read-words-from-file fname)
  (let* ((in (open-input-file fname))
        (data (port->string in))
        (words (string-split data "\n")))
    words))

;; You can get the wordlists from:
;;   https://gist.github.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b
;;   https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c
(define answers
  (read-words-from-file "wordle-answers-alphabetical.txt")) 
(define allowed-guesses
  (read-words-from-file "wordle-allowed-guesses.txt")) 

(define (random-word)
  (list-ref answers (random (length answers))))

(define (read-guess)
  (let loop ((word (read-line)))
    (cond ((not (= (string-length word) 5)) (loop (read-line)))
          ((not (or (member word allowed-guesses) (member word answers))) (loop (read-line)))
          (else word))))

(define (letter-color guess-char answer-char all-chars)
  (cond ((eq? guess-char answer-char) "green")
        ((member guess-char all-chars) "yellow")
        (else "gray")))

(define (score-iter  guess-chars answer-chars all-chars)
  (if (empty? guess-chars) '()
      (cons (list (letter-color (car guess-chars) (car answer-chars) all-chars) (car guess-chars))
            (score-iter (cdr guess-chars) (cdr answer-chars) all-chars))))

(define (score-guess guess answer)
  (score-iter (string->list guess) (string->list answer) (string->list answer)))

; Utilities to transfer the scored guess to printable text strings:
(define (sgel-color sgel) (car sgel))
(define (sgel-letter sgel) (cadr sgel))
(define (sgel->string sgel) (string-join (list (sgel-color sgel) (string (sgel-letter sgel)))))
(define (sg->string sg) (string-join (map sgel->string sg)))

; Utilities to draw graphical colored letter boxes
(define (letterbox char color (height 50) (width 40) (charsize 24))
  (overlay 
   (text (string-upcase (string char)) charsize "black")
   (rectangle width height "outline" "black")
   (rectangle width height "solid" color))
  )

(define (aligned-letterwords sg) (apply beside (letterwords sg)))

(define (spacer size) (line size size "white"))

(define (keyboard-and-letterwords sg letters-seen)
  (apply above (list
                (spacer 10)
                (aligned-letterwords sg)
                (spacer 10)
                (keyboard-row "qwertyuiop" letters-seen)
                (keyboard-row "asdfghjkl" letters-seen)
                (keyboard-row "zxcvbnm" letters-seen))))

(define (keyboard-row letters letters-seen)
  (apply beside
         (for/list ((char (string->list letters))) (letterbox char (hash-ref letters-seen char "white") 25 20 12))))

(define (letterwords sg) (map letterboxed-el sg))
(define (letterboxed-el sgel)  (letterbox (sgel-letter sgel) (sgel-color sgel)))

(define (play-wordle)
  (let ((answer (random-word))
        (letters-seen (make-hash)))
    (define (guess word)
      (let ((sg (score-guess word answer)))
        (if (and (not (member word allowed-guesses))
                 (not (member word answers))) "Disallowed guess"
            (begin
              (for ((sgel sg))
                (let* ((color (sgel-color sgel))
                       (letter (sgel-letter sgel))
                       (oldcolor (hash-ref letters-seen letter "gray")))
                  (cond ((and (eq? color "yellow") (eq? oldcolor "gray")) (hash-set! letters-seen letter "yellow"))
                        ((eq? color "green") (hash-set! letters-seen letter "green"))
                        ((and (eq? color "gray") (eq? oldcolor "gray")) (hash-set! letters-seen letter "gray"))
                        ; else do nothing
                        )))
              ;(aligned-letterwords sg)))))
              (keyboard-and-letterwords sg letters-seen)))))
    guess))


(define (run-tests)
  ; Check that the files were read in properly
  (check-equal? (length answers) 2315)
  (check-equal? (length allowed-guesses) 10657)

  ; Check that random word returns a word
  (check-true (string? (random-word)))

  ; Check the letter color results
  (check-equal? (letter-color #\c #\c '(#\c)) "green")
  (check-equal? (letter-color #\c #\b '(#\c)) "yellow")
  (check-equal? (letter-color #\c #\b '(#\b)) "gray")

  ; Check score-guess:
  (check-equal? (score-guess "choke" "champ") '(("green" #\c) ("green" #\h) ("gray" #\o) ("gray" #\k) ("gray" #\e)))
  (check-equal? (sg->string (score-guess "choke" "champ")) "green c green h gray o gray k gray e")
  ; Make sure I handle multiple letters in the wrong place
  (check-equal? (score-guess "beefy" "refer") '(("gray" #\b) ("green" #\e) ("yellow" #\e) ("yellow" #\f) ("gray" #\y)))
  

  ; Check sgel utils
  (check-equal? (sgel-letter '("green" #\c)) #\c)
  (check-equal? (sgel-color '("green" #\c)) "green")
  (check-equal? (sgel->string '("green" #\c)) "green c")
  (printf "Tests passed~n")
)  

(run-tests)
(aligned-letterwords (score-guess "wordle" "wxodxe"))


