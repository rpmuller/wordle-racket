#lang racket

(require rackunit
         2htdp/image)

(define (read-words-from-file fname) (string-split (port->string (open-input-file fname)) "\n"))

;; Get the wordlists from:
;;   https://gist.github.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b
;;   https://gist.github.com/cfreshman/cdcdf777450c5b5301e439061d29694c
(define answers (read-words-from-file "wordle-answers-alphabetical.txt")) 
(define allowed-guesses (read-words-from-file "wordle-allowed-guesses.txt"))
(define (random-list-element xs) (list-ref xs (random (length xs))))
(define (play-wordle) (wordle (random-list-element answers)))
(define (different-chars guess target)
  (for/list ((g (string->list guess))
             (t (string->list target))
             #:unless (eq? g t)) t))

; This is a racket version of Norvig's code at https://github.com/norvig/pytudes/blob/main/ipynb/Wordle.ipynb
(define (reply-for guess target)
  (let* ([reply (naive-reply guess target)]
         [count (counter (different-chars guess target))])
    (for/list ([g guess] [t target] [r (naive-reply guess target)])
      (if (and (eq? r ".") (> (hash-ref count g 0) 0))
          (begin
            (set! count (hash-update count g sub1))
            "Y")
          r))))

(define (naive-reply guess target)
  (for/list ([g guess] [t target]) (if (eq? g t) "G" ".")))

(define (counter list)
  (if (empty? list) (make-immutable-hash)
      (hash-update (counter (cdr list)) (car list) add1 0)))

(define (color colorsym)
  (cond ((eq? colorsym "G") "green")
        ((eq? colorsym "Y") "yellow")
        ((eq? colorsym ".") "gray")
        (else "white")))

(define (letterbox char colorsym (height 50) (width 40) (charsize 24))
  (overlay (text (string-upcase (string char)) charsize "black")
           (rectangle width height "outline" "black")
           (rectangle width height "solid" (color colorsym))))
  

(define (letterword word reply)
  (apply beside (for/list ([char (string->list word)]
                           [colorsym reply])
                  (letterbox char colorsym))))

(define (keyboard-letterword word reply letters-seen)
  (apply above (list
                (spacer 10)
                (letterword word reply)
                (spacer 10)
                (keyboard-row "qwertyuiop" letters-seen)
                (keyboard-row "asdfghjkl" letters-seen)
                (keyboard-row "zxcvbnm" letters-seen))))

(define (spacer size) (line size size "white"))

(define (keyboard-row letters letters-seen)
  (apply beside
         (for/list ((char (string->list letters))) (letterbox char (hash-ref letters-seen char "w") 25 20 12))))

(define (disallowed word) (or (and (not (member word allowed-guesses)) (not (member word answers))) (not (eq? 5 (string-length word)))))

(define (update-letters-seen letters-seen word reply)
  (if (empty? word) letters-seen
      (hash-set (update-letters-seen letters-seen (cdr word) (cdr reply))
                (car word) (car reply))))

(define (wordle answer)
  (let [(letters-seen (make-immutable-hash))]
    (define (guess word)
      (let ([reply (reply-for word answer)])
        (if (disallowed word) "Disallowed word"
            (begin
              (set! letters-seen (update-letters-seen letters-seen (string->list word) reply))
              ;(letterword word (reply-for word answer))))))
              (keyboard-letterword word reply letters-seen)))))
    guess))

(define (run-tests)
  ; Check that the files were read in properly
  (check-equal? (length answers) 2315)
  (check-equal? (length allowed-guesses) 10657)

  ; Check that random word returns a word
  (check-true (string? (random-list-element answers)))

  ; Tests for reply-for with word "hello"
  (define (reply-test word) (string-join (reply-for "hello" word) ""))
  (check-equal? (reply-test "hello") "GGGGG")
  (check-equal? (reply-test "world") "...GY")
  (check-equal? (reply-test "cello") ".GGGG")
  (check-equal? (reply-test "alley") ".YGY.")
  (check-equal? (reply-test "heavy") "GG...")
  (check-equal? (reply-test "heart") "GG...")
  (check-equal? (reply-test "allay") "..GY.")
  (check-equal? (reply-test "lilac") "..GY.")
  
  (printf "Tests passed~n")

)

(run-tests)

