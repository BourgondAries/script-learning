#! /usr/bin/env racket
#lang racket

(require lens racket/pretty syntax/parse/define threading
         "logger.rkt")

(define help "Convert the kana into romaji.
Input  ka        correct answer for か or カ (similar for other kana).
       help      print this message.
       hiragana  switch to ひらがな.
       katakana  switch to カタカナ.
       [0-9]+    change the amount of letters.
       skip      skip the current letter, print correct answer.
       stats     print answer statistics.
       reset     reset statistics.
       exit      to exit.")
(displayln help)

(define alphabet (file->value "alphabet.rkt-value"))

(define-simple-macro (defines (name:id value:expr) ...+)
  (begin (define name value) ...))

(struct/lens entry (rights wrongs) #:prefab)

(defines (start-letter-count 5)
         (hiragana first)
         (katakana second)
         (romaji   third)
         (rights entry-rights-lens)
         (wrongs entry-wrongs-lens))

(define (score-letter letter statistic)
  (if (hash-has-key? statistic letter)
    (let ([entry (hash-ref statistic letter)])
      (- (entry-rights entry) (* 2 (entry-wrongs entry))))
    0))

(define (random-from-list list)
  (list-ref list (random (length list))))

(define (select-random-letter#io letter-count statistics kana previous)
  (define percentage 25)
  (define multiplier (/ 1 (/ percentage 100)))
  (let ([prev-new (if (= letter-count 1) 'none previous)])
    (~>>
      (take alphabet letter-count)
      (filter (lambda (x) (not (equal? (kana x) prev-new))))
      (map (lambda (letter) (list letter (score-letter (kana letter) statistics))))
      (sort _ (lambda (x y) (< (second x) (second y))))
      ((lambda (list) (take list (ceiling (/ (length list) multiplier)))))
      (map first)
      random-from-list)))

(define (is-digit? character)
  (<= 48 (char->integer character) 57))

(define (is-string-digits? string)
  (and (non-empty-string? string) (andmap is-digit? (string->list string))))

(define (clamp-letter-count value)
  (cond
    ([> value (length alphabet)] (length alphabet))
    ([= value 0] (length alphabet))
    ([< value 0] 1)
    (else value)))

(define (display-many . many)
  (for ([i many])
    (display i))
  (newline))


(define (add1-to-statistics hash current which)
  (let* ([this-entry (lens-compose which (hash-ref-lens current))])
    (if (hash-has-key? hash current)
      (lens-transform this-entry hash add1)
      (~>
        (lens-set (hash-ref-lens current) hash (entry 0 0))
        (lens-transform this-entry _ add1)))))

(define (occurrence entry)
  (+ (entry-rights entry) (entry-wrongs entry)))

(define (pair-to-list pair)
  (list (car pair) (cdr pair)))

(define (statistics-to-list stats)
  (if (hash-empty? stats)
    "The statistics set is empty"
    (~>>
      (hash->list stats)
      (map pair-to-list)
      (sort _ (lambda (x y) (> (occurrence x) (occurrence y))) #:key second)
      (map (lambda (x) (list (first x) 'right: (entry-rights (second x)) 'wrong: (entry-wrongs (second x))))))))

(let loop ([current (select-random-letter#io start-letter-count (make-immutable-hash) hiragana 'none)]
           [letter-count start-letter-count]
           [kana hiragana]
           [statistics (make-immutable-hash)])
  (display (string-append (symbol->string (kana current)) ": "))
  (let* ([input (read-line)]
         [compare (lambda (string) (string=? (if (symbol? string) (symbol->string string) string) input))])
    (cond
      ([eof-object? input] (newline)
                           (void))
      ([compare "exit"] (void))
      ([compare "reset"] (displayln "RESET-STATISTICS!")
                         (loop current letter-count kana (make-immutable-hash)))
      ([compare "stats"] (let ([stats (statistics-to-list statistics)])
                           (pretty-display stats)
                           (display-many "Total entries: " (length stats)))
                         (loop current letter-count kana statistics))
      ([compare "help"] (displayln help)
                        (loop current letter-count kana statistics))
      ([compare "skip"] (display-many "SKIP! The correct answer was: " (romaji current))
                        (let ([new-stats (add1-to-statistics statistics (kana current) wrongs)])
                          (loop (select-random-letter#io letter-count new-stats kana (kana current)) letter-count kana new-stats)))
      ([compare "hiragana"] (display-many "KANA-CHOICE HIRAGANA: " (map hiragana (take alphabet letter-count)))
                            (loop current letter-count hiragana statistics))
      ([compare "katakana"] (display-many "KANA-CHOICE KATAKANA: " (map katakana (take alphabet letter-count)))
                            (loop current letter-count katakana statistics))
      ([compare (romaji current)] (displayln "CORRECT!")
                                  (let ([new-stats (add1-to-statistics statistics (kana current) rights)])
                                    (loop (select-random-letter#io letter-count new-stats kana (kana current)) letter-count kana new-stats)))
      ([is-string-digits? input] (display "LETTER-CHOICE: ")
                                 (let ([new-count (clamp-letter-count (string->number input))])
                                   (displayln (map kana (take alphabet new-count)))
                                   (loop current new-count kana statistics)))
      (else (displayln "WRONG!")
            (loop current letter-count kana (add1-to-statistics statistics (kana current) wrongs))))))
