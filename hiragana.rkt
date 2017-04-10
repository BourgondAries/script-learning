#! /usr/bin/env racket
#lang racket

(displayln "Convert the hiragana (ひらがな) into romaji.
Write a number to change the amount of letters.
Write skip to skip the current letter.
Write exit or press control+c or control+d to exit.")

(define alphabet
  '(
    (#\あ "a")
    (#\い "i")
    (#\う "u")
    (#\え "e")
    (#\お "o")

    (#\か "ka")
    (#\き "ki")
    (#\く "ku")
    (#\け "ke")
    (#\こ "ko")

    (#\さ "sa")
    (#\し "shi")
    (#\す "su")
    (#\せ "se")
    (#\そ "so")

    (#\た "ta")
    (#\ち "chi")
    (#\つ "tsu")
    (#\て "te")
    (#\と "to")

    (#\な "na")
    (#\に "ni")
    (#\ぬ "nu")
    (#\ね "ne")
    (#\の "no")

    (#\は "ha")
    (#\ひ "hi")
    (#\ふ "hu")
    (#\へ "he")
    (#\ほ "ho")

    (#\ま "ma")
    (#\み "mi")
    (#\む "mu")
    (#\め "me")
    (#\も "mo")

    (#\や "ya")
    (#\ゆ "yu")
    (#\よ "yo")

    (#\ら "ra")
    (#\り "ri")
    (#\る "ru")
    (#\れ "re")
    (#\ろ "ro")

    (#\わ "wa")
    (#\ゐ "wi")
    (#\ゑ "we")
    (#\を "wo")

    (#\ん "n")
  ))

(define (select-random-letter#io letter-count)
  (list-ref alphabet (random letter-count)))

(define (is-digit? character)
  (<= 48 (char->integer character) 57))

(define (is-string-digits? string)
  (andmap is-digit? (string->list string)))

(define (clamp-letter-count value)
  (cond
    ([> value (length alphabet)] (length alphabet))
    ([<= value 0] 1)
    (else value)))

(define start-letter-count 5)

(let loop ([current (select-random-letter#io start-letter-count)]
           [letter-count start-letter-count])
  (display (string-append (string (first current)) ": "))
  (let ([input (read-line)])
    (cond
      ([or (eof-object? input) (string=? input "exit")] void)
      ([string=? input "skip"] (displayln "SKIP!")
                               (loop (select-random-letter#io letter-count) letter-count))
      ([is-string-digits? input] (display "LETTER-CHOICE: ")
                                 (let ([new-count (clamp-letter-count (string->number input))])
                                   (for ([i new-count])
                                     (display (first (list-ref alphabet i))))
                                    (displayln "")
                                   (loop current new-count)))
      ([string=? input (second current)] (displayln "CORRECT!")
                                         (loop (select-random-letter#io letter-count) letter-count))
      (else (displayln "WRONG!")
            (loop current letter-count)))))
