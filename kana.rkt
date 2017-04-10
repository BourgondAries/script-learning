#! /usr/bin/env racket
#lang racket

(displayln "Convert the kana into romaji.
Input  hiragana  to switch to ひらがな.
       katakana  to switch to カタカナ.
       [0-9]+    to change the amount of letters.
       skip      to skip the current letter.
       exit      to exit.")

(define alphabet
  '(
    (あ ア a)  (い イ i)   (う ウ u)   (え エ e)  (お オ o)
    (か カ ka) (き キ ki)  (く ク ku)  (け ケ ke) (こ コ ko)
    (さ サ sa) (し シ shi) (す ス su)  (せ セ se) (そ ソ so)
    (た タ ta) (ち チ chi) (つ ツ tsu) (て テ te) (と ト to)
    (な ナ na) (に ニ ni)  (ぬ ヌ nu)  (ね ネ ne) (の ノ no)
    (は ハ ha) (ひ ヒ hi)  (ふ フ hu)  (へ ヘ he) (ほ ホ ho)
    (ま マ ma) (み ミ mi)  (む ム mu)  (め メ me) (も モ mo)
    (や ヤ ya) (ゆ ユ yu)  (よ ヨ yo)
    (ら ラ ra) (り リ ri)  (る ル ru)  (れ レ re) (ろ ロ ro)
    (わ ワ wa) #| (ゐ ヰ wi) (ゑ ヱ we) |#          (を ヲ wo)
    (ん ン n)
  ))

(define (select-random-letter#io letter-count)
  (list-ref alphabet (random letter-count)))

(define (is-digit? character)
  (<= 48 (char->integer character) 57))

(define (is-string-digits? string)
  (and (non-empty-string? string) (andmap is-digit? (string->list string))))

(define (clamp-letter-count value)
  (cond
    ([> value (length alphabet)] (length alphabet))
    ([<= value 0] 1)
    (else value)))

(define start-letter-count 5)
(define hiragana first)
(define katakana second)
(define romaji   third)

(define (display-many . many)
  (for ([i many])
    (display i))
  (newline))

(let loop ([current (select-random-letter#io start-letter-count)]
           [letter-count start-letter-count]
           [kana hiragana])
  (display (string-append (symbol->string (kana current)) ": "))
  (let ([input (read-line)])
    (cond
      ([eof-object? input] (newline)
                           (void))
      ([string=? input "exit"] (void))
      ([string=? input "skip"] (display-many "SKIP! The correct answer was: " (romaji current))
                               (loop (select-random-letter#io letter-count) letter-count kana))
      ([string=? input "hiragana"] (display-many "KANA-CHOICE HIRAGANA: " (map hiragana (take alphabet letter-count)))
                                   (loop current letter-count hiragana))
      ([string=? input "katakana"] (display-many "KANA-CHOICE KATAKANA: " (map katakana (take alphabet letter-count)))
                                   (loop current letter-count katakana))
      ([is-string-digits? input] (display "LETTER-CHOICE: ")
                                 (let ([new-count (clamp-letter-count (string->number input))])
                                   (displayln (map kana (take alphabet new-count)))
                                   (loop current new-count kana)))
      ([string=? input (symbol->string (romaji current))] (displayln "CORRECT!")
                                                          (loop (select-random-letter#io letter-count) letter-count kana))
      (else (displayln "WRONG!")
            (loop current letter-count kana)))))
