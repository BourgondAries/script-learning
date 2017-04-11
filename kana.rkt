#! /usr/bin/env racket
#lang racket

(require lens racket/pretty syntax/parse/define threading)

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

(define alphabet
  '(
    #| Monographs |#
    (あ ア a)     (い イ i)   (う ウ u)   (え エ e)     (お オ o)
    (か カ ka)    (き キ ki)  (く ク ku)  (け ケ ke)    (こ コ ko)
    (さ サ sa)    (し シ shi) (す ス su)  (せ セ se)    (そ ソ so)
    (た タ ta)    (ち チ chi) (つ ツ tsu) (て テ te)    (と ト to)
    (な ナ na)    (に ニ ni)  (ぬ ヌ nu)  (ね ネ ne)    (の ノ no)
    (は ハ ha)    (ひ ヒ hi)  (ふ フ fu)  (へ ヘ he)    (ほ ホ ho)
    (ま マ ma)    (み ミ mi)  (む ム mu)  (め メ me)    (も モ mo)
    (や ヤ ya)                (ゆ ユ yu)                (よ ヨ yo)
    (ら ラ ra)    (り リ ri)  (る ル ru)  (れ レ re)    (ろ ロ ro)
    (わ ワ wa) #| (ゐ ヰ wi)              (ゑ ヱ we) |# (を ヲ wo)
    (ん ン n)

    #| Digraphs |#
    (きゃ キャ kya) (きゅ キュ kyu) (きょ キョ kyo)
    (しゃ シャ sha) (しゅ シュ shu) (しょ ショ sho)
    (ちゃ チャ cha) (ちゅ チュ chu) (ちょ チョ cho)
    (にゃ ニャ nya) (にゅ ニュ nyu) (にょ ニョ nyo)
    (ひゃ ヒャ hya) (ひゅ ヒュ hyu) (ひょ ヒョ hyo)
    (みゃ ミャ mya) (みゅ ミュ myu) (みょ ミョ myo)
    (りゃ リャ rya) (りゅ リュ ryu) (りょ リョ ryo)

    #| Diacritics |#
    (が ガ ga)    (ぎ ギ gi)    (ぐ グ gu) (げ ゲ ge) (ご ゴ go)
    (ざ ザ za)    (じ ジ ji)    (ず ズ zu) (ぜ ゼ ze) (ぞ ゾ zo)
    (だ ダ da) #| (ぢ ヂ ji) |# (づ ヅ zu) (で デ de) (ど ド do)
    (ば バ ba)    (び ビ bi)    (ぶ ブ bu) (べ ベ be) (ぼ ボ bo)
    (ぱ パ pa)    (ぴ ピ pi)    (ぷ プ pu) (ぺ ペ pe) (ぽ ポ po)
                             #| (ゔ ヴ vu) |#
  ))

(define-simple-macro (defines (name:id value:expr) ...+)
  (begin (define name value) ...))

(defines (start-letter-count 5)
         (hiragana first)
         (katakana second)
         (romaji   third))

(define (select-random-letter#io letter-count)
  (list-ref alphabet (random letter-count)))

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

(struct/lens entry (rights wrongs) #:prefab)

(define (add1-to-current hash current correct-or-wrong)
  (let* ([c-or-w (if (symbol=? correct-or-wrong 'right) entry-rights-lens entry-wrongs-lens)]
         [this-entry (lens-compose c-or-w (hash-ref-lens current))])
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

(let loop ([current (select-random-letter#io start-letter-count)]
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
      ([compare "stats"] (pretty-display (statistics-to-list statistics))
                         (loop current letter-count kana statistics))
      ([compare "help"] (displayln help)
                        (loop current letter-count kana statistics))
      ([compare "skip"] (display-many "SKIP! The correct answer was: " (romaji current))
                        (loop (select-random-letter#io letter-count) letter-count kana statistics))
      ([compare "hiragana"] (display-many "KANA-CHOICE HIRAGANA: " (map hiragana (take alphabet letter-count)))
                            (loop current letter-count hiragana statistics))
      ([compare "katakana"] (display-many "KANA-CHOICE KATAKANA: " (map katakana (take alphabet letter-count)))
                            (loop current letter-count katakana statistics))
      ([compare (romaji current)] (displayln "CORRECT!")
                                  (~>
                                    (add1-to-current statistics (kana current) 'right)
                                    (loop (select-random-letter#io letter-count) letter-count kana _)))
      ([is-string-digits? input] (display "LETTER-CHOICE: ")
                                 (let ([new-count (clamp-letter-count (string->number input))])
                                   (displayln (map kana (take alphabet new-count)))
                                   (loop current new-count kana statistics)))
      (else (displayln "WRONG!")
            (loop current letter-count kana (add1-to-current statistics (kana current) 'wrong))))))
