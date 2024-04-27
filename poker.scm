(use-modules (srfi srfi-27) (srfi srfi-1))

(define (suit-color suit)
  (if (or (eq? suit 'hearts) (eq? suit 'diamonds)) 'red 'black))

(define suits '(hearts diamonds spades clubs))
(define ranks
  '(two three four five six seven eight nine ten jack queen king ace))

(define (make-card suit rank) (cons suit rank))
(define (card-suit card) (car card))
(define (card-rank card) (cdr card))
(define (card-color card)
  (suit-color (card-suit card)))

(define cards
  (apply append
         (map (lambda (rank)
                (map (lambda (suit) (make-card suit rank)) suits))
              ranks)))

(define (card-compare lcard rcard)
  (letrec ((go (lambda (lrank rrank ranks)
                 (cond ((eq? lrank (car ranks)) 'lt)
                       ((eq? rrank (car ranks)) 'gt)
                       (else (go lrank rrank (cdr ranks)))))))
    (if (eq? (card-rank lcard) (card-rank rcard))
      'eq
      (go (card-rank lcard) (card-rank rcard) ranks))))

(define (card-successive-descending? lcard rcard)
  (letrec ((go (lambda (lrank rrank ranks)
                 (if (nil? (cdr ranks))
                   #f
                   (if (and (eq? lrank (car ranks))
                            (eq? rrank (car (cdr ranks))))
                     #t
                     (go lrank rrank (cdr ranks)))))))
    (if (and (eq? (card-rank lcard) 'two)
             (eq? (card-rank rcard) 'ace))
      #t
      (go (card-rank rcard) (card-rank lcard) ranks))))

(define (card-compare-lists llist rlist)
  (if (null? llist)
    'eq
    (let ((result (card-compare (car llist) (car rlist))))
      (if (eq? result 'eq)
        (card-compare-lists (cdr llist) (cdr rlist))
        result))))

(define (deck-shuffle deck)
  (letrec ((go (lambda (deck new-deck)
                 (if (nil? deck)
                   new-deck
                   (let ((idx (random-integer (length deck))))
                     (let ((head (list-head deck idx))
                           (tail (list-tail deck idx)))
                       (let ((card (car tail)) (tail (cdr tail)))
                         (go (append head tail) (cons card new-deck)))))))))
    (go deck '())))

(define (make-deck) (deck-shuffle cards))

(define (make-combination name cards)
  (cons name cards))

(define (combination-name combination)
  (car combination))

(define (combination-cards combination)
  (cdr combination))

(define (combination-determine table hand)
  (letrec ((go (lambda (argument functions)
                 (let ((result ((car functions) argument)))
                   (if result result (go argument (cdr functions)))))))
    (go (stable-sort
          (append table hand)
          (lambda (l r) (eq? (card-compare l r) 'gt)))
        (list combination-determine-royal-flush
              combination-determine-straight-flush
              combination-determine-four-of-a-kind
              combination-determine-full-house
              combination-determine-flush
              combination-determine-straight
              combination-determine-three-of-a-kind
              combination-determine-two-pair
              combination-determine-pair
              combination-determine-high-card))))

(define (combination-is-flush? cards)
  (let ((suit (card-suit (car cards))))
    (every (lambda (card) (eq? (card-suit card) suit))
           cards)))

(define (combination-is-straight? cards)
  (let ((zipped (zip cards (cdr cards))))
    (every (lambda (cards)
             (card-successive-descending?
               (car cards)
               (car (cdr cards))))
           zipped)))

(define (combinations lst k)
  (cond ((eq? k 0) '(()))
        ((eq? k 1) (map (lambda (item) (list item)) lst))
        (else
         (concatenate
           (map (lambda (item)
                  (let ((filtered-lst (delete item lst)))
                    (map (lambda (lst) (cons item lst))
                         (combinations filtered-lst (- k 1)))))
                lst)))))

(define (combination-straight-helper cards)
  (sort (filter
          combination-is-straight?
          (combinations cards 5))
        (lambda (l r) (eq? (card-compare-lists l r) 'gt))))

(define (combination-same-rank-helper cards k)
  (let ((fold-result
          (fold (lambda (card acc)
                  (if (eq? (length acc) k)
                    acc
                    (if (null? acc)
                      (list card)
                      (if (eq? (card-rank (car acc)) (card-rank card))
                        (cons card acc)
                        (list card)))))
                '()
                cards)))
    (if (eq? (length fold-result) k)
      (let ((fold-result (reverse fold-result)))
        (let ((rest (filter
                      (lambda (card) (not (member card fold-result)))
                      cards)))
          (append fold-result rest)))
      #f)))

(define (combination-determine-royal-flush cards)
  (let ((result
          (filter
            combination-is-flush?
            (combination-straight-helper cards))))
    (if (null? result)
      #f
      (let ((result (car result)))
        (if (eq? (card-rank (car result)) 'ace)
          (make-combination 'royal-flush result)
          #f)))))

(define (combination-determine-straight-flush cards)
  (let ((result
          (filter
            combination-is-flush?
            (combination-straight-helper cards))))
    (if (null? result)
      #f
      (make-combination 'straight-flush (car result)))))

(define (combination-determine-four-of-a-kind cards)
  (let ((result (combination-same-rank-helper cards 4)))
    (if result
      (make-combination
        'four-of-a-kind
        (list-head result 5))
      #f)))

(define (combination-determine-full-house cards)
  (let ((three-result
          (combination-same-rank-helper cards 3)))
    (if three-result
      (let ((two-result
              ((combination-same-rank-helper
                 (drop three-result 3)
                 2))))
        (if two-result
          (make-combination
            'full-house
            (append
              (take three-result 3)
              (take two-result 2)))
          #f))
      #f)))

(define (combination-determine-flush cards)
  (let ((result
          (filter
            (lambda (lst) (>= (length lst) 5))
            (map (lambda (suit)
                   (map (lambda (card) (eq? suit (card-suit card)))
                        cards))
                 suits))))
    (if (null? result)
      #f
      (make-combination 'flush (take (car result) 5)))))

(define (combination-determine-straight cards)
  (let ((result (combination-straight-helper cards)))
    (if (null? result)
      #f
      (make-combination 'straight (car result)))))

(define (combination-determine-three-of-a-kind cards)
  (let ((result (combination-same-rank-helper cards 3)))
    (if result
      (make-combination
        'three-of-a-kind
        (list-head result 5))
      #f)))

(define (combination-determine-two-pair cards)
  (let ((first-result
          (combination-same-rank-helper cards 2)))
    (if first-result
      (let ((second-result
              ((combination-same-rank-helper
                 (drop first-result 2)
                 2))))
        (if second-result
          (make-combination
            'two-pair
            (take (append (take first-result 2) second-result)
                  5))
          #f))
      #f)))

(define (combination-determine-pair cards)
  (let ((result (combination-same-rank-helper cards 2)))
    (if result
      (make-combination 'pair (list-head result 5))
      #f)))

(define (combination-determine-high-card cards)
  (make-combination 'high-card (list-head cards 5)))

(define (make-player name)
  (list (cons 'name name)
        (cons 'bet 0)
        (cons 'bank 500)
        (cons 'folded #f)
        (cons 'hand '())))

(define (player-reset player)
  (assoc-set
    (assoc-set
      (assoc-set player 'hand '())
      'folded
      #f)
    'bet
    0))

(define (make-poker names)
  (list (cons 'players (map make-player names))
        (cons 'player-idx 0)
        (cons 'pot 0)
        (cons 'deck (make-deck))
        (cons 'table '())))

(define (assoc-set alst key value)
  (assoc-set! (list-copy alst) key value))

(define (poker-take-blind idx amount poker)
  (let ((players (assoc-ref poker 'players)))
    (if (> (length players) 0)
      (let ((player (list-ref players idx))
            (before (take players idx))
            (after (drop players (+ idx 1))))
        (let ((player
                (assoc-set
                  player
                  'bet
                  (- (assoc-ref player 'bet) amount)))
              (poker (assoc-set
                       poker
                       'pot
                       (+ (assoc-ref poker 'pot) amount))))
          (if (< (assoc-ref player 'bet) 0)
            (poker-take-blind
              idx
              amount
              (assoc-set poker 'players (append before after)))
            (assoc-set
              poker
              'players
              (concatenate (list before (list player) after))))))
      #f)))

(define (poker-take-small-blind poker)
  (poker-take-blind 0 5 poker))

(define (poker-take-big-blind poker)
  (poker-take-blind 1 10 poker))

(define (window-by count lst)
  (if (<= (length lst) count)
    (list lst)
    (cons (take lst count)
          (window-by count (drop lst count)))))

(define (poker-deal-cards poker)
  (let ((count (* 2 (length (assoc-ref poker 'deck)))))
    (let ((to-deal (take (assoc-ref poker 'deck) count))
          (rest (drop (assoc-ref poker 'deck) count)))
      (let ((to-deal (window-by 2 to-deal))
            (poker (assoc-set poker 'deck rest)))
        (assoc-set
          poker
          'players
          (map (lambda (lst)
                 (assoc-set (car (cdr lst)) 'hand (car lst)))
               (zip to-deal (assoc poker 'players))))))))

(define (poker-reset poker)
  (assoc-set
    (assoc-set
      (assoc-set
        (assoc-set
          (assoc-set
            poker
            'players
            (map (lambda (player) (player-reset player))
                 (assoc-ref poker 'players)))
          'table
          '())
        'deck
        (make-deck))
      'pot
      0)
    'player-idx
    0))

(define (poker-next-game poker)
  (let ((poker (poker-reset poker)))
    (let ((poker (poker-take-small-blind poker)))
      (if poker
        (let ((poker (poker-take-big-blind poker)))
          (if poker (poker-deal-cards poker) #f))
        #f))))
