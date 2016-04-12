;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Brendan Reed and Lindsay Richter- PS 8 FINAL|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))

; Brendan Reed and Lindsay Richter
; Problem Set 8

;;;;;;;;;;;;;;;;;;;
;;;; PROBLEM 1 ;;;;
;;;EXERCISE 258 ;;;

; list1 : Number -> List
; creates the list (list 0 ... (- n 1)) for any natural number n
(define (list1 n)
  (cond [(= n 0) '(0)]
        [else (append (cons 0 (build-list (- n 1) add1)))]))

(check-expect (list1 7) (list 0 1 2 3 4 5 6))
(check-expect (list1 4) (list 0 1 2 3))
(check-expect (list1 0) '(0))

; list2 : Number -> List
; creates the list (list 1 ... n) for any natural number n
(define (list2 n)
  (build-list n add1))

(check-expect (list2 10) (list 1 2 3 4 5 6 7 8 9 10))
(check-expect (list2 4) (list 1 2 3 4))

; pow : Number Number -> Number
; raises number to p-th power
(define (pow n p)
  (cond [(= 0 p) 1]
        [else (* n (pow n (- p 1)))]))

(check-expect (pow 2 2) 4)
(check-expect (pow 4 0) 1)
(check-expect (pow 0 4) 0)

; list3 : Number -> List
; creates the list (list 1 1/10 1/100 ...) of n numbers for any natural number n
(define (list3 n)
  (build-list n (λ (x) (/ 1 (pow 10 x)))))

(check-expect (list3 4) '(1 1/10 1/100 1/1000))
(check-expect (list3 1) '(1))

; list4 : Number -> List
; creates the list of the first n even numbers
(define (list4 n)
  (build-list n (λ (x) (* 2 (+ 1 x)))))

(check-expect (list4 5) '(2 4 6 8 10))
(check-expect (list4 1) '(2))

; list5 : Number -> [List-of [List-of Numbers]]
; creates a list of n lists of 0 and 1 in a diagonal arrangement
(define (list5 n)
  (build-list n (λ (x)
                  (build-list n (λ (y) (if (= x y)
                                           1
                                           0))))))

(check-expect (list5 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (list5 0) empty)

; tabulate : Number -> [List-of Number]
(define (tabulate n op)
  (cond [(= n 0) (list (op 0))]
        [else (build-list (+ 1 n) op)]))

(check-expect (tabulate 3 sqr) '(0 1 4 9))
(check-expect (tabulate 0 sqr) '(0))

;;;;;;;;;;;;;;;;;;;
;;;; PROBLEM 2 ;;;;
;;;EXERCISE 267 ;;;

; Number -> Boolean
; consumes a number and decides whether it is less than 10
(lambda (x) (< x 10))

; Number Number -> String
; consumes two numbers, multiplies them, and turns result into a string
(lambda (x y) (number->string (* x y)))


(define-struct IR [name price])
; IR IR -> IR
; returns the IR with the higher price
(lambda (x y) (if (> (IR-price x) (IR-price y))
                  x
                  y))

; Number -> Number
; returns 0 if n is even and 1 if n is odd
(lambda (x) (modulo (/ x 2)))

; Posn Image -> Image
; adds a red 3-pixel dot to the image at p
(lambda (p i) (place-image (circle 1.5 'solid 'red)
                           (posn-x p) (posn-y p)
                           i))


;;;;;;;;;;;;;;;;;;;
;;;; PROBLEM 3 ;;;;
;;;EXERCISE 277 ;;;

; append-from-fold : [List-of X] [List-of X] -> [List-of X]
; Consumes a number of lists and creates one list that contains all
;   parts of the given lists

(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

; foldl reverses the order

(check-expect (append-from-fold (list 1 2 3) (list "a" "b" "c"))
              (list 1 2 3 "a" "b" "c"))
(check-expect (append-from-fold empty (list 2 2)) (list 2 2))

; add-list : [List-of Number] -> Number
; Computes the sum of a list of numbers
(define (add-list l)
  (foldr + 0 l))

(check-expect (add-list (list 1 2 3)) 6)
(check-expect (add-list empty) 0)

; mult-list : [List-of Number] -> Number
; Computes the product of a list of numbers
(define (mult-list l)
  (foldr * 1 l))

(check-expect (mult-list (list 1 2 3)) 6)
(check-expect (mult-list empty) 1)

; horizontal-images : [List-of Image] -> Image
; Horizontally composes a list of Images
(define (horizontal-images l)
  (foldr beside empty-image l))

(horizontal-images (list (circle 3 'solid 'red) (square 3 'solid 'blue)))

; vertical-images : [List-of Image] -> Image
; Vertically composes a list of images
(define (vertical-images l)
  (foldr above empty-image l))

(vertical-images (list (circle 3 'solid 'red) (square 3 'solid 'blue)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TETRIS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define PIXEL-WIDTH 300) ; pixels
(define PIXEL-HEIGHT 600) ; pixels
(define GRID-HEIGHT 20) ; grid units
(define GRID-WIDTH 10) ; grid units
(define CELL-SIZE (/ PIXEL-WIDTH GRID-WIDTH)) ; pixels
(define TICK-SPEED .2) ; seconds/tick
(define BG (empty-scene PIXEL-WIDTH PIXEL-HEIGHT))

; Grid units X increase to the right
; Grid units Y increase down

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Block is a (make-block Number Number Color)
; X and Y are in grid cells
(define-struct block (x y color))

; A Tetra is a (make-tetra Posn BSet)
; The center point is the point around which the tetra rotates
; When it spins.
; The posn is in grid cells
(define-struct tetra (center blocks))

; A Set of Blocks (BSet) is one of:
; - empty
; - (cons Block BSet)
; Order does not matter

; A World is a (make-world Tetra BSet)
; The BSet represents the pile of blocks at the bottom of the screen.
(define-struct world (tetra pile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7 "Kinds" of Tetras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BSet
(define O (cons (make-block 0 0 'green)
                (cons (make-block 0 1 'green)
                      (cons (make-block 1 0 'green)
                            (cons (make-block 1 1 'green) empty)))))

(define I (cons (make-block 0 0 'blue)
                (cons (make-block 1 0 'blue)
                      (cons (make-block 2 0 'blue)
                            (cons (make-block 3 0 'blue) empty)))))

(define L (cons (make-block 2 0 'purple)
                (cons (make-block 0 1 'purple)
                      (cons (make-block 1 1 'purple)
                            (cons (make-block 2 1 'purple) empty)))))

(define J (cons (make-block 0 0 'lightblue)
                (cons (make-block 0 1 'lightblue)
                      (cons (make-block 1 1 'lightblue)
                            (cons (make-block 2 1 'lightblue) empty)))))

(define T (cons (make-block 1 0 'orange)
                (cons (make-block 0 1 'orange)
                      (cons (make-block 1 1 'orange)
                            (cons (make-block 2 1 'orange) empty)))))

(define Z (cons (make-block 0 0 'pink)
                (cons (make-block 1 0 'pink)
                      (cons (make-block 1 1 'pink)
                            (cons (make-block 2 1 'pink) empty)))))

(define S (cons (make-block 1 0 'red)
                (cons (make-block 2 0 'red)
                      (cons (make-block 0 1 'red)
                            (cons (make-block 1 1 'red) empty)))))

; Tetras

(define tetO (make-tetra (make-posn 0 0) O))
(define tetI (make-tetra (make-posn 0 0) I))
(define tetL (make-tetra (make-posn 0 1) L))
(define tetJ (make-tetra (make-posn 0 0) J))
(define tetT (make-tetra (make-posn 0 1) T))
(define tetZ (make-tetra (make-posn 0 0) Z))
(define tetS (make-tetra (make-posn 0 1) S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; make-random-tetra : Number -> Tetra
; makes tetra based on number (which will be randomly selected)
(define (make-random-tetra n)
  (cond [(= n 0) tetO]
        [(= n 1) tetI]
        [(= n 2) tetL]
        [(= n 3) tetJ]
        [(= n 4) tetT]
        [(= n 5) tetZ]
        [(= n 6) tetS]))

;Initial World: 
(define WORLD0 (make-world (make-random-tetra (random 7)) empty))

; Samples:

(define tet1 (make-tetra (make-posn 10 20)
             (cons (make-block 7 19 'red)
                (cons (make-block 7 20 'red)
                      (cons (make-block 8 20 'red)
                            (cons (make-block 9 20 'red) empty))))))
(define tet2 (make-tetra (make-posn 10 20)
             (cons (make-block 7 19 'red)
                (cons (make-block 7 20 'red)
                      (cons (make-block -1 20 'red)
                            (cons (make-block 9 -1 'red) empty))))))
(define tet3 (make-tetra (make-posn 9 5)
                         (list (make-block 9 5 'blue) (make-block 10 5 'blue))))
(define tet4 (make-tetra (make-posn 10 20)
                         (list (make-block 9 5 'blue) (make-block 8 5 'blue))))
(define pile1 (list (make-block 1 1 'blue) (make-block 2 2 'red)))
(define pile2 (list (make-block 10 10 'blue) (make-block 15 5 'red)))
(define world1 (make-world tetO pile1))
(define world2 (make-world tet1 pile1))
(define world3 (make-world tetO (list (make-block 0 1 'blue)
                               (make-block 1 1 'blue)
                               (make-block 2 1 'blue)
                               (make-block 3 1 'blue)
                               (make-block 4 1 'blue)
                               (make-block 5 1 'blue)
                               (make-block 6 1 'blue)
                               (make-block 7 1 'blue)
                               (make-block 8 1 'blue)
                               (make-block 9 1 'blue))))
(define world4 (make-world tetI (list (make-block 0 10 'blue)
                               (make-block 1 10 'blue)
                               (make-block 2 10 'blue)
                               (make-block 3 10 'blue)
                               (make-block 4 10 'blue)
                               (make-block 5 10 'blue)
                               (make-block 6 10 'blue)
                               (make-block 7 10 'blue)
                               (make-block 8 10 'blue)
                               (make-block 9 10 'blue)
                               (make-block 3 9 'green)
                               (make-block 4 9 'green)
                               (make-block 3 8 'green)
                               (make-block 4 8 'green))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; place-image/grid : Image Number Number Image -> Image
; Adds an image using grid coordinates.
(define (place-image/grid fg x y bg)
  (place-image fg
               (round (* (+ 0.5 x) CELL-SIZE))
               (round (* (+ 0.5 y) CELL-SIZE))
               bg))

; draw-block : Block Image -> Image
; Draws a block.
(define (draw-block b scn)
  (place-image/grid (square CELL-SIZE "solid" (block-color b))
                    (block-x b)
                    (block-y b)
                    scn))

(check-expect (draw-block (make-block 1 1 'blue) BG)
              (place-image/grid (square CELL-SIZE 'solid 'blue)
                                1 1 BG))
                           
              
; draw-blocks : BSet Image -> Image
; Draws all the blocks in a BSet.
(define (draw-blocks s scn)
  (foldr draw-block scn s))

(check-expect (draw-blocks pile1 BG)
              (draw-block (make-block 1 1 'blue)
                          (draw-block (make-block 2 2 'red) BG)))

; draw-tetra : Tetra Image -> Image
; Draws the set of blocks in a Tetra
(define (draw-tetra t scn)
  (draw-blocks (tetra-blocks t) scn))

(check-expect (draw-tetra tetO BG)
              (draw-blocks (tetra-blocks tetO) BG))


; score : World -> Number
; Calculates number of free spaces
(define (score w)
  (- 200 (length (world-pile w))))

(check-expect (score WORLD0) 200)
(check-expect (score world1) 198)

; draw-score : World -> Image
(define (draw-score w)
  (place-image/grid (overlay (rectangle 100 40 'outline 'black)
                             (text (string-append "Score : "
                                                  (number->string (score w)))
                                                  20 'black))
                    6
                    1
                    BG))

(check-expect (draw-score WORLD0)
              (place-image/grid (overlay (rectangle 100 40 'outline 'black)
                                         (text "Score : 200" 20 'black))
                                         6
                                         1
                                         BG))

; render : World -> Image
; Draws the world's tetra and the pile of blocks
(define (render w)
  (draw-tetra (world-tetra w) (draw-blocks (world-pile w) (draw-score w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Collisions/Bounds 
;;;;;;;;;;;;;;;;;;;;;;;;;;

; block-touch-blocks-right? : Block BSet -> Boolean
; Is the first block touching any blocks on their right sides in the set?
(define (block-touch-blocks-right? b set)
  (local [; block-touch-block-right? : Block Block -> Boolean
          ; Is the first block touching the right side of the second block?
           (define (block-touch-block-right? b2 b1)
             (and (= (block-x b1) (+ 1 (block-x b2)))
                  (= (block-y b1) (block-y b2))))]
    (cond [(empty? set) false]
        [else (if (block-touch-block-right? b (first set))
                  true
                  (block-touch-blocks-right? b (rest set)))])))

(check-expect (block-touch-blocks-right? (make-block 0 1 'blue) pile1) true)
(check-expect (block-touch-blocks-right? (make-block 2 1 'blue) pile1) false)

; block-touch-blocks-left? : Block BSet -> Boolean
; Is the first block touching any blocks on their left sides in the set?
(define (block-touch-blocks-left? b set)
  (local [; block-touch-block-left? : Block Block -> Boolean
          ; Is the first block touching the left side of the second block?
          (define (block-touch-block-left? b2 b1)
            (and (= (+ 1 (block-x b1)) (block-x b2))
                 (= (block-y b1) (block-y b2))))]
  (cond [(empty? set) false]
        [else (if (block-touch-block-left? b (first set))
                  true
                  (block-touch-blocks-left? b (rest set)))])))

(check-expect (block-touch-blocks-left? (make-block 0 1 'blue) pile1) false)
(check-expect (block-touch-blocks-left? (make-block 2 1 'blue) pile1) true)

; blocks-touch-blocks-right? : BSet BSet -> Boolean
; Are any of the blocks in the first set touching
;  any of the blocks in the second set on the right side?
(define (blocks-touch-blocks-right? s1 s2)
  (cond [(empty? s1) false]
        [else (if (block-touch-blocks-right? (first s1) s2)
                  true
                  (blocks-touch-blocks-right? (rest s1) s2))]))

(check-expect (blocks-touch-blocks-right? pile1
                                          (list (make-block 0 1 'blue)
                                                (make-block 2 1 'blue))) true)
(check-expect (blocks-touch-blocks-right? pile1
                                          (list (make-block 2 1 'blue)
                                                (make-block 3 1 'blue))) true)

; blocks-touch-blocks-left? : BSet BSet -> Boolean
; Are any of the blocks in the first set touching
;  any of the blocks in the second set on the left side?
(define (blocks-touch-blocks-left? s1 s2)
  (cond [(empty? s1) false]
        [else (if (block-touch-blocks-left? (first s1) s2)
                  true
                  (blocks-touch-blocks-left? (rest s1) s2))]))

(check-expect (blocks-touch-blocks-left? pile1
                                          (list (make-block 0 1 'blue)
                                                (make-block 2 1 'blue))) true)
(check-expect (blocks-touch-blocks-left? pile1
                                          (list (make-block 4 1 'blue)
                                                (make-block 3 1 'blue))) false)


; block-on-blocks? : Block BSet -> Boolean
; Is this block on top of any of the blocks in the set?
(define (block-on-blocks? b set)
  (local [; block-on-block? : Block Block -> Boolean
          ; Is the first block on top of the second?
          (define (block-on-block? b1 b2)
            (and (= (block-x b1) (block-x b2))
                 (= (block-y b1) (- (block-y b2) 1))))]
  (cond [(empty? set) false]
        [else (if (block-on-block? b (first set))
              true
              (block-on-blocks? b (rest set)))])))

(check-expect (block-on-blocks? (make-block 3 3 'blue) pile1) false)
(check-expect (block-on-blocks? (make-block 2 1 'blue) pile1) true)
(check-expect (block-on-blocks? (make-block 1 2 'blue) empty) false)

; blocks-on-blocks? : BSet BSet -> Boolean
; Are any of the blocks in the first set on top of
;  any of the blocks in the second set?
(define (blocks-on-blocks? s1 s2)
  (cond [(empty? s1) false]
        [else (if (block-on-blocks? (first s1) s2)
                  true
                  (blocks-on-blocks? (rest s1) s2))]))

(check-expect (blocks-on-blocks? (list (make-block 3 3 'blue)
                                       (make-block 2 1 'blue)) pile1) true)
(check-expect (blocks-on-blocks? pile1 (list (make-block 3 3 'blue)
                                             (make-block 5 5 'blue))) false)
(check-expect (blocks-on-blocks? pile1 empty) false)
(check-expect (blocks-on-blocks? empty pile1) false)

; blocks-on-floor? BSet -> Boolean
; Are any of the blocks in the set touching the bottom edge of the grid?
(define (blocks-on-floor? s)
  (local [; block-on-floor? : Block -> Boolean
          ; Is the block touching the bottom edge of the grid?
          (define (block-on-floor? b)
            (>= (block-y b) (- GRID-HEIGHT 1)))]
  (cond [(empty? s) false]
        [(cons? s) (if (block-on-floor? (first s))
                       true
                       (blocks-on-floor? (rest s)))])))

(check-expect (blocks-on-floor? pile1) false)
(check-expect (blocks-on-floor? (list (make-block 10 20 'blue )
                                      (make-block 10 0 'red))) true)
(check-expect (blocks-on-floor? empty) false)

; block-in-walls? : Block -> Boolean
; Is the block within left and right bounds of the grid?
(define (block-in-walls? b)
  (and (<= (block-x b) (- GRID-WIDTH 1))
       (>= (block-x b) 0)))

(check-expect (block-in-walls? (make-block 9 0 'blue)) true)
(check-expect (block-in-walls? (make-block 11 0 'blue)) false)
(check-expect (block-in-walls? (make-block 0 0 'blue)) true)
(check-expect (block-in-walls? (make-block -1 0 'blue)) false)

; blocks-in-walls? : BSet -> Boolean
; Are the blocks in the set within the left and right bounds of the grid?
(define (blocks-in-walls? s)
  (cond [(empty? s) true]
        [else (if (block-in-walls? (first s))
                  (blocks-in-walls? (rest s))
                  false)]))

(check-expect (blocks-in-walls? pile1) true)
(check-expect (blocks-in-walls? (list (make-block 10 0 'blue)
                                      (make-block 11 0 'blue))) false)
(check-expect (blocks-in-walls? (list (make-block 0 0 'blue)
                                      (make-block -1 0 'blue))) false)
(check-expect (blocks-in-walls? empty) true)

; blocks-over-top? : BSet -> Boolean
; Are any of the blocks in the set over the top of the screen?
(define (blocks-over-top? s)
  (local [; block-over-top? : Block -> Boolean
          ; Is the block over the top of the screen?
            (define (block-over-top? b)
              (<= (block-y b) 0))]
  (cond [(empty? s) false]
        [else (if (block-over-top? (first s))
                  true
                  (blocks-over-top? (rest s)))])))

(check-expect (blocks-over-top? (tetra-blocks tet2)) true)
(check-expect (blocks-over-top? (tetra-blocks tet1)) false)
(check-expect (blocks-over-top? empty) false)

; pile-over-top : World -> Boolean
; Is the pile over the top of the screen?
(define (pile-over-top? w)
  (blocks-over-top? (world-pile w)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FILLED ROWS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; End game: Does this horizontal list of blocks fill every column in the row?
; [List-of Block] Number -> Boolean
; - Are there 10 blocks in this row?
;   -add up the blocks in a row
;   -is the block in the row?
;   -is this row full?
; - Are any of the rows in this world full?
;   -return row number

; block-in-row? : Block Number -> Boolean
; Is the given block in the row number?
(define (block-in-row? b row)
  (= (block-y b) row))

(check-expect (block-in-row? (make-block 0 1 'blue) 1) true)
(check-expect (block-in-row? (make-block 0 3 'red) 4) false)

; blocks-in-row : BSet Number -> Number
; How many blocks are there in this row?
(define (blocks-in-row set row)
  (length (filter (λ (b) (block-in-row? b row)) set)))

(check-expect (blocks-in-row (list (make-block 0 1 'blue)
                               (make-block 1 1 'blue)
                               (make-block 2 1 'blue)
                               (make-block 3 1 'blue)
                               (make-block 4 1 'blue)
                               (make-block 5 1 'blue)
                               (make-block 6 1 'blue)
                               (make-block 7 1 'blue)
                               (make-block 8 1 'blue)
                               (make-block 9 1 'blue)) 1) 10)

(check-expect (blocks-in-row empty 2) 0)
  

; row-full? : BSet Number -> Boolean
; Are there 10 or more blocks in this row?
(define (row-full? set n)
  (>= (blocks-in-row set n) 10))

(check-expect (row-full? pile1 0) false)
(check-expect (row-full? (list (make-block 0 1 'blue)
                               (make-block 1 1 'blue)
                               (make-block 2 1 'blue)
                               (make-block 3 1 'blue)
                               (make-block 4 1 'blue)
                               (make-block 5 1 'blue)
                               (make-block 6 1 'blue)
                               (make-block 7 1 'blue)
                               (make-block 8 1 'blue)
                               (make-block 9 1 'blue)) 1) true)


;;; go through world, check if any of the rows are full.
;;; need to have some reference to rows for this function.
 
; World -> Number
; returns the row number of the lowest full row
; if no rows full, returns a negative number
(define (full-rows w)
  ; there are 20 rows in a world
  (cond [(row-full? (world-pile w) 0) 0]
        [(row-full? (world-pile w) 1) 1]
        [(row-full? (world-pile w) 2) 2]
        [(row-full? (world-pile w) 3) 3]
        [(row-full? (world-pile w) 4) 4]
        [(row-full? (world-pile w) 5) 5]
        [(row-full? (world-pile w) 6) 6]
        [(row-full? (world-pile w) 7) 7]
        [(row-full? (world-pile w) 8) 8]
        [(row-full? (world-pile w) 9) 9]
        [(row-full? (world-pile w) 10) 10]
        [(row-full? (world-pile w) 11) 11]
        [(row-full? (world-pile w) 12) 12]
        [(row-full? (world-pile w) 13) 13]
        [(row-full? (world-pile w) 14) 14]
        [(row-full? (world-pile w) 15) 15]
        [(row-full? (world-pile w) 16) 16]
        [(row-full? (world-pile w) 17) 17]
        [(row-full? (world-pile w) 18) 18]
        [(row-full? (world-pile w) 19) 19]
        [else -1]))
   

(make-world tetO (list (make-block 0 1 'blue)
                               (make-block 1 1 'blue)
                               (make-block 2 1 'blue)
                               (make-block 3 1 'blue)
                               (make-block 4 1 'blue)
                               (make-block 5 1 'blue)
                               (make-block 6 1 'blue)
                               (make-block 7 1 'blue)
                               (make-block 8 1 'blue)
                               (make-block 9 1 'blue)))

(check-expect (full-rows (make-world tetO (list (make-block 0 1 'blue)
                               (make-block 1 1 'blue)
                               (make-block 2 1 'blue)
                               (make-block 3 1 'blue)
                               (make-block 4 1 'blue)
                               (make-block 5 1 'blue)
                               (make-block 6 1 'blue)
                               (make-block 7 1 'blue)
                               (make-block 8 1 'blue)
                               (make-block 9 1 'blue))))
              1)

(check-expect (full-rows (make-world tetO (list (make-block 0 19 'blue)
                               (make-block 1 19 'blue)
                               (make-block 2 19 'blue)
                               (make-block 3 19 'blue)
                               (make-block 4 19 'blue)
                               (make-block 5 19 'blue)
                               (make-block 6 19 'blue)
                               (make-block 7 19 'blue)
                               (make-block 8 19 'blue)
                               (make-block 9 19 'blue))))
              19)

(check-expect (full-rows (make-world tetO (list (make-block 0 19 'blue)
                               (make-block 2 19 'blue)
                               (make-block 3 19 'blue)
                               (make-block 4 19 'blue)
                               (make-block 5 19 'blue)
                               (make-block 6 19 'blue)
                               (make-block 7 19 'blue)
                               (make-block 8 19 'blue)
                               (make-block 9 19 'blue))))
              -1)


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DELETE STUFF ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; keep-block? : Block Number -> Boolean
; Should this block be kept?
(define (keep-block? b row)
  (not (= (block-y b) row)))


; delete-blocks: BSet Number -> BSet
; Makes new list with blocks that should be kept
(define (delete-blocks set n)
  (filter (λ (x) (keep-block? x n)) set))

(check-expect (delete-blocks (list (make-block 0 20 'blue)
                                   (make-block 1 20 'blue)
                                   (make-block 2 20 'blue)
                                   (make-block 3 20 'blue)
                                   (make-block 4 20 'blue)
                                   (make-block 5 20 'blue)
                                   (make-block 6 20 'blue)
                                   (make-block 7 20 'blue)
                                   (make-block 8 20 'blue)
                                   (make-block 9 20 'blue)
                                   (make-block 0 19 'green)
                                   (make-block 1 19 'green)
                                   (make-block 0 18 'green)
                                   (make-block 1 18 'green)) 20)
              (list (make-block 0 19 'green)
                                   (make-block 1 19 'green)
                                   (make-block 0 18 'green)
                                   (make-block 1 18 'green)))

; blocks-down-row : BSet Number -> BSet
; Moves blocks in a set down 1 grid space if its y position is less than input number
(define (blocks-down-row set n)
  (local [(define (block-down-row b)
            (if (< (block-y b) n)
                (block-down b)
                b))]
    (map block-down-row set)))

(check-expect (blocks-down-row (list (make-block 0 10 'blue)
                               (make-block 1 10 'blue)
                               (make-block 2 10 'blue)
                               (make-block 3 10 'blue)
                               (make-block 4 10 'blue)
                               (make-block 5 10 'blue)
                               (make-block 6 10 'blue)
                               (make-block 7 10 'blue)
                               (make-block 8 10 'blue)
                               (make-block 9 10 'blue)) 11)
              (list (make-block 0 11 'blue)
                               (make-block 1 11 'blue)
                               (make-block 2 11 'blue)
                               (make-block 3 11 'blue)
                               (make-block 4 11 'blue)
                               (make-block 5 11 'blue)
                               (make-block 6 11 'blue)
                               (make-block 7 11 'blue)
                               (make-block 8 11 'blue)
                               (make-block 9 11 'blue)))

; delete-row : World -> World
; removes a full row from the pile in the world
(define (delete-row w)
  (make-world (world-tetra w) (delete-blocks (world-pile w) (full-rows w))))

(check-expect (delete-row world3) (make-world tetO empty))


; pile-down : World -> World
; check if row is full
; delete row
; moves rows down above the deleted row
(define (pile-down w)
  (make-world (world-tetra w)
              (blocks-down-row (delete-blocks (world-pile w) (full-rows w)) (full-rows w))))

(check-expect (pile-down world3)
              (make-world tetO empty))
(check-expect (pile-down world4)
              (make-world tetI (list (make-block 3 10 'green)
                               (make-block 4 10 'green)
                               (make-block 3 9 'green)
                               (make-block 4 9 'green))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shift Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; shift-posn-left : Posn -> Posn
; Moves posn 1 grid unit to the left
(define (shift-posn-left p)
  (make-posn (- (posn-x p) 1) (posn-y p)))

(check-expect (shift-posn-left (make-posn 1 1)) (make-posn 0 1))

; shift-posn-right : Posn -> Posn
; Moves posn 1 grid unit to the right
(define (shift-posn-right p)
  (make-posn (+ (posn-x p) 1) (posn-y p)))

(check-expect (shift-posn-right (make-posn 1 1)) (make-posn 2 1))

; shift-posn-down : Posn -> Posn
; Moves posn 1 grid unit down
(define (shift-posn-down p)
  (make-posn (posn-x p) (+ 1 (posn-y p))))

(check-expect (shift-posn-down (make-posn 1 1)) (make-posn 1 2))

; block-right : Block -> Block
; Moves block 1 grid unit right
(define (block-right b)
  (make-block (+ (block-x b) 1) (block-y b) (block-color b)))

(check-expect (block-right (make-block 1 1 'blue)) (make-block 2 1 'blue))

; block-left : Block -> Block
; Moves block 1 grid unit left
(define (block-left b)
  (make-block (- (block-x b) 1) (block-y b) (block-color b)))

(check-expect (block-left (make-block 1 1 'blue)) (make-block 0 1 'blue))

; block-down : Block -> Block
; Moves block 1 grid unit down
(define (block-down b)
  (make-block (block-x b) (+ (block-y b) 1) (block-color b)))

(check-expect (block-down (make-block 1 1 'blue)) (make-block 1 2 'blue))

; blocks-right : BSet -> BSet
; Shifts all blocks in the set right one grid space
(define (blocks-right s)
  (map block-right s))

(check-expect (blocks-right pile1) (list (make-block 2 1 'blue)
                                         (make-block 3 2 'red)))
(check-expect (blocks-right empty) empty)

; blocks-left : BSet -> BSet
; Shifts all blocks in the set left one grid space
(define (blocks-left s)
  (map block-left s))

(check-expect (blocks-left pile1) (list (make-block 0 1 'blue)
                                         (make-block 1 2 'red)))
(check-expect (blocks-left empty) empty)

; blocks-down : BSet -> BSet
; Shifts all blocks in the set down one grid space
(define (blocks-down s)
  (map block-down s))

(check-expect (blocks-down pile1) (list (make-block 1 2 'blue)
                                         (make-block 2 3 'red)))
(check-expect (blocks-down empty) empty)

; tetra-right : Tetra -> Tetra
; Shifts a tetra right one grid space
; A tetra is a (make-tetra Posn BSet)
(define (tetra-right t)
  (make-tetra (shift-posn-right (tetra-center t))
              (blocks-right (tetra-blocks t))))

(check-expect (tetra-right tet1)
              (make-tetra (make-posn 11 20)
                          (list (make-block 8 19 'red)
                                (make-block 8 20 'red)
                                (make-block 9 20 'red)
                                (make-block 10 20 'red))))

; tetra-left : Tetra -> Tetra
; Shifts a tetra left one grid space
; A tetra is a (make-tetra Posn BSet)
(define (tetra-left t)
  (make-tetra (shift-posn-left (tetra-center t))
              (blocks-left (tetra-blocks t))))

(check-expect (tetra-left tet1)
              (make-tetra (make-posn 9 20)
                          (list (make-block 6 19 'red)
                                (make-block 6 20 'red)
                                (make-block 7 20 'red)
                                (make-block 8 20 'red))))

; tetra-down : Tetra -> Tetra
; Shifts a tetra down one grid space
; A tetra is a (make-tetra Posn BSet)
(define (tetra-down t)
  (make-tetra (shift-posn-down (tetra-center t))
              (blocks-down (tetra-blocks t))))

(check-expect (tetra-down tet1)
              (make-tetra (make-posn 10 21)
                          (list (make-block 7 20 'red)
                                (make-block 7 21 'red)
                                (make-block 8 21 'red)
                                (make-block 9 21 'red))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rotate Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; block-rotate-ccw : Posn Block -> Block
; Rotate the block 90 counterclockwise around the posn.
(define (block-rotate-ccw center set)
  (make-block (+ (posn-x center)
                 (- (posn-y center)
                    (block-y set)))
              (+ (posn-y center)
                 (- (block-x set)
                    (posn-x center)))
              (block-color set)))

; block-rotate-cw : Posn Block -> Block
; Rotate the block 90 clockwise around the posn.
(define (block-rotate-cw center set)
  (block-rotate-ccw center (block-rotate-ccw center (block-rotate-ccw center set))))


; blocks-rotate-ccw : Posn BSet -> BSet
; Rotate a group of blocks 90 counterclockwise around the specificed posn
(define (blocks-rotate-ccw center set)
  (cond [(empty? set) empty]
        [else (cons (block-rotate-ccw center (first set))
                    (blocks-rotate-ccw center (rest set)))]))

; blocks-rotate-cw : Posn BSet -> BSet
; Rotate a group of blocks 90 clockwise around the specificed posn
(define (blocks-rotate-cw center set)
  (cond [(empty? set) empty]
        [else (cons (block-rotate-cw center (first set))
                    (blocks-rotate-cw center (rest set)))]))

; tetra-rotate-ccw : Posn Tetra -> Tetra
; Rotate a Tetra 90 counterclockwise around its center
(define (tetra-rotate-ccw t)
  (make-tetra (tetra-center t)
              (blocks-rotate-ccw (tetra-center t) (tetra-blocks t))))

; tetra-rotate-cw : Posn Tetra -> Tetra
; Rotate a Tetra 90 clockwise around its center
(define (tetra-rotate-cw t)
  (make-tetra (tetra-center t)
              (blocks-rotate-cw (tetra-center t) (tetra-blocks t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restrictions on Movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; block-right? : Block -> Boolean
; Can you move the block right without running into the wall?
(define (block-right? b)
  (block-in-walls? (make-block (+ (block-x b) 1)
                               (block-y b)
                               (block-color b))))

(check-expect (block-right? (make-block 10 5 'blue)) false)
(check-expect (block-right? (make-block 6 5 'blue)) true)

; block-left? : Block -> Boolean
; Can you move the block left without running into the wall?
(define (block-left? b)
  (block-in-walls? (make-block (- (block-x b) 1)
                               (block-y b)
                               (block-color b))))

(check-expect (block-left? (make-block 0 5 'blue)) false)
(check-expect (block-left? (make-block 1 5 'blue)) true)

; blocks-right? : BSet -> Boolean
; Can you move the set of blocks right without running into the wall?
(define (blocks-right? s)
  (cond [(empty? s) true]
        [else (if (block-right? (first s))
                  (blocks-right? (rest s))
                  false)]))

(check-expect (blocks-right? (list (make-block 9 5 'blue)
                                   (make-block 10 5 'blue))) false)
(check-expect (blocks-right? (list (make-block 4 5 'blue)
                                   (make-block 8 5 'blue))) true)

; blocks-left? : BSet -> Boolean
; Can you move the set of blocks left without running into the wall?
(define (blocks-left? s)
  (cond [(empty? s) true]
        [else (if (block-left? (first s))
                  (blocks-left? (rest s))
                  false)]))

(check-expect (blocks-left? (list (make-block 1 5 'blue)
                                   (make-block 0 5 'blue))) false)
(check-expect (blocks-left? (list (make-block 1 5 'blue)
                                   (make-block 2 5 'blue))) true)

; tetra-right? : Tetra -> Boolean
; Can the tetra move right without running into the wall?
(define (tetra-right? t)
  (blocks-right? (tetra-blocks t)))

(check-expect (tetra-right? tet3) false)
(check-expect (tetra-right? tet4) false)

; tetra-left? : Tetra -> Boolean
; Can the tetra move left without running into the wall?
(define (tetra-left? t)
  (blocks-left? (tetra-blocks t)))

(check-expect (tetra-left? tet2) false)
(check-expect (tetra-left? tet4) true)

; world-right? : World -> Boolean
; Can the tetra in the world move right?
(define (world-right? w)
  (and (blocks-right? (tetra-blocks (world-tetra w)))
       (run-into-tetra-right? w)))

; world-left? : World -> Boolean
; Can the tetra in the world move left?
(define (world-left? w)
  (and (blocks-left? (tetra-blocks (world-tetra w)))
       (run-into-tetra-left? w)))


; blocks-rotate-ccw? : Posn BSet -> Boolean
; Can the set of blocks rotate ccw without running into the wall?
(define (blocks-rotate-ccw? center set)
  (cond [(empty? set) true]
        [else (or (blocks-in-walls? (blocks-rotate-ccw center set))
                  (blocks-on-floor? (blocks-rotate-ccw center set)))]))

; tetra-rotate-ccw? : Tetra -> Boolean
; Can the tetra rotate ccw without running into the wall?
(define (tetra-rotate-ccw? t)
  (blocks-rotate-ccw? (tetra-center t) (tetra-blocks t)))

(check-expect (tetra-rotate-ccw? tetO) false)
(check-expect (tetra-rotate-ccw? tet3) true)
(check-expect (blocks-rotate-ccw? (make-posn 1 1) empty) true)

; blocks-rotate-cw? : Posn BSet -> Boolean
; Can the set of blocks rotate cw without running into the wall?
(define (blocks-rotate-cw? center set)
  (cond [(empty? set) true]
        [else (or (blocks-in-walls? (blocks-rotate-ccw center set))
                  (blocks-on-floor? (blocks-rotate-ccw center set)))]))

; tetra-rotate-ccw? : Tetra -> Boolean
; Can the tetra rotate cw without running into the wall?
(define (tetra-rotate-cw? t)
  (blocks-rotate-cw? (tetra-center t) (tetra-blocks t)))

(check-expect (tetra-rotate-cw? tet1) true)
(check-expect (tetra-rotate-cw? tet3) true)
(check-expect (blocks-rotate-cw? (make-posn 1 1) empty) true)


; run-into-tetra-left? : World -> Boolean
; Is the tetra able to move left and not collide with a tetra from the pile?
(define (run-into-tetra-left? w)
  (not (blocks-touch-blocks-left? (tetra-blocks (world-tetra w)) (world-pile w))))

; run-into-tetra-right? : World -> Boolean
; Is the tetra able to move right and not collide with a tetra from the pile?
(define (run-into-tetra-right? w)
  (not (blocks-touch-blocks-right? (tetra-blocks (world-tetra w)) (world-pile w))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Running the Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Starts at WORLD0.

; Tetra falls continuously UNTIL it hits one of:
; -- floor
; -- another tetra

; After this "collision", another world is created
; -- new "pile" with tetra from previous world
; -- new "tetra", generated randomly.

; make-random-tetra : Number -> Tetra
; makes tetra based on number (which will be randomly selected)
(define (randomize-tetras n)
  (cond [(= n 0) tetO]
        [(= n 1) tetI]
        [(= n 2) tetL]
        [(= n 3) tetJ]
        [(= n 4) tetT]
        [(= n 5) tetZ]
        [(= n 6) tetS]))

; add-tetra-to-set : Tetra Bset -> Bset
; appends the tetra onto the set of blocks
(define (add-tetra-to-set t s)
  (append (tetra-blocks t) s))

; add-tetra-to-pile : World -> W
(define (add-tetra-to-pile w)
  (add-tetra-to-set (world-tetra w) (world-pile w)))

; collision : World -> World
; In the case of the tetra being on the floor or on top of a block,
; -- call add-tetra to the pile
; -- call new-tetra
(define (collision w)
  (pile-down (make-world (randomize-tetras (random 7)) (add-tetra-to-pile w))))

; next-world : World -> World
; Generates the next world. 
; -- if the tetra is on the floor or on top of a block, it will call collision
; -- if not, the tetra will shift down

(define (next-world w)
  (cond [(pile-over-top? w) w]
        [else (if (or (blocks-on-floor? (tetra-blocks (world-tetra w)))
                      (blocks-on-blocks? (tetra-blocks (world-tetra w)) (world-pile w)))
                  (collision w)
                  (make-world (tetra-down (world-tetra w)) (world-pile w)))]))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; KEY-EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Left-arrow : moves tetra left
; Right-arrow : moves tetra right
; s : rotates tetra clockwise
; a : rotates tetra counterclockwise


; got-key : World KeyEvent -> World
; Changes world based on keystrokes
; - Left-arrow : moves tetra left
; - Right-arrow : moves tetra right
; - s : rotates tetra clockwise
; - a : rotates tetra counterclockwise
(define (got-key w ke)
  (cond [(string=? ke "left") (if
                               (world-left? w)
                               (make-world (tetra-left (world-tetra w))
                                               (world-pile w))
                               w)]
        
        [(string=? ke "right") (if
                               (world-right? w)
                               (make-world (tetra-right (world-tetra w))
                                               (world-pile w))
                               w)]
        
        [(string=? ke "s") (if
                            (tetra-rotate-cw? (world-tetra w))
                            (make-world (tetra-rotate-cw (world-tetra w))
                                            (world-pile w))
                            w)]
        
        [(string=? ke "a") (if
                            (tetra-rotate-ccw? (world-tetra w))
                            (make-world (tetra-rotate-ccw (world-tetra w))
                                            (world-pile w))
                            w)]
        [else w]))

(check-expect (got-key world1 "left") (make-world tetO pile1))
(check-expect (got-key world2 "left") (make-world
                                       (make-tetra (make-posn 9 20)
                                             (list (make-block 6 19 'red)
                                                   (make-block 6 20 'red)
                                                   (make-block 7 20 'red)
                                                   (make-block 8 20 'red)))
                                       pile1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ending the Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Game ends when pile-over-top returns true
; at this point, the number of blocks in the pile should be returned

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIG-BANG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (main _unused)
  (big-bang WORLD0
            [on-tick next-world TICK-SPEED]
            [to-draw render]
            [on-key got-key]
            [stop-when pile-over-top?]))

;(score (main 0))
                            
   





