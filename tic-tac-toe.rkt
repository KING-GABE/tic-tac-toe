#lang racket

(define (player-to-string player)
  (match player
    [1 "x"]
    [-1 "o"]
    [0 "-"]))

(define (row-to-str row)
  (if (empty? row)
      ""
      (string-append (player-to-string (car row))
                     " " (row-to-str (cdr row)))))

(define (board-to-str board)
  (if (empty? board)
      ""
      (string-append ( row-to-str (car board))
                     "~n"
                     (board-to-str (cdr board)))))

(define (print-board board)
  (printf (board-to-str board)))

(define (full-row row)
  (if (empty? row )
      #t
      (and (not (= (car row) 0)) (full-row (cdr row)))))

(define (full-board board)
  (if (empty? board)
      #t
      (and (full-row (car board)) (full-board (cdr board)))))

(define (nth l n)
  (if (= n 0)
      (car l)
      (nth (cdr l) (- n 1))))

(define (get board row col )
  (nth (nth board row) col))

(define (move-in-row row col-index player)
  (if (= col-index 0 )
      (cons player (cdr row))
      (cons (car row) (move-in-row (cdr row) ( - col-index 1) player ))))

(define (move-h board row-index col-index player)
  (if ( = row-index 0 )
      (cons (move-in-row (car board) col-index player)
            (cdr board))
      (cons (car board) (move-h (cdr board)(- row-index 1 )
                                col-index player))))

(define (move board indices player)
  (move-h board (car indices) (cadr indices) player))

(define (set-winner set )
  (if (empty? (cdr set))
      (car set)
      (if (= (car set) (set-winner (cdr set)))
          (car set)
          0)))

(define (make-col board col-index)
  (list (get board 0 col-index)
        (get board 1 col-index)(get board 2 col-index)))

(define (rows-winner board)
  (if (empty? board )
      0
      (if (not (=(set-winner (car board )) 0))
          (set-winner(car board))
          (rows-winner (cdr board)))))

(define (validate board row-index col-index player)
  (and (< row-index 3) ( < col-index 3 )
       (>= row-index 0)(>= col-index 0)
       (=(get board row-index col-index) 0 )
       (or (= player 1) (= player -1))))

(define test-board (list (list 1 0 0) (list 1 -1 0) (list 0 0 0 )))
(define (cols-winner board col-index)
  (if (= col-index 3)
      0
      (if (= 0(set-winner (make-col board col-index)))
          (cols-winner board (+ 1 col-index))
          (set-winner (make-col board col-index)))))

(define (board-winner board )
  (sgn (+(cols-winner board 0)
         (rows-winner board)
         (set-winner (list (get board 0 0) (get board 1 1)(get board 2 2 )))
         (set-winner (list (get board 2 0) (get board 1 1)(get board 0 2 ))))))

(define (game-over board )
  (or (not (= (board-winner board) 0))
      (full-board board)))

(define (get-move board player)
  (printf (string-append
           "Hi, player "
           (player-to-string player)
           "! What row do you want to go in?~n"))
  (define row-index (string->number (read-line)))
  (printf "What column do you want to go in?~n")
  (define col-index (string->number (read-line)))
  (if (validate board row-index col-index player)
      (list row-index col-index)
      (begin
        (printf
         "I'm sorry bro, but that move ain't gonna work.
 ~nIf you want that move of
 yours to be solid, you better listen to me.
 ~nYour move is solid if the numbers for
 your row and column are from 0 to 2.
 ~nBut that won't work if you try to bust into
 somebody else's space and take theirs!~nYou have to go in an open space.
 ~n")
        (get-move board player))))

(define (play-round board player)
  (print-board board)
  (if (game-over board)
      (if (or (=(board-winner board) 1) (= (board-winner board) -1))
          (printf (string-append "Player " (player-to-string (* -1 player))
                                 " has won!!!~n" ))
          (printf "you two players, who are playing Tic Tac Toe, have tied~n"))
      (play-round (move board (get-move board player) player) (* player -1))))

(define (start-game)
  (play-round (list (list 0 0 0) (list 0 0 0) (list 0 0 0)) 1))