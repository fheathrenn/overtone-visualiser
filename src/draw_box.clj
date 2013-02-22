(ns draw-box (:use quil.core
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt scale line abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

(defn evalmap [m constants h y s controlsrc]
 ;(text "Called correctly" 100 100)
 (def numm (get m :src))
 ( cond
  (= -1 numm) (text (str (nth constants (get m :index))) (+ 10 h) (+ 40 y))
  (= numm controlsrc) (text (str (:name (nth (:pnames s) (get m :index)))) (+ 10 h) (+ 40 y))
  :else ( do
   ;(text (str numm) (+ 10 h) (+ 40 y)) ;no point writing the number if it's not a constant
   ;(draw-box (nth (:ugens s) numm) constants h (+ 80 y) s)
  )
 )
)

;this function examines the contents of :inputs for each ugen. :inputs is in the form of a sequence of maps, eg ({:src 1, :index 5} {:src 6, :index 0}).
;src = -1 indicates that the input is a constant, and in this case we use index to look it up in the global list of constants for this synthdef and print it.
;if src matches the number we know to refer to the "Control" UGen that represents arguments, we use index to look up what formal parameter the argument refers to, and print that
;otherwise, the input is from another ugen; the tree structure will carry the information and there is no need to print anything.

(defn draw-small-box [u constants x y s controlsrc tblrmap]
  ;Draws a non-enlarged box representing ugen u such that the top edge of the box is centred on (x, y).
  ;(text "Hello!" 100 100)
  (def heigh 50)
  (if (= (:name u) "Control") (def widt 100) (def widt (max 100 (* 50 (count (:inputs u))) ) ) )
  (fill 255 255 255)
  (def left (- x (* 0.5 widt)))
  (rect left y widt heigh)
  (rect left y widt 25 )
  (def edge left)
  ;now draw the input boxes
  (while (< edge (+ widt left) ) 
    (rect edge (+ y 25) 50 25 )
    (def edge (+ 50 edge) )
  )
  (if (= 1 (count (:inputs u))) (do (fill 200 200 200) (rect (- edge 50) (+ y 25) 50 25)) ) 
  ;(text "Fine" 100 100)
  (def tlbrs (assoc tblrmap u [y left (+ y heigh) (+ left widt)]))
  (fill 0 0 0)
  (text (real-ugen-name u) (+ (* 0.3 widt) left) (+ 15 y))
;  (text (str (:special u)) (+ 10 x) (+ 40 y))
  (def h left)
  ;(text "Calling evalmap..." 100 100)
  (doseq [maps (:inputs u)]
      (evalmap maps constants h y s controlsrc)
      (def h (+ 50 h))
  )
)

;(defn draw-big-box [u constants x y s controlsrc tblrmap]
;  (def heigh 125)
;  (if (= (:name u) "Control") (def widt 100) (def widt (max 100 (* 50 (count (:inputs u))) ) ) )
;  (fill 255 255 255)
;  (def left (- x (* 0.5 widt)))
;  (rect left y widt heigh)
;  (rect left y widt 25 )
;  (def edge left)
;  (while (< edge (+ widt left) ) 
;    (rect edge (+ y 100) 50 25 )
;    (def edge (+ 50 edge) )
;  )
;  (if (= 1 (count (:inputs u))) (do (fill 200 200 200) (rect (- edge 50) (+ y 100) 50 25)) ) 
;  (def tlbrs (assoc tblrmap u [y left (+ y heigh) (+ left widt)]))
;  (fill 0 0 0)
;  (text (real-ugen-name u) (+ (* 0.3 widt) left) (+ 15 y))
;  (text (str (:special u)) (+ 10 x) (+ 40 y))
;  (def h left)
;  (doseq [maps (:inputs u)]
;      (evalmap maps constants h (+ y 75) s controlsrc)
;      (def h (+ 50 h))
;  )
;)
;
(defn draw-box [u constants x y s controlsrc expands tblrmap]
;  (if (expands u)
;    (draw-big-box u constants x y s controlsrc tblrmap)
    (draw-small-box u constants x y s controlsrc tblrmap)
;  )
)

;have exported scope functionality out to seperate boxes

(defn draw-graph [x y name tapatoms]
  ;Draws a graph of the output of the tapper named name, 100 high and 200 wide. Centred on the given x and with its top at the given y.
  (def graphvals @(tapatoms name))
  (def graphvals (vec (rseq graphvals))) ;newer entries are at the front now, older entries at back where can be peeked at
  (fill 144 238 144)
  (rect (- x 100) y 200 100)
  (line (- x 100) (+ y 50) (+ x 100) (+ y 50))
  (def oldx (+ x 100))
  (def oldy (+ y 50))
  ;(text (str graphvals) 30 30)
  (while (> oldx (- x 100)) (do 
    (fill 0 0 0)
    ;(text "Hello?" 30 30)
    (def v (first graphvals))
    (def graphvals (rest graphvals))
    (def v (min v 2))
    (def v (max v -2)) ;clip to b/w 1 and -1
    (def v (* 25 v))
    (def newy (+ (- y v) 50))
    (def newx (- oldx 10))
    (fill 0 0 0)
    (line [oldx oldy] [newx newy])
    (def oldx newx)
    (def oldy newy) )
  )
)

(defn draw-tap [x y name tlbr tapatoms expands]
  (def heigh 50)
  (def widt 100)
  (fill 255 255 255)
  (def left (- x (* 0.5 widt)))
  (rect left y widt heigh)
  (fill 0 0 0)
  (text (str name) (+ left 25) (+ y (* 0.5 heigh) ) )
  (if (= true (expands name))
    (do 
      (draw-graph x y name tapatoms)
      (def tlbrs (assoc tlbr name [y (- x 100) (+ y 100) (+ x 100)]))
    )
    (def tlbrs (assoc tlbr name [y left (+ y heigh) (+ left widt)])) ;else
  )
)
