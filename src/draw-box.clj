(declare draw-box)

(defn evalmap [m constants h y s]
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

(defn- draw-small-box [u constants x y s]
  ;Draws a non-enlarged box representing ugen u such that the top edge of the box is centred on (x, y).
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
  (def tlbrs (assoc tlbrs u [y left (+ y heigh) (+ left widt)]))
  (fill 0 0 0)
  (text (real-ugen-name u) (+ (* 0.3 widt) left) (+ 15 y))
;  (text (str (:special u)) (+ 10 x) (+ 40 y))
  (def h left)
  (doseq [maps (:inputs u)]
      (evalmap maps constants h y s)
      (def h (+ 50 h))
  )
)

(defn- draw-big-box [u constants x y s]
  (def heigh 125)
  (if (= (:name u) "Control") (def widt 100) (def widt (max 100 (* 50 (count (:inputs u))) ) ) )
  (fill 255 255 255)
  (def left (- x (* 0.5 widt)))
  (rect left y widt heigh)
  (rect left y widt 25 )
  (def edge left)
  (while (< edge (+ widt left) ) 
    (rect edge (+ y 100) 50 25 )
    (def edge (+ 50 edge) )
  )
  (if (= 1 (count (:inputs u))) (do (fill 200 200 200) (rect (- edge 50) (+ y 100) 50 25)) ) 
  (def tlbrs (assoc tlbrs u [y left (+ y heigh) (+ left widt)]))
  (fill 0 0 0)
  (text (real-ugen-name u) (+ (* 0.3 widt) left) (+ 15 y))
;  (text (str (:special u)) (+ 10 x) (+ 40 y))
  (def h left)
  (doseq [maps (:inputs u)]
      (evalmap maps constants h (+ y 75) s)
      (def h (+ 50 h))
  )
)

(defn- draw-box [u constants x y s]
  (if (expands u)
    (draw-big-box u constants x y s)
    (draw-small-box u constants x y s)
  )
)
