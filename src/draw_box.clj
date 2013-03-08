(ns draw-box (:use quil.core
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt scale line abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

(defn draw_box [u x y properties]
 ;Draws a box for the ugen u centred on x and with its top at y.
 (let [w (get (@properties u) :cellwidth)
       left (- x (/ w 2))
       constargs (filter number? (:args u))
       midheight (+ 20 (* 15 (count constargs)))
       h (if (get (@properties u) :expands) (+ 50 midheight) 50)]
  (fill 200 200 200)
  (rect left y w h)
  ;title box
  (fill 255 255 255)
  (rect left y w 25)
  (fill 0 0 0)
  (text (real-ugen-name u) (+ (* 0.3 w) left) (+ 15 y))
  ;input boxes
  (fill 255 255 255)
 (let [ugenkids (remove number? (:args u))]
  (doseq [[child count] (map list ugenkids (range 0 (count ugenkids)))]
   (rect (+ (* 50 count) left) (- (+ y h) 25) 50 25)
  ))
  ;middle section
  (if (get (@properties u) :expands) (do 
   (rect left (+ 25 y) w midheight)
   (fill 0 0 0)
   (doseq [[arg count] (map list constargs (range 0 (count constargs)))]
    (text (str arg) (+ 10 left) (+ (+ 40 y) (* 15 count)))
   ))
  )
  ;update properties
  (swap! properties assoc u {:cellwidth w :treewidth (get (@properties u) :treewidth) :expands (get (@properties u) :expands) :tlbr {:top y :left left :bottom (+ y h) :right (+ left w)}})
))
