(ns draw-box-editor
  (:require [quil.core :as q]
            [update :as u] :reload)
  (:use     [overtone.sc.machinery.ugen.common]
            [overtone.live]))

(defn draw_move_symbol [x y]
  ;Draws a move symbol, relative to the top left of the ugen box.
  (q/fill 255 255 255)
  (q/rect (+ 3 x) (+ 3 y) 12 12)
  (q/fill 0 0 0)
  (q/line (+ 9 x) (+ y 5) (+ 9 x) (+ 13 y))
  (q/line (+ 5 x) (+ y 9) (+ 13 x) (+ 9 y))
)

(defn draw_edit_symbol [x y]
  ;Draws an edit symbol, relative to the top right of the ugen box.
  (q/fill 255 255 255)
  (q/rect (- x 15) (+ 3 y) 12 12)
  (q/fill 0 0 0)
  (q/line (- x 12) (+ y 6) (- x 6) (+ 12 y))
)

(defn draw_box [u x y properties]
 ;Draws a box for the ugen u centred on x and with its top at y.
 (let [w (get (@properties u) :cellwidth)
       left (- x (/ w 2))
       constargs (filter (fn [x] (= :const (first (val x)))) (:arg-map u))
       midheight (if (= (count constargs) 0) 0 (+ 20 (* 15 (count constargs))))
       colour (if (get (@properties u) :highlight) [255 245 100] [255 255 255])
       h (if (get (@properties u) :expands) (if (get (@properties u) :tapped?) (+ 100 midheight) (+ 50 midheight)) 50)]
  (apply q/fill colour)
  (q/rect left y w h)
  ;title box
  (apply q/fill colour)
  (q/rect left y w 25)
  (q/fill 0 0 0)
  (q/text (:name u) (+ (* 0.3 w) left) (+ 15 y))
  ;move
  (draw_move_symbol left y)
  (draw_edit_symbol (+ left w) y)
  ;input boxes
 (let [ugenkids (remove (fn [x] (= :const (first (val x)))) (:arg-map u))]
  (doseq [[child count] (map list (reverse ugenkids) (range 0 (count ugenkids)))]
   (apply q/fill colour)
   (q/rect (+ (* 50 count) left) (- (+ y h) 25) 50 25)
   (q/fill 0 0 0)
   (q/text (name (key child)) (+ (* 50 count) (+ 5 left)) (- (+ y h) 10))
  ))
  ;middle section
  (if (get (@properties u) :expands) (do 
   (apply q/fill colour)
   (q/rect left (+ 25 y) w midheight)
   (q/fill 0 0 0)
   (doseq [[arg count] (map list (reverse constargs) (range 0 (count constargs)))]
    (let [argstr (fn [x] (str (name (key x)) ": " (last (val x))))]
     (q/text (argstr arg) (+ 10 left) (+ (+ 40 y) (* 15 count)))
   )))
  )
  ;TODO: Add some form of hovertext documentation to arguments
  ;Argument documentation is contained in (:args (:spec ugen)), although you'll have to find the :doc corresponding to the :name you know
  ;update properties
  (u/update properties u {:tlbr {:top y :left left :bottom (+ y h) :right (+ left w)}})
))
