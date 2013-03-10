(ns draw-box
  (:require [quil.core :as q])
  (:use     [overtone.sc.machinery.ugen.common]
            [overtone.live]))

(defn draw_box [u x y properties]
 ;Draws a box for the ugen u centred on x and with its top at y.
 (let [w (get (@properties u) :cellwidth)
       left (- x (/ w 2))
       constargs (remove (fn [x] (map? (val x))) (:arg-map u))
       midheight (+ 20 (* 15 (count constargs)))
       h (if (get (@properties u) :expands) (+ 50 midheight) 50)]
  (q/fill 255 255 255)
  (q/rect left y w h)
  ;;;;; what if it's a control proxy? we should indicate that somehow:
  (if 
   (= (subs (str u) 0 47) "overtone.sc.machinery.ugen.sc_ugen.ControlProxy")
   (do (q/fill 0 0 0)
   (q/text "Synthesiser argument" (+ 5 left) (- (+ y h) 10))))
  ;;;;;
  ;title box
  (q/fill 255 255 255)
  (q/rect left y w 25)
  (q/fill 0 0 0)
  (q/text (real-ugen-name u) (+ (* 0.3 w) left) (+ 15 y))
  ;input boxes
 (let [ugenkids (filter (fn [x] (map? (val x))) (:arg-map u))]
  (doseq [[child count] (map list (reverse ugenkids) (range 0 (count ugenkids)))]
   (q/fill 255 255 255)
   (q/rect (+ (* 50 count) left) (- (+ y h) 25) 50 25)
   (q/fill 0 0 0)
   (q/text (name (key child)) (+ (* 50 count) (+ 5 left)) (- (+ y h) 10))
  ))
  ;middle section
  (if (get (@properties u) :expands) (do 
   (q/fill 255 255 255)
   (q/rect left (+ 25 y) w midheight)
   (q/fill 0 0 0)
   (doseq [[arg count] (map list (reverse constargs) (range 0 (count constargs)))]
    (let [argstr (fn [x] (str (name (key x)) ": " (val x)))]
     (q/text (argstr arg) (+ 10 left) (+ (+ 40 y) (* 15 count)))
   )))
  )
  ;TODO: Add some form of hovertext documentation to arguments
  ;Argument documentation is contained in (:args (:spec ugen)), although you'll have to find the :doc corresponding to the :name you know
  ;update properties
  (swap! properties assoc u {:cellwidth w :treewidth (get (@properties u) :treewidth) :expands (get (@properties u) :expands) :tlbr {:top y :left left :bottom (+ y h) :right (+ left w)}})
))
