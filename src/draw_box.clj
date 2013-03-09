(ns draw-box (:use quil.core
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt scale line abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

(defn draw_box [u x y properties]
 ;Draws a box for the ugen u centred on x and with its top at y.
 (let [w (get (@properties u) :cellwidth)
       left (- x (/ w 2))
       constargs (filter (fn [x] (number? (val x))) (:arg-map u))
       midheight (+ 20 (* 15 (count constargs)))
       h (if (get (@properties u) :expands) (+ 50 midheight) 50)]
  (fill 255 255 255)
  (rect left y w h)
  ;;;;;
  (if 
   (= (subs (str u) 0 47) "overtone.sc.machinery.ugen.sc_ugen.ControlProxy")
   (do (fill 0 0 0)
   (text "Synthesiser argument" (+ 5 left) (- (+ y h) 10))))
  ;;;;;
  ;title box
  (fill 255 255 255)
  (rect left y w 25)
  (fill 0 0 0)
  (text (real-ugen-name u) (+ (* 0.3 w) left) (+ 15 y))
  ;input boxes
 (let [ugenkids (remove (fn [x] (number? (val x))) (:arg-map u))]
  (doseq [[child count] (map list (reverse ugenkids) (range 0 (count ugenkids)))]
   (fill 255 255 255)
   (rect (+ (* 50 count) left) (- (+ y h) 25) 50 25)
   (fill 0 0 0)
   (text (name (key child)) (+ (* 50 count) (+ 5 left)) (- (+ y h) 10))
  ))
  ;middle section
  (if (get (@properties u) :expands) (do 
   (fill 255 255 255)
   (rect left (+ 25 y) w midheight)
   (fill 0 0 0)
   (doseq [[arg count] (map list (reverse constargs) (range 0 (count constargs)))]
    (let [argstr (fn [x] (str (name (key x)) ": " (val x)))]
     (text (argstr arg) (+ 10 left) (+ (+ 40 y) (* 15 count)))
   )))
  )
  ;TODO: Add some form of hovertext documentation to arguments
  ;Argument documentation is contained in (:args (:spec ugen)), although you'll have to find the :doc corresponding to the :name you know
  ;update properties
  (swap! properties assoc u {:cellwidth w :treewidth (get (@properties u) :treewidth) :expands (get (@properties u) :expands) :tlbr {:top y :left left :bottom (+ y h) :right (+ left w)}})
))
