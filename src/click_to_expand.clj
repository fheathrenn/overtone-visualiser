(ns click-to-expand (:use quil.core
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt scale line abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

(defn interpole [mouse-xy tlbr]
  ;Given a vector [x y] representing the mouse position
  ;and a map {:top top :left left :bottom bottom :right right] representing the edges of a box,
  ;returns true if the click is in the box and false otherwise
  (if (= tlbr nil) false
  (and (and (>= (nth mouse-xy 1) (:top tlbr)) (>= (nth mouse-xy 0) (:left tlbr)) ) (and (<= (nth mouse-xy 1) (:bottom tlbr)) (<= (nth mouse-xy 0) (:right tlbr)) )))
)

(defn identify [mouse-xy properties] (do 
  ;Given a vector [x y] representing mouse position
  ;and the map properties, 
  ;report the ugen corresponding to the box clicked on,
  ;else return :nil.
 (let [output (atom :nil)
       tlbrseq (map :tlbr (vals @properties))]
  (doseq [[tlbr count] (map list tlbrseq (range 0 (count tlbrseq)))]
   (if (interpole mouse-xy tlbr) 
    (reset! output (nth (keys @properties) count))))
  @output)))
