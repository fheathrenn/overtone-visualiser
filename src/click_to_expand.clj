(ns click-to-expand)

(defn interpole [mouse-xy tlbr]
  ;Given a vector [x y] representing the mouse position
  ;and a map {:top top :left left :bottom bottom :right right} representing the edges of a box,
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
