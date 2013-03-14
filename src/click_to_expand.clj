(ns click-to-expand)

(defn interpole [mouse-x mouse-y tlbr]
  ;Given mouse-x and mouse-y representing the mouse position
  ;and a map {:top top :left left :bottom bottom :right right} representing the edges of a box,
  ;returns true if the click is in the box and false otherwise
  (if (= tlbr nil) false
  (and (and (>= mouse-y (:top tlbr)) (>= mouse-x (:left tlbr)) ) (and (<= mouse-y (:bottom tlbr)) (<= mouse-x (:right tlbr)) ))
))

(defn identify [mouse-x mouse-y properties] (do 
  ;Given mouse-x and mouse-y representing mouse position
  ;and the map properties, 
  ;report the ugen corresponding to the box clicked on,
  ;else return :nil.
 (let [output (atom :nil)
       tlbrseq (map :tlbr (vals @properties))]
  (doseq [[tlbr count] (map list tlbrseq (range 0 (count tlbrseq)))]
   (if (interpole mouse-x mouse-y tlbr) 
    (reset! output (nth (keys @properties) count))))
  @output)))
