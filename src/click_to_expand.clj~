(defn interpole [mouse-xy tlbr]
  ;Given a vector [x y] representing the mouse position
  ;and a vector [top left bottom right] representing the edges of a box,
  ;returns true if the click is in the box and false otherwise
  (and (and (>= (nth mouse-xy 1) (nth tlbr 0)) (>= (nth mouse-xy 0) (nth tlbr 1)) ) (and (<= (nth mouse-xy 1) (nth tlbr 2)) (<= (nth mouse-xy 0) (nth tlbr 3)) ))
)

(defn identify [mouse-xy tlbrmap] (do 
  ;Given a vector [x y] representing mouse position
  ;and a map {n1 [t1 l1 d1 r1], n2 [t2 l2 d2 r2]...} representing edges of boxes, 
  ;report the n corresponding to the box clicked on,
  ;else return :nil.
  (def tlbrseq (seq tlbrmap))
;tldrseq is now of the form ([n1 [t1 l1 d1 r1]] [n2 ...))
  (def output 0)
  (doseq [vec tlbrseq]
  ;vec is now of the form [n1 [t1 l1 d1 r1]]
    (def tlbrvec (nth vec 1))
    ;tlbrvec is now [t1 l1 d1 r1]
    (def output 
      (if (interpole mouse-xy tlbrvec) (nth vec 0) output )
    )
  )
  ;return value of function
  output
))
