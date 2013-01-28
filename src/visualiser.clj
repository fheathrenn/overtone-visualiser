(ns visualiser
  (:use quil.core
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt scale line abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

(def tlbrs {})
(def expands {})

;tlbr maps a ugen onto a 4-tuple that contains values for the Top, Left, Bottom and Right edges of the box representing that ugen
;expands maps a ugen onto whether it should be drawn in expanded big-box form or not

(def controlsrc "test")
;stores the source number of the Control Ugen, if there is one

;(defsynth foo [] (tap :tap 10 (sin-osc 440)))
;(defsynth foo [freq 440] (lpf (sin-osc freq) 300))
(defsynth foo [freq 440 depth 10 rate 6 length 3]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))
;the synthdef we'll be rendering

;(def doccstr (arg-doc-str foo))

(load-file "/home/flahr/fh291_p2proj/src/draw-box.clj")
(load-file "/home/flahr/fh291_p2proj/src/scroll-n-zoom.clj")
(load-file "/home/flahr/fh291_p2proj/src/click-to-expand.clj")

;TODO: change to relative pathways


;;;;;;;; MAXWIDTH STUFF ;;;;;;;;;;;;;;;;;

(def maxwidth {})
;maxwidth associates each ugen with the maximum width of the subtree it is the root of. this we have to recursively precompute, obviously

(def kidwidth {})
;also recursively precompute, this is the sum of all of the widths of the children of a node, plus some horizontal spacing. this means subtrees render correctly where their roots have more inputs than their children combined.
(def sources {})
;associates a ugen with its source number
(def widths {})
;used internally by fill_widths to hold values over levels of recursion; no need to access from elsewhere

(declare fill_widths)

;fill_widths recurses through the entire tree and fills in the maxwidths, kidwidth and expands maps.
;It also finds the control UGen, if there is one, and stores its source number in controlsrc.
;maxwidths and kidwidth are values that need to be calculated in order to draw the tree correctly;
;expands is initialised to false for every ugen, but we need to recurse through all ugens to set that up.
;The function should probably be called initialise rather than fill_widths.

(defn fill_widths [ugen s]
  ;(text (:name ugen) 10 10)
  (def expands (assoc expands ugen false))
  (def widthe 0)
  (def widths (assoc widths ugen widthe))
  (doseq [children (:inputs ugen)]
     (def source (get children :src))
     (def sources (assoc sources ugen {children source}))
     ;(text (str source) 50 50)
     (if (not= source -1)
        (do 
          (def testy (:name (nth (:ugens s) source)))
;          (if (= (:name (nth (:ugens s) source) ) "Control")
;             (def controlsrc source)
             (if (= (str testy) "Control") (do (def controlsrc source) (fill_widths (nth (:ugens s) source) s)) (fill_widths (nth (:ugens s) source) s)) ;this execution will change the value of source!
;          )
          ;need to retrieve
          (def source ((sources ugen) children))
          (def widthe (widths ugen))
          ;(text (str maxwidth) 50 150)
          ;(text (str source) 100 100)
          (def widthe (+ widthe (+ 20 (maxwidth (nth (:ugens s) source)))) ) ;the 20 is for spacing
          (def widths (assoc widths ugen widthe))
          ;(text "Hello" 50 200)
        )
     )
  )
  (def widthe (max (- widthe 20) 0) )
  ;deletes the extraneous spacing while preventing ugens with no inputs from having a width of -20
  (def kidwidth (assoc kidwidth ugen widthe))
  (def widthe (max widthe (* 50 (count (:inputs ugen)))))
  ;if there are any children we will end up with 20 space at the right that is unnecessary - this deletes it. if there aren't any children then the *50 etc will be maximal anyway so the -20 has no effect
  (if (= (:name ugen) "Control") (def maxwidth (assoc maxwidth ugen 1)) (def maxwidth (assoc maxwidth ugen widthe)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(declare draw-tree)

(defn init [s]
  (def y 40)
  (def ugen (last (:ugens s)) )
  (fill_widths ugen s)
)

(defn setup []
  (smooth)
  (frame-rate 1)
  (init (:sdef foo))
)

(defn recurse [s]
  (draw-tree ugen (:constants s) 20 y 500 s)
  ;(text doccstr 50 20)
)

(def nss {})
;associates a ugen the value of x being used to draw the current subtree. We increase this every time we finish drawing a subtree in order to enforce the subtree seperation property.
(def Iss {})
;associates a ugen with the position along that ugen's box's bottom edge which corresponds to the input to that ugen we are currently drawing a tree corresponding to. Since input box widths are 50, we just bump this up by 50 every time we consider a new input, and that moves along correctly.

(defn draw-tree [root constants x y treewidth s]
  (draw-box root constants (+ x (/ treewidth 2)) y s)
  (def nss (assoc nss root 0))
  (def Iss (assoc Iss root 25))
  (doseq [children (:inputs root)]
     (def source (get children :src))
     (if (not= source -1)
        (do 
           (def child (nth (:ugens s) source))
           (def prop (* (/ (maxwidth child) (kidwidth root)) treewidth))
           ;prop calculates the width of the current subtree required to give it an amount of the width of the whole tree proportionate to its maximum width
           (if (= source controlsrc) () ;don't draw boxes for "Control" ugens
           (do
             (draw-tree child constants (+ x (nss root)) (+ (nth (tlbrs root) 2) 20) prop s) 
;draw the subtree starting from a y-value 20 below the bottom edge of the root
;redefines child, so...
             (def source (get children :src))
             (def child (nth (:ugens s) source))
             (line (+ (Iss root) (nth (tlbrs root) 1)) (nth (tlbrs root) 2) (/ (+ (nth (tlbrs child) 1) (nth (tlbrs child) 3)) 2) (nth (tlbrs child) 0))
           ) )
           (def space (* (/ 20 (kidwidth root)) treewidth))
           (def nss (assoc nss root (+ (nss root) (+ space prop)) ) )
           (def Iss (assoc Iss root (+ (Iss root) 50 ) ) )
           ;(text "Hello" 200 300)
        )
        (def Iss (assoc Iss root (+ (Iss root) 50 ) ) ) ;else
     )
  )
)

(defn draw []
  (background 200)
  (fill 0 0 0)
  (text (:name (:sdef foo)) 100 20)
  (scale zoom)
  (with-translation [xoffset yoffset]
    (recurse (:sdef foo))
  )
)

(defn mouse-press []
   (def agenter (identify [(mouse-x) (mouse-y)] tlbrs))
   (cond
    (= agenter 0) ()
    :else     (do 
     (def expands (assoc expands agenter (not (expands agenter))))
     (redraw)
    )
   )
)

(defsketch exampler :setup setup :draw draw :size [600 700] :key-typed key-press :mouse-pressed mouse-press)
