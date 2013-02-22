(ns visualiser
  (:require [draw-box :as db]
            [scroll-n-zoom :as snz]
            [click-to-expand :as cte] :reload)
  (:use quil.core
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt scale line abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

(def tlbrs {})
(def expands {})

;tlbr maps a ugen onto a 4-tuple that contains values for the Top, Left, Bottom and Right edges of the box representing that ugen
;expands maps a ugen onto whether it should be drawn in expanded big-box form or not

(def controlsrc "test")
;stores the source number of the Control Ugen, if there is one

;(defsynth foo [] (lpf (sin-osc 440) 300))
;(defsynth foo [] (lpf (tap "sintap" 10 (sin-osc 440)) (sin-osc 200)))
;(defsynth foo [freq 440] (lpf (sin-osc freq) 300))
(defsynth foo [freq 440 depth 10 rate 6 length 3]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (tap "tap1" 10 (sin-osc:kr rate)))))))
;the synthdef we'll be rendering

;(def doccstr (arg-doc-str foo))


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

(def tapsrctoname {}) ;this map maps src numbers of ugens to the name of the tap associated with them

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
  (if (= nil (get tapsrctoname source)) ;then box should have a width of at least /200/
    () ;if so, the source is untapped, so continue as normal
    (def widthe (max widthe 200))
  )
  ;if there are any children we will end up with 20 space at the right that is unnecessary - this deletes it. if there aren't any children then the *50 etc will be maximal anyway so the -20 has no effect
  (if (= (:name ugen) "Control") (def maxwidth (assoc maxwidth ugen 1)) (def maxwidth (assoc maxwidth ugen widthe)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare draw-tree)
(declare initialise_watchers)

(defn init [synth]
  (def s (:sdef synth))
  (defonce runsynth (synth))
  (def y 40)
  (def ugenposition (- (count (:ugens s)) 1) )
  (def ugen (nth (:ugens s) ugenposition))
  (def currenttap (- (count (:taps runsynth)) 1))
  (while (= (real-ugen-name ugen) "send-reply")
    (do
      (def tappedsrc (:src (last (:inputs ugen))))
      (def tapname (first (nth (seq (:taps runsynth)) currenttap)))
      (def expands (assoc expands tapname false))
      (def tapsrctoname (assoc tapsrctoname tappedsrc tapname))
      (def ugenposition (- ugenposition 2))
      (def currenttap (- currenttap 1))
      (def ugen (nth (:ugens s) ugenposition))
    )
  ) ;the purpose of all this is to define the root of the tree as the last ugen in the :ugens list, except all the side effects of taps. Each tap creates an impulse and a send-reply at the end of the list; therefore, to ignore them all, we have to skip back twice for every send-reply we encounter. Phew.
;Also at the end of this we have in tapsrctoname a map from src numbers of ugens -> tap names that can be used via (get (:taps runsynth) "name").
  (fill_widths ugen s)
  (initialise_watchers [runsynth])
)

(def taptoatom {})

(defn initialise_watchers [runsynth]
  ;Function to fill the map taptoatom with values
  (defn adder [r v]
    (swap! r (fn [o]
      (conj (subvec o 1) v)
      )
    )
  ) ;so (adder (taptoatom "name") x) has the effect of bunging x on the end of the atom associated with "name", okay?
  (doseq [mytap (vals tapsrctoname)]
    ;mytap holds the name of a tap
    (def taptoatom (assoc taptoatom mytap (atom [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])))
;   (add-watch (get (:taps runsynth) mytap) mytap 
;     (fn [k r o n] (reset! (taptoatom mytap) (adder (taptoatom mytap) n)))
;   )
  )
)

(defn setup []
  (smooth)
  (frame-rate 1)
  (init foo)
)

(defn recurse [s]
  (doseq [mytap (vals tapsrctoname)]
    (reset! (taptoatom mytap) (adder (taptoatom mytap) @(get (:taps runsynth) mytap)))
  )
  (draw-tree ugen (:constants s) 20 y 500 s)
  ;(text (str taptoatom) 50 20)
  ;(text (str @(get (:taps runsynth) "sintap")) 50 30)
)

(def nss {})
;associates a ugen the value of x being used to draw the current subtree. We increase this every time we finish drawing a subtree in order to enforce the subtree seperation property.
(def Iss {})
;associates a ugen with the position along that ugen's box's bottom edge which corresponds to the input to that ugen we are currently drawing a tree corresponding to. Since input box widths are 50, we just bump this up by 50 every time we consider a new input, and that moves along correctly.

(defn draw-tree [root constants x y treewidth s]
  (db/draw-box root constants (+ x (/ treewidth 2)) y s controlsrc expands tlbrs)
  (def tlbrs db/tlbrs)
  ;(text "Hello!" 100 100)
  (def nss (assoc nss root 0))
  (def Iss (assoc Iss root 25))
  (doseq [children (:inputs root)]
     (def source (get children :src))
     (if (not= source -1)
        (do 
           (def child (nth (:ugens s) source))
           (def tapy 0) ;reset tapy
           (def prop (* (/ (maxwidth child) (kidwidth root)) treewidth))
           ;prop calculates the width of the current subtree required to give it an amount of the width of the whole tree proportionate to its maximum width
           ;(text (real-ugen-name child) 10 10)
           (if (= nil (get tapsrctoname source))
             () ;if so, the source is untapped, so continue as normal
             (do
               (def tappername (get tapsrctoname source))
               (db/draw-tap (+ (+ x (/ prop 2)) (nss root)) (+ (nth (tlbrs root) 2) 20) tappername tlbrs taptoatom expands)
               (def tlbrs db/tlbrs)
               (if (= true (expands tappername)) (def tapy 120) (def tapy 70))
               (line (+ (Iss root) (nth (tlbrs root) 1)) (nth (tlbrs root) 2) (/ (+ (nth (tlbrs tappername) 1) (nth (tlbrs tappername) 3)) 2) (nth (tlbrs tappername) 0))
             )
           )
           (if (= source controlsrc) () ;don't draw boxes for "Control" ugens
           (do
             (draw-tree child constants (+ x (nss root)) (+ (nth (tlbrs root) 2) (+ 20 tapy)) prop s) 
;draw the subtree starting from a y-value 20+tapy below the bottom edge of the root
;redefines child, so...
             (def source (get children :src))
             (def child (nth (:ugens s) source))
             (if (not= nil (get tapsrctoname source))
(line (/ (+ (nth (tlbrs (get tapsrctoname source)) 1) (nth (tlbrs (get tapsrctoname source)) 3)) 2) (nth (tlbrs (get tapsrctoname source)) 2) (/ (+ (nth (tlbrs child) 1) (nth (tlbrs child) 3)) 2) (nth (tlbrs child) 0)) ; then draw a line from the tap box straight down
(line (+ (Iss root) (nth (tlbrs root) 1)) (nth (tlbrs root) 2) (/ (+ (nth (tlbrs child) 1) (nth (tlbrs child) 3)) 2) (nth (tlbrs child) 0)) ;else draw line from root to kiddie
             )
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
  (scale snz/zoom)
  (with-translation [snz/xoffset snz/yoffset]
    (recurse (:sdef foo))
  )
)

(defn mouse-press []
   (def agenter (cte/identify [(mouse-x) (mouse-y)] tlbrs))
   (cond
    (= agenter 0) ()
    :else     (do 
     (def expands (assoc expands agenter (not (expands agenter))))
     (redraw)
    )
   )
)

(defsketch exampler :setup setup :draw draw :size [600 700] :key-typed snz/key-press :mouse-pressed mouse-press)
