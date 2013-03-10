(ns visualiser
  (:require [draw-box :as db]
            [scroll-n-zoom :as snzu]
            [click-to-expand :as c2e] :reload)
  (:use [quil.core]
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt line scale abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

(def properties (atom {}))
(def interboxspace 30)

;Synthesiser to be drawn to screen
;(defsynth foo [] (lpf (+ (sin-osc (sin-osc 440) (sin-osc 2)) (sin-osc 220)) 300))
(defsynth foo [pass_freq 300] (out 0 (lpf (+ (sin-osc (sin-osc 440)) (sin-osc 2)) (sin-osc pass_freq))))
;(defsynth foo [] (out 0 (tap "tap2" 20 (sin-osc (+ 440 (* 40 (tap "tap1" 10 (sin-osc 1))))))))

;;;
;A NOTE ON THE STRUCTURE OF TAPS
;Each tap adds a new root to the tree: a send-reply ugen. It has an impulse ugen as a child, but we can ignore that.
;More importantly, in the send-reply ugen's :arg-map is an argument called :values which contains a sequence of ugens.
;The first items in that sequence is the ugen that tap is, er, tapping.
;THUS, to re-implement scope:
;-find a list of roots
;-for any which are send-reply ugens, assume this implies the existence of a tap; to the ugen it taps, add :tapped? true in the atom properties
;-when drawing a ugen that is tapped, draw a scope box above it - also, treat its :cellwidth as at least 200, to allow for the 200-wide scope box
;-then add watchers etc
;;;


(defn fill_widths [root]
 ;Initialise the properties map with the values required to draw the tree
 (let [ugenkids (filter (fn [x] (map? (val x))) (:arg-map root))
       rootwidth (if (= (subs (str root) 0 47) "overtone.sc.machinery.ugen.sc_ugen.ControlProxy") 150 (max 100 (* 50 (count ugenkids))))]
  (if (zero? (count ugenkids))
    ;true
    (swap! properties assoc root {:cellwidth rootwidth :treewidth rootwidth :expands false})
    ;false
    (do 
     (doseq [child (vals ugenkids)]
      (fill_widths child))
     ;now the :cellwidth and :treewidth of all kids are filled in
     (let [subtreesum (reduce + (map :treewidth (map @properties (vals ugenkids))))
           subtreewidth (+ subtreesum (* interboxspace (- (count ugenkids) 1)))]
     (swap! properties assoc root {:cellwidth rootwidth :treewidth (max rootwidth subtreewidth) :expands false}))))))

(defn draw_tree [r x y]
 ;Draw the tree with root r such that its left edge is at x and its top is at y.
 (let [tw (get (@properties r) :treewidth)
       ugenkids (filter (fn [x] (map? (val x))) (:arg-map r))
       subtreespace (atom x)]
  (db/draw_box r (+ (* 0.5 tw) x) y properties)
  (doseq [[child count] (map list (reverse (vals ugenkids)) (range 0 (count ugenkids)))]
   ;so now count stores a from-0 count of which child we're on
   (draw_tree child @subtreespace (+ 20 (get-in (@properties r) [:tlbr :bottom])))
   (let [childcentrex (+ (* 0.5 (get (@properties child) :cellwidth))(get-in (@properties child) [:tlbr :left]))
         childtopy (get-in (@properties child) [:tlbr :top])
         rootleftx (get-in (@properties r) [:tlbr :left])
         rootbottomy (get-in (@properties r) [:tlbr :bottom])]
   (line childcentrex childtopy (+ (* 50 count) (+ 25 rootleftx)) rootbottomy))
   (swap! subtreespace + interboxspace)
   (swap! subtreespace + (get (@properties child) :treewidth)))))

(defn setup []
  (smooth)
  (frame-rate 1)
  (print "Entering fill_widths")
  (fill_widths (last (:ugens foo))))

(defn draw []
  (background 200)
  (fill 0 0 0)
  (text (:name foo) 100 20)
  (scale @snzu/zoom)
  (with-translation [@snzu/xoffset @snzu/yoffset]
    (draw_tree (last (:ugens foo)) 50 50)
  )
)

(defn mouse-press []
  (let [agenter (c2e/identify [(mouse-x) (mouse-y)] properties)]
   (if (not= agenter :nil)
    (do 
     (let [expandable (not= 0 (count (remove map? (:args agenter))))
           newxp (not (get (@properties agenter) :expands))
           xp (if expandable newxp false)] ;can only expand a box if there is info to show
     (swap! properties assoc agenter
      {:cellwidth (get (@properties agenter) :cellwidth)
       :treewidth (get (@properties agenter) :treewidth)
       :expands xp
       :tlbr (get (@properties agenter) :tlbr)
      })
     (redraw))))))

(defsketch exampler :setup setup :draw draw :size [600 700] :key-typed snzu/key-press :mouse-pressed mouse-press)
