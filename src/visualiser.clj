(ns visualiser
  (:require [draw-box :as db]
            [scroll-n-zoom :as snzu]
            [click-to-expand :as c2e]
            [update :as u]
            [quil.core :as q] :reload)
  (:use [overtone.sc.machinery.ugen common doc]
        [overtone.live]))

(def properties (atom {}))
(def roots (atom ()))
(def interboxspace 30)


(def docstring (atom ""))
(def doccoords (atom [0 0]))
(defn draw-docs []
  (do 
  (let [x (first @doccoords) y (last @doccoords)
        lines (count (re-seq #"\n" @docstring))]
  (q/fill 255 250 205)
  (q/rect x y (q/text-width @docstring) (* 15 lines))
  (q/fill 0 0 0)
  (q/text @docstring x y)
)))

;Synthesiser to be drawn to screen
;(defsynth foo [] (lpf (+ (sin-osc (sin-osc 440) (line (sin-osc 1) (sin-osc 1) (sin-osc 1) (sin-osc 1))) (sin-osc 220)) 300))
;(defsynth foo [freq 440 fie 200] (line (+ (saw fie) 6) (lpf (+ (sin-osc (sin-osc freq) (tap "tap2" 21 (sin-osc 21))) (sin-osc fie)) 300) (tap "tap3" 50 (+ (sin-osc (sin-osc freq) (sin-osc 2)) (sin-osc 220))) FREE))
;(defsynth foo [pass_freq 300] (lpf (+ (sin-osc [(sin-osc 440) (saw 440)]) (sin-osc 2)) (sin-osc pass_freq)))
;(defsynth foo [] (out 0 (tap "tap2" 20 (sin-osc (+ 440 (* 40 (tap "tap1" 10 (sin-osc 1))))))))
;(defsynth foo [] (out 0 (sin-osc [440 450])))
;(defsynth foo [freq 2] (out 0 (tap "tap" 100 (sin-osc freq))))
(defsynth foo [] (out 0 (sin-osc 440)))
;(defsynth foo [freq 440 fie 200] (tap "tap1" 100 (* (line 0 1 10 FREE) (sin-osc 2))))
;(defsynth foo [start 0 end 1 time 10 atend FREE] (* (line start end time atend) (sin-osc 2)))

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

(defn discard [ugen seq] (let [isugen? (fn [x] (= ugen x))] (swap! seq (fn [oldseq] (remove isugen? oldseq)))))

(defn find_roots [synth]
  ;Return a lazyseq of ugens that are roots of the synthesiser synth. This list does not include any byproducts of taps, which are removed from the list in a process that also tags the ugens they tap with :tagged? true.
  (let [ugenlist (reverse (get synth :ugens))
        output (atom ugenlist)
        numtaps (atom 0)
        discard (fn [ugen seq] (let [isugen? (fn [x] (= ugen x))] (swap! seq (fn [oldseq] (remove isugen? oldseq)))))]
    (do
      (doseq [node ugenlist]
         (let [ugenkids (filter (fn [x] (map? (val x))) (:arg-map node))]
            (doseq [kid (vals ugenkids)]
               (discard kid output))))
  ;now we need to remove the taps
      (doseq [proot @output]
        (if (= (:cmd-name (:arg-map proot)) "/overtone/tap")
           ;then it's a tap, so
           (do
              (discard proot output)
              (let [tapped (:in (:arg-map (first (:values (:arg-map proot))))) tapname (nth (reverse (keys (:taps (synth)))) @numtaps) tapfreq (:freq (:arg-map (:trig (:arg-map proot))))]
                (u/update properties tapped {:tapped? true :tapname tapname :tapvalue (atom (vec (replicate (* 2 tapfreq) 0))) :tapfreq tapfreq})
                ;why the reverse? because find_roots runs through the list of ugens backwards, and thus starts with the last tap
                ;magic number in replicate is secs of output to show
               (add-watch (get (:taps (synth)) tapname) ::my-key
                 (fn [k r o n] (swap! (:tapvalue (@properties tapped)) (fn [r v] (conj (subvec r 1) v)) n))
               ) 
                ) ;let tapped runs out here
              (swap! numtaps + 1)
           )))
  ;now finally we can output the list in peace
      (reverse @output))))

(defn fill_widths [root]
 ;Initialise the properties map with a list of associations of nodes of the tree with their width as drawn (cellwidth) and the width of their subtree (treewidth).
 (let [ugenkids (filter (fn [x] (map? (val x))) (:arg-map root))
       defaultcellwidth (if (get (@properties root) :tapped?) 150 100) ;nodes with oscilloscopes display wider
       controldefaultcellwidth 150 ;nodes that are Control Proxies for parameters to the whole synthesiser display wider
       rootwidth (if (= (subs (str root) 0 47) "overtone.sc.machinery.ugen.sc_ugen.ControlProxy") controldefaultcellwidth (max defaultcellwidth (* 50 (count ugenkids))))]
  (if (zero? (count ugenkids))
    ;true: base case, no children
    (u/update properties root {:cellwidth rootwidth :treewidth rootwidth :expands false})
    ;false: has children
    (do 
     (doseq [child (vals ugenkids)]
      (fill_widths child)) ;recurse down the tree
     ;now the :cellwidth and :treewidth of all kids have been filled in
     (let [subtreesum (reduce + (map :treewidth (map @properties (vals ugenkids))))
           subtreewidth (+ subtreesum (* interboxspace (- (count ugenkids) 1)))]
     (u/update properties root {:cellwidth rootwidth :treewidth (max rootwidth subtreewidth) :expands false}))))))

(declare draw_tree)

(defn draw_forest [roots x y]
  (let [currx (atom x)]
     (doseq [root roots]
        (draw_tree root @currx y)
        (swap! currx + (+ interboxspace (get (@properties root) :treewidth)))
     )
  )
)

(defn draw_tree [r x y]
 ;Draw the tree with root r such that its left edge is at x and its top is at y.
 (let [tw (get (@properties r) :treewidth)
       ugenkids (filter (fn [x] (map? (val x))) (:arg-map r))
       subtreespace (atom x)]
  (db/draw_box r (+ (* 0.5 tw) x) y properties) ;draw a box for the root
  (doseq [[child count] (map list (reverse (vals ugenkids)) (range 0 (count ugenkids)))]
   ;so now count stores a from-0 count of which child we're on
   (draw_tree child @subtreespace (+ 20 (get-in (@properties r) [:tlbr :bottom]))) ;recursion
   (let [childcentrex (+ (* 0.5 (get (@properties child) :cellwidth))(get-in (@properties child) [:tlbr :left]))
         childtopy (get-in (@properties child) [:tlbr :top])
         rootleftx (get-in (@properties r) [:tlbr :left])
         rootbottomy (get-in (@properties r) [:tlbr :bottom])]
   (q/line childcentrex childtopy (+ (* 50 count) (+ 25 rootleftx)) rootbottomy)) ;draw line from child to parent
   (swap! subtreespace + interboxspace)
   (swap! subtreespace + (get (@properties child) :treewidth))))) ;update subtreespace: this is a record of the sum of the widths of the subtrees that have been drawn so far, meaning that using this number as the left-hand edge of the next subtree to be drawn ensures the subtree separation property holds

(defn setup []
  (q/smooth)
  (q/frame-rate 1)
  (reset! roots (find_roots foo))
  (doseq [root @roots]
    (fill_widths root))
)

(defn draw []
  (q/background 200)
  (q/fill 0 0 0)
  (q/text (str "Structure of synthesiser " (:name foo)) 150 20)
  (q/text (str "W, A, S, D to move, I/O to zoom in/out") 150 40)
  (q/text (str "Click on boxes to see more, hover for documentation") 150 60)
  (q/scale @snzu/zoom)
  (q/with-translation [@snzu/xoffset @snzu/yoffset]
    (draw_forest @roots 50 100)
  )
  (q/scale (/ 1 @snzu/zoom))
  (draw-docs)
)

(defn mouse-press []
  (let [agenter (c2e/identify (- (/ (q/mouse-x) @snzu/zoom) @snzu/xoffset) (- (/ (q/mouse-y) @snzu/zoom) @snzu/yoffset) properties)]
   (if (not= agenter :nil)
    (do 
     (let [expandable (or (get (@properties agenter) :tapped?) (not= 0 (count (remove map? (:args agenter)))))
           newxp (not (get (@properties agenter) :expands))
           xp (if expandable newxp false)] ;can only expand a box if there is info to show
     (u/update properties agenter {:expands xp}))))))

(defn mouse-move []
  (let [agenter (c2e/identify (- (/ (q/mouse-x) @snzu/zoom) @snzu/xoffset) (- (/ (q/mouse-y) @snzu/zoom) @snzu/yoffset) properties)]
   (if (not= agenter :nil)
    (do 
     (let [docs (:full-doc (:spec agenter))] ;can only expand a box if there is info to show
     (reset! docstring (str docs))
     (reset! doccoords [(q/mouse-x) (q/mouse-y)])
     )) ;end of do
    (do (reset! docstring ""))
    ) ;end of if
 ))

(q/defsketch exampler :setup setup :draw draw :size [1000 2000] :key-typed snzu/key-press :mouse-pressed mouse-press :mouse-moved mouse-move)
