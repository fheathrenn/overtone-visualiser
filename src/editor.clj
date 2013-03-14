(ns editor
  (:require [draw-box-editor :as db]
            [scroll-n-zoom :as snzu]
            [click-to-expand :as c2e]
            [update :as u]
            [quil.core :as q] :reload)
  (:use [overtone.sc.machinery.ugen common doc]
        [overtone.live]))

(def mysynth (atom {:name "foo"
 :ugens (seq [
  {:ugen-id 1
   :name "out"
   :arg-map{:signals [:id 2] :bus [:const 0]}}
  {:ugen-id 2
   :name "saw"
   :arg-map{:freq [:id 3]}}
  {:ugen-id 3
   :name "+"
   :arg-map{:a [:const 660] :b [:id 4]}}
  {:ugen-id 4
   :name "*"
   :arg-map{:a [:const 220] :b [:id 5]}}
  {:ugen-id 5
   :name "sin-osc"
   :arg-map{:freq [:nil] :phase [:const 0]}} ]) }))
;corresponds to (defsynth foo [] (out 0 (saw (+ 660 (* 220 (sin-osc 1))))))

(def properties (atom {
{:ugen-id 1
   :name "out"
   :arg-map{:signals [:id 2] :bus [:const 0]}} {:cellwidth 100 :x 60 :y 10 :expands false}
  {:ugen-id 2
   :name "saw"
   :arg-map{:freq [:id 3]}} {:cellwidth 100 :x 60 :y 100 :expands false}
  {:ugen-id 3
   :name "+"
   :arg-map{:a [:const 660] :b [:id 4]}} {:cellwidth 100 :x 200 :y 130 :expands false}
  {:ugen-id 4
   :name "*"
   :arg-map{:a [:const 220] :b [:id 5]}} {:cellwidth 100 :x 160 :y 240 :expands false}
  {:ugen-id 5
   :name "sin-osc"
   :arg-map{:freq [:nil] :phase [:const 0]}} {:cellwidth 100 :x 60 :y 340 :expands false}
}))
;so properties maps our stunted ugens to a :cellwidth and :expands and :x and :y

(defn draw_ugen [ugen]
 (let [x (:x (get @properties ugen))
       y (:y (get @properties ugen))
       ugenkids (filter (fn [x] (= :id (first (val x)))) (:arg-map ugen))]
  (do
  ;first: draw the box
  (db/draw_box ugen x y properties)
  ;now: draw links
  (let [currx (atom (+ (:left (:tlbr (get @properties ugen))) 25))]
  (doseq [kid ugenkids]
    ;kid is of the form [:argname [:id ugen-id]]
    (let [ugen-id (last (last kid))
          ugen-arg (first (filter (fn [x] (= ugen-id (:ugen-id x))) (:ugens @mysynth)))]
      (q/line @currx (:bottom (:tlbr (get @properties ugen))) (:x (get @properties ugen-arg)) (:y (get @properties ugen-arg)))
      (swap! currx + 50)
))))))

(defn setup []
  (q/smooth)
  (q/frame-rate 1)
)

(defn draw []
  (q/background 200)
  (q/fill 0 0 0)
  (q/text (str "Structure of synthesiser " (:name @mysynth)) 150 20)
  (q/scale @snzu/zoom)
  (q/with-translation [@snzu/xoffset @snzu/yoffset]
    (doseq [ugen (:ugens @mysynth)]
      (draw_ugen ugen)
    )
  )
)

(def active (atom {:value false}))

(defn handle-click [ugen x y]
  ;Does the actions necessary to handle a click on the box for ugen at coordinates x and y relative to the top-left hand corner of the box.
(if (c2e/interpole x y {:top 3 :bottom 15 :left 3 :right 15}) ;magic numbers for being in move box
 ;then we're in the move box
 (do 
   (reset! active {:value true :action "move" :ugen ugen}))
 ;else
 ;it's not a move...
 (let [top (:top (:tlbr (@properties ugen)))
       bottom (:bottom (:tlbr (@properties ugen)))
       left (:left (:tlbr (@properties ugen)))
       right (:right (:tlbr (@properties ugen)))]
  (if (c2e/interpole x y {:top (- (- bottom 25) top) :bottom (- bottom top) :left 0 :right (- right left)})
  ;then we're editing an input box!
  (do
   (let [number (/ (- x (mod x 50)) 50)
         ugenargs (filter (fn [x] (= :id (first (val x)))) (:arg-map ugen))]
    (if (> (count ugenargs) number)
    ;then
    (let [arg (key (nth ugenargs number))]
      (reset! active {:value true :action "input" :ugen ugen :arg arg})
    )
  )))
  ;else
  ;just expand
  (do 
     (let [expandable (not= 0 (count (remove (fn [x] (= :id (first (val x)))) (:arg-map ugen))))
           newxp (not (get (@properties ugen) :expands))
           xp (if expandable newxp false)] ;can only expand a box if there is info to show
     (u/update properties ugen {:expands xp})))
))))

(defn mouse-press []
(let [x (- (/ (q/mouse-x) @snzu/zoom) @snzu/xoffset)
      y (- (/ (q/mouse-y) @snzu/zoom) @snzu/yoffset)
      agenter (c2e/identify x y properties)]
(if (:value @active)
  ;then need to finish action
  (if (= (:action @active) "move")
   ;then
   (do 
    (u/update properties (get @active :ugen) {:x x :y y})
    (reset! active {:value false}))
   ;else
   (if (= (:action @active) "input")
    ;then
   (if (not= agenter :nil)
    (do
      ;switch the input
      (let [input-id (get agenter :ugen-id)
           ugen (:ugen @active)
           arg-to-switch (:arg @active)
           newugen (merge ugen {:arg-map (merge (:arg-map ugen) {arg-to-switch [:id input-id]})})
           oldugens (:ugens @mysynth)
           newugens (atom (seq []))]
      (doseq [u oldugens]
      (if (not= ugen u)
        (swap! newugens (fn [oldval x] (cons x oldval)) u) ;add old ugens to new sequence
        (swap! newugens (fn [oldval x] (cons x oldval)) newugen) ;unless it's the ugen we're replacing
      ))
      ;now we need to substiute the new sequence for the old one in the synthdef
      (swap! mysynth (fn [oldsynth] (merge oldsynth {:ugens @newugens})))
      ;it's not over yet! we also need to update that ugen entry in properties... or, more simply for the moment, just create a new entry for the new ugen :P
      (swap! properties (fn [oldprop] (let [postupdate (get oldprop ugen)] (assoc oldprop newugen postupdate))))
      ;TODO: Implement this properly
      (reset! active {:value false})
    )
   ) ;end of do
   ;if agenter IS nil we've not clicked on anything so we should just do nothing
   (reset! active {:value false})
  )))
  ;else
   (if (not= agenter :nil)
       (let [top (:top (get (@properties agenter) :tlbr))
             left (:left (get (@properties agenter) :tlbr))]
          (handle-click agenter (- x left) (- y top)))))))

(q/defsketch exampler :setup setup :draw draw :size [600 700] :key-typed snzu/key-press :mouse-pressed mouse-press)
