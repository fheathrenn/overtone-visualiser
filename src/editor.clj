(ns editor
  (:require [draw-box-editor :as db]
            [scroll-n-zoom :as snzu]
            [click-to-expand :as c2e]
            [update :as u]
            [fred-map-synth :as fms]
            [quil.core :as q] :reload)
  (:use [overtone.sc.machinery.ugen common doc]
        [overtone.live]))

(def mysynth (atom {:name "foo" :ugens (seq [])}))
;corresponds to (defsynth foo [] (out 0 (saw (+ 660 (* 220 (sin-osc 1))))))

(def properties (atom {}))
;so properties maps our stunted ugens to a :cellwidth and :expands and :x and :y

(def active (atom {:value false}))
(def mouse-xy (atom [100 100]))
(def ugen-id-counter (atom 0)) ;once i no longer have the scratch can start from 0
(defn get-new-id [] (swap! ugen-id-counter inc))

(defn draw_ugen [ugen]
 (let [x (:x (get @properties ugen))
       y (:y (get @properties ugen))
       ugenkids (filter (fn [x] (not= :const (first (val x)))) (:arg-map ugen))]
  (do
  ;first: draw the box
  (db/draw_box ugen x y properties)
  ;now: draw links
  (let [currx (atom (+ (:left (:tlbr (get @properties ugen))) 25))]
  (doseq [kid ugenkids]
    ;kid is of the form [:argname [:id ugen-id]]
    (let [ugen-id (last (last kid))
          ugen-arg (if (not= ugen-id :nil) (first (filter (fn [x] (= ugen-id (:ugen-id x))) (:ugens @mysynth))) nil)]
      (if (not= ugen-id :nil) (q/line @currx (:bottom (:tlbr (get @properties ugen))) (:x (get @properties ugen-arg)) (:y (get @properties ugen-arg))) ())
      (swap! currx + 50)
))))))

(defn playsynth [synth]
  ;need to check to see if any nils
  (if (= synth {:name "foo" :ugens (seq [])}) ;then it's empty
  (q/text "Synth empty!" 442 49)
  ;else
  (if (let [myor (fn [x y] (or x y))] (reduce myor (map (fn [x] (reduce myor x)) (map (fn [y] (map (fn [x] (= x [:nil])) y)) (map vals (map :arg-map (:ugens synth)))))))
  ;if this is true then there is a nil somewhere
  (q/text "Synth incomplete!" 442 49)
  (fms/play synth)
)))

;(reduce 'or (map (fn [x] (reduce 'or x)) (map (fn [y] (map (fn [x] (= x [:nil])) y)) (map vals (map :arg-map (:ugens synth))))))
;evaluates to true iff there is at least one nil in synth

(defn draw-add-menu [x y]
(q/with-translation [x y]
  (q/fill 255 245 100)
  (q/rect 0 0 400 200)
  (q/fill 255 255 255)
  (q/rect 100 35 20 20) ;+
  (q/rect 160 35 20 20) ;-
  (q/rect 220 35 20 20) ;*
  (q/rect 280 35 20 20) ;/
  (q/rect 80 75 100 25) ;sin-osc
  (q/rect 220 75 100 25) ;saw
  (q/rect 80 120 100 25) ;lpf
  (q/rect 220 120 100 25) ;out
  (q/rect 150 165 100 25) ;cancel
  (q/fill 0 0 0)
  (q/text "Add ugen" 160 20)
  (q/text "+" 105 50)
  (q/text "-" 165 50)
  (q/text "*" 225 50)
  (q/text "/" 285 50)
  (q/text "sin-osc" 90 90)
  (q/text "saw" 230 90)
  (q/text "lpf" 90 135)
  (q/text "out" 230 135)
  (q/text "cancel" 160 180)))

(defn delete-ugen [ugen]
  (let [ugen-id (:ugen-id ugen)]
  ;delete from synth
  (swap! mysynth (fn [t] {:name (:name t), :ugens  (remove (fn [x] (= x ugen)) (:ugens t)) }))
  ;the following behemoth goes into mysynth and replaces any references to the synth with [:nil]. whew!
  (swap! mysynth (fn [my] {:name "foo", :ugens (map (fn [t] (zipmap (keys t) (map (fn [v] (if (= (:arg-map t) v) (zipmap (rseq (vec (keys v))) (map (fn [v2] (if (= [:id ugen-id] v2) [:nil] v2)) (rseq (vec (vals v))))) v)) (vals t)))) (:ugens my))}))
  ;delete from properties
  (swap! properties (fn [t] (dissoc t ugen)))
  (swap! properties (fn [p] (zipmap (map (fn [t] (zipmap (keys t) (map (fn [v] (if (= (:arg-map t) v) (zipmap (rseq (vec (keys v))) (map (fn [v2] (if (= [:id ugen-id] v2) [:nil] v2)) (rseq (vec (vals v))))) v)) (vals t)))) (keys p)) (vals p))))
))

(defn name-to-symbol [x]
 (case x
   "sin-osc" sin-osc
   "lpf" lpf
   "saw" saw
   "out" out
   {:summary "A binary operator", :params (seq [{:name "a", :doc "Left-hand operand"} {:name "b", :doc "Right-hand operand"}])} ;for the binary op u gens, since there's no docs on these :(
))

(defn draw-edit-menu [x y]
 (q/with-translation [x y]
 (let [arg-map (:arg-map-curr @active)
       countargs (count arg-map)
       height (+ 100 (* 50 countargs))]
  (q/fill 255 245 100)
  (q/rect 0 0 400 height)
  (q/fill 255 255 255)
  (q/rect 30 (- height 30) 100 25) ;Save
  (q/rect 150 (- height 30) 100 25) ;Cancel
  (q/rect 270 (- height 30) 100 25) ;Delete
  (q/fill 0 0 0)
  (q/text "Save" 45 (- height 15)) ;Save
  (q/text "Cancel" 165 (- height 15)) ;Cancel
  (q/text "Delete" 285 (- height 15)) ;Delete
  (q/text (:name (:ugen @active)) 25 20)
  (q/text (:summary (name-to-symbol (:name (:ugen @active)))) 25 35)
  (let [counter (atom 0)]
   (doseq [args arg-map]
     (q/fill 0 0 0)
     (q/text (name (key args)) 50 (+ 60 (* @counter 50)))
     (q/text (first (filter (fn [x] (not= x nil)) (map (fn [x] (if (= (:name x) (name (key args))) (:doc x))) (:params (name-to-symbol (:name (:ugen @active))))))) 50 (+ 80 (* @counter 50)))
     (let [type (first (val args))
           down (* 50 @counter)
           varcolour (if (or (= type :id) (= type :nil)) [255 255 150] [255 255 255])
           constcolour (if (= type :const) [255 255 150] [255 255 255])]
      (apply q/fill varcolour)
      (q/rect 110 (+ 40 down) 80 25)
      (apply q/fill constcolour)
      (q/rect 200 (+ 40 down) 80 25)
      (q/fill 0 0 0)
      (q/line [200 (+ 52.5 down)] [260 (+ 52.5 down)]) 
      (q/line [212 (+ 40 down)] [212 (+ 65 down)]) 
      (q/line [224 (+ 40 down)] [224 (+ 65 down)]) 
      (q/line [236 (+ 40 down)] [236 (+ 65 down)]) 
      (q/line [248 (+ 40 down)] [248 (+ 65 down)]) 
      (q/line [260 (+ 40 down)] [260 (+ 65 down)]) 
      (q/text "0 1 2 3 4" 202 (+ 50 down))
      (q/text "5 6 7 8 9" 202 (+ 63 down))
      (q/text "X" 265 (+ 60 down))
      (q/text "Variable" 115 (+ 56 down))
      ;(q/text "Constant" 205 (+ 55 down))
      (q/text (if (= type :const) (str (second (val args))) "______") 285 (+ 62 down))
     )
     (swap! counter inc)
  ))
)))

(defn handle-click-const-nums [x y arg]
 (do 
 (if (not= (first (get (:arg-map-curr @active) arg)) :const)
    (u/update active :arg-map-curr {arg [:const 0]})
 ) ;if not const, make const 0
 (cond
  (> x 60) ;we're in the reset band
  (u/update active :arg-map-curr {arg [:const 0]})
  (<= y 12.5)
  (let [pressed (quot x 12) curr (last (get (:arg-map-curr @active) arg))] 
    (u/update active :arg-map-curr {arg [:const (+ pressed (* 10 curr))]})) 
  (> y 12.5)
  (let [pressed (+ 5 (quot x 12)) curr (last (get (:arg-map-curr @active) arg))] 
    (u/update active :arg-map-curr {arg [:const (+ pressed (* 10 curr))]})) 
 )
))

(defn handle-click-edit-menu [x y]
 (let [arg-map (:arg-map-curr @active) ;values of arguments as shown in menu
       countargs (count arg-map)
       height (+ 100 (* 50 countargs))]
 (do
  (cond
  (c2e/interpole x y {:left 150 :top (- height 30) :right 250 :bottom (- height 5)}) ;cancel button, then...
  (reset! active {:value false}) ;close this dowwwwwn
  (c2e/interpole x y {:left 270 :top (- height 30) :right 370 :bottom (- height 5)}) ;delete button, then...
  (do (delete-ugen (:ugen @active)) (reset! active {:value false})) ;delete & close
  (c2e/interpole x y {:left 30 :top (- height 30) :right 130 :bottom (- height 5)}) ;save button, then...
  (do 
   (let [ugen (:ugen @active)
           newugen (merge ugen {:arg-map arg-map}) ;construct new ugen
           oldugens (:ugens @mysynth)
           newugens (atom (seq []))
           nonconstsnum (count (filter (fn [x] (not= (first (second x)) :const) ) arg-map))
           newcellwidth (max 100 (* 50 nonconstsnum))]
      (doseq [u oldugens]
      (if (not= ugen u)
        (swap! newugens (fn [oldval x] (cons x oldval)) u) ;add old ugens to new sequence
        (swap! newugens (fn [oldval x] (cons x oldval)) newugen) ;unless it's the ugen we're replacing
      ))
      ;now we need to substitute the new sequence for the old one in the synthdef
      (swap! mysynth (fn [oldsynth] (merge oldsynth {:ugens @newugens})))
      ;now update that ugen entry in properties
      (swap! properties (fn [r o v] (let [value (r o)] (merge (dissoc r o) {v (merge value {:cellwidth newcellwidth})}))) ugen newugen)
      (reset! active {:value false}) ;close
    )) ;save & close
   :else
   (let [index_pre (let [casted (quot (- y 40) 25)] (if (and (or (pos? casted) (zero? casted)) (even? casted)) (/ casted 2) nil))
        index (if (not= index_pre nil) (if (<= (count arg-map) index_pre) nil index_pre) nil)] ;this determines if one of the buttons corresponding to parameter values was clicked and, if so, which one
   (if (not= index nil) ;we have a click in a valid row
   (let [arg (first (nth (seq arg-map) index))]
   (cond
     (and (<= 110 x) (>= 190 x)) ;then it's a click on variable
     (u/update active :arg-map-curr {arg [:nil]})
     (and (<= 200 x) (>= 280 x)) ;then it's a click on constant
     (handle-click-const-nums (- x 200) (- y (+ 40 (* 50 index))) arg) ;call function to handle clicks on the number pad
   ))))))))

(defn handle-click-add-menu [x y]
 (let [variables (atom {})]
 (do
  (cond
  (c2e/interpole x y {:left 150 :top 165 :right 250 :bottom 190}) ;cancel button, then...
  () ;indicate we should close
  (c2e/interpole x y {:left 100 :top 35 :right 120 :bottom 55}) ;+
  (reset! variables {:name "+" :arg-map{:a [:nil] :b [:nil]}})
  (c2e/interpole x y {:left 160 :top 35 :right 180 :bottom 55}) ;-
  (reset! variables {:name "-" :arg-map{:a [:nil] :b [:nil]}})
  (c2e/interpole x y {:left 220 :top 35 :right 240 :bottom 55}) ;*
  (reset! variables {:name "*" :arg-map{:a [:nil] :b [:nil]}})
  (c2e/interpole x y {:left 280 :top 35 :right 300 :bottom 55}) ;/
  (reset! variables {:name "/" :arg-map{:a [:nil] :b [:nil]}})
  (c2e/interpole x y {:left 80 :top 75 :right 180 :bottom 100}) ;sin-osc
  (reset! variables {:name "sin-osc" :arg-map (let [args (map (fn [x] (get x :name) ) (:params sin-osc))] (zipmap (vec args) (replicate (count args) [:nil])))})
  (c2e/interpole x y {:left 220 :top 75 :right 320 :bottom 100}) ;saw
  (reset! variables {:name "saw" :arg-map (let [args (map (fn [x] (get x :name) ) (:params saw))] (zipmap (vec args) (replicate (count args) [:nil])))})
  (c2e/interpole x y {:left 80 :top 120 :right 180 :bottom 145}) ;lpf
  (reset! variables {:name "lpf" :arg-map (let [args (map (fn [x] (get x :name) ) (:params lpf))] (zipmap (vec args) (replicate (count args) [:nil])))})
  (c2e/interpole x y {:left 220 :top 120 :right 320 :bottom 145}) ;out
  (reset! variables {:name "out" :arg-map (let [args (map (fn [x] (get x :name) ) (:params out))] (zipmap (vec args) (replicate (count args) [:nil])))})
) ;end of cond
  (case @variables 
   {} (reset! active {:value false})
   ;default case
   (let [newugen (merge {:ugen-id (get-new-id)} @variables)]
     (do
   ;generate the ugen to add and then add it in some highly convoluted manner
   ;then set active to move, thereby closing the menu, and having this highlighted thing following us around
     (swap! mysynth (fn [oldsynth] (merge oldsynth {:ugens (cons newugen (:ugens oldsynth))})))
     (swap! properties (fn [oldprop] (merge oldprop {newugen {:cellwidth 100 :x 300 :y 140 :expands false :highlight true}})))
     (reset! active {:value true :action "move" :ugen newugen})))))))

(defn setup []
  (q/smooth)
  (q/frame-rate 1)
)

(defn draw []
  (q/background 200)
  (q/fill 255 255 255)
  (q/rect 400 10 40 20)
  (q/rect 400 35 40 20)
  (q/fill 0 0 0)
  (q/text (str "Structure of synthesiser " (:name @mysynth)) 150 20)
  (q/text (str "Click \"Add...\" to add ugens.\nClick + in top left to move, click \\ in top right to edit.\nTo manage flow between boxes, click on an input box and then the ugen you want to feed into it.\nw, a, s, d to move, i/o to zoom in/out.") 150 60)
  (q/text (str "Add...") 402 24)
  (q/text (str "Play") 402 49)
  (q/scale @snzu/zoom)
  (q/with-translation [@snzu/xoffset @snzu/yoffset]
    (doseq [ugen (:ugens @mysynth)]
      (draw_ugen ugen)
    )
    (if (:value @active) ;there is an action occurring
      (case (:action @active)
       "addmenu" (draw-add-menu 100 100)
       "editmenu" (draw-edit-menu 100 100)
       "input" (do (q/fill 0 0 0) (q/line (:xy @active) @mouse-xy) )
       () ;default action
    ))
  )
)

(defn handle-click [ugen x y]
  ;Does the actions necessary to handle a click on the box for ugen at coordinates x and y relative to the top-left hand corner of the box.
(cond
 (c2e/interpole x y {:top 3 :bottom 15 :left 3 :right 15}) ;magic numbers for being in move box
 ;then we're in the move box
 (do 
   (reset! active {:value true :action "move" :ugen ugen})
   (u/update properties ugen {:highlight true}))
 ;it's not a move...
 ;next condition:
 (c2e/interpole x y {:top 3 :bottom 15 :left (- (:cellwidth (@properties ugen)) 15) :right (- (:cellwidth (@properties ugen)) 3)})
 ;open the edit menu!
 (reset! active {:value true :action "editmenu" :ugen ugen :arg-map-curr (:arg-map ugen)})
 ;next condition:
 (let [top (:top (:tlbr (@properties ugen)))
       bottom (:bottom (:tlbr (@properties ugen)))
       left (:left (:tlbr (@properties ugen)))
       right (:right (:tlbr (@properties ugen)))]
  (c2e/interpole x y {:top (- (- bottom 25) top) :bottom (- bottom top) :left 0 :right (- right left)}))
 ;then we're editing an input box!
 (do
   (let [number (/ (- x (mod x 50)) 50)
         ugenargs (remove (fn [x] (= :const (first (val x)))) (:arg-map ugen))]
    (if (> (count ugenargs) number)
    ;then
    (let [arg (key (nth ugenargs number))]
      (reset! active {:value true :action "input" :ugen ugen :arg arg :xy [(+ (:left (:tlbr (@properties ugen))) 25 (* 50 number)) (:bottom (:tlbr (@properties ugen)))]})
      (u/update properties ugen {:highlight true})))))
 ;else
 ;just expand
 :else
 (do 
     (let [expandable (not= 0 (count (filter (fn [x] (= :const (first (val x)))) (:arg-map ugen))))
           newxp (not (get (@properties ugen) :expands))
           xp (if expandable newxp false)] ;can only expand a box if there is info to show
     (u/update properties ugen {:expands xp})))))

(defn mouse-press []
(let [x (- (/ (q/mouse-x) @snzu/zoom) @snzu/xoffset)
      y (- (/ (q/mouse-y) @snzu/zoom) @snzu/yoffset)
      agenter (c2e/identify x y properties)]
(if (:value @active)
  ;then need to finish action
  (case (:action @active)
   "addmenu"
   (handle-click-add-menu (- x 100) (- y 100))
   ;next case
   "editmenu"
   (handle-click-edit-menu (- x 100) (- y 100))
   ;next case
   "move"
   (do 
    (u/update properties (get @active :ugen) {:x x :y y :highlight false})
    (reset! active {:value false}))
   ;next case
   "input"
   (if (not= agenter :nil)
    (do
      (u/update properties (:ugen @active) {:highlight false})
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
      ;now update that ugen entry in properties
      (swap! properties (fn [r o v] (let [value (r o)] (merge (dissoc r o) {v value}))) ugen newugen)
      (reset! active {:value false})
    )
   ) ;end of do
   ;if agenter IS nil we've not clicked on anything -> reset 
   (do 
   (u/update properties (:ugen @active) {:highlight false})
   (reset! active {:value false}))
  ))
  ;else
   (if (not= agenter :nil)
       (let [top (:top (get (@properties agenter) :tlbr))
             left (:left (get (@properties agenter) :tlbr))]
          (handle-click agenter (- x left) (- y top)))
   ;but you could have clicked on the menu
   (if (c2e/interpole x y {:top 10 :bottom 30 :left 400 :right 440}) ;then you've clicked the menu
      (reset! active {:value true :action "addmenu"})
   ;but you could have clicked play
   (if (c2e/interpole x y {:top 35 :bottom 55 :left 400 :right 440})
      (playsynth @mysynth)
   ;now you genuinely have done nothing
   )
   )
 ))))

(defn mouse-move []
(let [x (- (/ (q/mouse-x) @snzu/zoom) @snzu/xoffset)
      y (- (/ (q/mouse-y) @snzu/zoom) @snzu/yoffset)]
(if (:value @active)
  ;then need to finish action
  (cond
   (= (:action @active) "move")
   ;then
   (do 
    (u/update properties (get @active :ugen) {:x x :y y}))
   (= (:action @active) "input")
   ;then
   (reset! mouse-xy [x y])
  )
())))

(q/defsketch exampler :setup setup :draw draw :size [1200 700] :key-typed snzu/key-press :mouse-pressed mouse-press :mouse-moved mouse-move)
