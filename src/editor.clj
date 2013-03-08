;(defsynth sin-osc-bus [out-bus 0]
;  (out out-bus (sin-osc 440) ))

;(defsynth send-out [in-bus 3]
;  (out 0 (* 2 (in in-bus))))

;(def my-bus (audio-bus))
;(sin-osc-bus :position :head my-bus)
;(send-out :position :tail my-bus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsynth ConstBus [out-bus 0 const 0]
  (out out-bus (+ const (sin-osc 0)))
)
(defsynth SinWrap [out-bus 0 in-bus-freq 3] ;ignore phase for now
  (out out-bus (sin-osc (in in-bus-freq)))
)
(defsynth SawWrap [out-bus 0 in-bus-freq 3] ;ignore phase for now
  (out out-bus (saw (in in-bus-freq)))
)
(defsynth MultWrap [out-bus 0 in-bus-left 3 in-bus-right 4]
  (out out-bus (* (in in-bus-left) (in in-bus-right)))
)
(defsynth AddWrap [out-bus 0 in-bus-left 3 in-bus-right 4]
  (out out-bus (+ (in in-bus-left) (in in-bus-right)))
)
(defsynth LPFWrap [out-bus 0 in-bus-signal 3 in-bus-freq 4]
  (out out-bus (lpf (in in-bus-signal) (in in-bus-freq)))
)
(defn KillAll [upper 45] 
  (kill (vec (range 25 upper)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STRUCTURE OF SYNTH
;
;
;                      LPF
;                      7| \8
;                      saw \
;                       |6  \
;                      add   \
;                     /4  \5  \
;             multiply     660 \
;             /2  \3            \
;      sin-osc    220           600
;         |1
;         1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Define busses
(def out-bus 0)
(def patch1 (audio-bus))
(def patch2 (audio-bus))
(def patch3 (audio-bus))
(def patch4 (audio-bus))
(def patch5 (audio-bus))
(def patch6 (audio-bus))
(def patch7 (audio-bus))
(def patch8 (audio-bus))

(ConstBus :position :head patch1 1)
(SinWrap patch2 patch1)
(ConstBus patch3 220)
(MultWrap patch4 patch2 patch3)
(ConstBus patch5 660)
(AddWrap patch6 patch4 patch5)
(SawWrap patch7 patch6)
(ConstBus patch8 600)
(LPFWrap out-bus patch7 patch8)
