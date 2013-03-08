(ns scroll-n-zoom (:use quil.core
        [overtone.sc.machinery.ugen common doc]
        [overtone.live :exclude [tan atan sqrt scale line abs log atan2 round triangle mouse-button pow sin cos asin acos mouse-y mouse-x exp ln TWO-PI ceil floor]]))

;The structure of this code is mostly taken from the key-capture.clj program in quil's examples folder.

(def valid-keys {
  \w :up
  \s :down
  \a :left
  \d :right
  \i :in
  \o :out})

(def xoffset (atom 0))
(def yoffset (atom 0))
(def zoom (atom 1))

(def xchanges {:up 0
  :down 0
  :left -20
  :right 20
  :in 0
  :out 0
  :null 0})

(def ychanges {:up -20
  :down 20
  :left 0
  :right 0
  :in 0
  :out 0
  :null 0})

(def zooms {:up 1
  :down 1
  :left 1
  :right 1
  :in (/ 4 3)
  :out 0.75
  :null 1})

(defn key-press []
  (let [raw-key (raw-key)
    the-key-code (key-code)
    the-key-pressed (if (= processing.core.PConstants/CODED (int raw-key)) the-key-code raw-key)
     xchange (xchanges (get valid-keys the-key-pressed :null))
     ychange (ychanges (get valid-keys the-key-pressed :null))
     zoomer (zooms (get valid-keys the-key-pressed :null))]
    (swap! yoffset + ychange)
    (swap! xoffset + xchange)
    (swap! zoom * zoomer)
  )
)
