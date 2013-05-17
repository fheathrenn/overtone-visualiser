(ns fred-map-synth
  (:use [overtone.live])
  (:require [clojure.set :as set]))

;;This code by Sam Aaron

;; example original synth map
;; (def orig-fred-map
;;   {:name "foo"
;;    :ugens [{:ugen-id 1
;;             :name "out"
;;             :arg-map{:signals [:id 2] :bus [:const 0]}}
;;            {:ugen-id 2
;;             :name "saw"
;;             :arg-map{:freq [:id 3]}}
;;            {:ugen-id 3
;;             :name "+"
;;             :arg-map{:a [:const 660] :b [:id 4]}}
;;            {:ugen-id 4
;;             :name "*"
;;             :arg-map{:a [:const 220] :b [:id 5]}}
;;            {:ugen-id 5
;;             :name "sin-osc"
;;             :arg-map{:freq [:nil] :phase [:const 0]}}]})

;; modified version for use in this ns (see mk-fred-map)
;; (def fred-map
;;   {:name "foo"
;;    :ugens [{:ugen-id 1
;;             :name "out"
;;             :arg-map{:signals {:id 2} :bus {:const 0}}}
;;            {:ugen-id 2
;;             :name "saw"
;;             :arg-map{:freq {:id 3}}}
;;            {:ugen-id 3
;;             :name "+"
;;             :arg-map{:a {:const 660} :b {:id 4}}}
;;            {:ugen-id 4
;;             :name "*"
;;             :arg-map{:a {:const 220} :b {:id 5}}}
;;            {:ugen-id 5
;;             :name "sin-osc"
;;             :arg-map{:freq {} :phase {:const 0}}}]})

(defn update-info
  [orig-info]
  (cond
   (= 2 (count orig-info)) (apply hash-map orig-info)
   (= :nil (first orig-info)) {}
   :else (throw (Exception. (str "Unknown info form: " orig-info)))))

(defn update-arg-map
  [orig-arg-map]
  (reduce (fn [r [arg-name info]]
            (assoc r (keyword arg-name) (update-info info)))
          {}
          orig-arg-map))

(defn update-ugen
  [orig-ugen]
  (let [orig-arg-map (:arg-map orig-ugen)
        arg-map      (update-arg-map orig-arg-map)]
    (assoc orig-ugen :arg-map arg-map)))

(defn update-ugens
  [orig-ugens]
  (doall (map update-ugen orig-ugens)))

(defn mk-fred-map
  [orig-fred-map]
  (let [orig-ugens (:ugens orig-fred-map)
        ugens      (update-ugens orig-ugens)]
    (assoc orig-fred-map :ugens ugens)))

(defn find-ids
  [fred-map]
  (reduce (fn [r ug]
            (conj r (:ugen-id ug)))
          #{}
          (-> fred-map :ugens)))



(defn ugen-ids
  "returns a set of ids referenced by args in this ugen"
  [ugen]
  (reduce (fn [r [arg-name info]]
            (println "hi" info)
            (if-let [id (:id info)]
              (conj r id)
              r))
          #{}
          (:arg-map ugen)))

(defn find-used-ids
  [fred-map]
  (reduce (fn [r ug]
            (println "ug" ug)
            (let [ids (ugen-ids ug)]
              (if (empty? ids)
                r
                (apply conj r (ugen-ids ug)))))
          #{}
          (-> fred-map :ugens)))

(defn find-roots
  [fred-map]
  (let [ids      (find-ids fred-map)
        used-ids (find-used-ids fred-map)]
    (set/difference ids used-ids)))

(defn mk-ug
  [ug]
  (let [args (flatten (into [] (:resolved-arg-map ug)))]
    (apply (ns-resolve (find-ns 'overtone.sc.ugens) (symbol (:name ug)))
args)))

(defn get-ug
  [fred-map id]
  (some #(when (= id (:ugen-id %)) %) (:ugens fred-map)))

(defn construct-nested-ug
  [fred-map id]
  (let [ug   (get-ug fred-map id)
        args (:arg-map ug)
        new-args (into {} (map (fn [[k v]]
                                 (if (= {} v)
                                   [nil nil]
                                   [k (if-let [const-v (:const v)]
                                        const-v
                                        (construct-nested-ug fred-map (:id
v)))]))
                               args))
        new-args (dissoc new-args nil)]
    (assoc ug :arg-map new-args)))


(defn construct-ug-list
  ([nested-ug] (construct-ug-list nested-ug '()))
  ([nested-ug form]
     (when-not (:name nested-ug))
     (concat form
             (cons (symbol "overtone.core" (:name nested-ug)) (apply concat (map (fn [[k v]]
                                                          (if (number? v)
                                                            [k v]
                                                            [k
(construct-ug-list v)]))
                                                        (:arg-map
nested-ug)))))))

(defn convert [seqstyle] {:name (:name seqstyle) :ugens (vec (:ugens seqstyle))})

(defn demo-synth [demo-time orig-fred-map]
  (let [fm   (mk-fred-map orig-fred-map)
        root (first (find-roots fm))]
    (eval (cons 'overtone.core/run (cons demo-time (list (construct-ug-list (construct-nested-ug fm root))))))))

(def thissynth {:name "foo",
 :ugens (seq [ ;a sequence of ugens
  {:ugen-id 1,
     :name "out",
    :arg-map{"bus" [:const 0] "signals" [:id 2]}}
  {:ugen-id 2,
    :name "sin-osc",
    :arg-map{"freq" [:const 440] "phase" [:const 0]}}])})

(defn play [synth] (demo-synth 5 (convert synth)))
