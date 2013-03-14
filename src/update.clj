(ns update)

(defn update [map ugen updated]
  ;Updates the value of the keys of (get map ugen) to the value of those keys in updated, leaving any untouched keys, er, untouched.
  (swap! map (fn [oldmap]
   (let [preupdate (get oldmap ugen) postupdate (merge preupdate updated)]
     (assoc oldmap ugen postupdate)))))
