(ns game.snek
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    game.std)
  (:import [UnityEngine GameObject Quaternion Space]))


(def snek (atom []))
(def bodies (atom []))
(def dirs (atom []))

(defn update-game [o])

(defn place-segment [i o]
  (let [v (get @snek i)
        d (get @dirs i)
        front (direct-child-named o "snek-armature/front")
        rear (direct-child-named o "snek-armature/rear")
        v2 (get @snek (dec i) v)
        d2 (get @dirs (dec i) d)]
  (set! (.position (.transform front)) v)
  (set! (.rotation (.transform front)) d)
  (.Rotate (.transform front) (v3 90 0 0))
  (set! (.position (.transform rear)) v2)
  
  (set! (.rotation (.transform rear)) d2)
  (.Rotate (.transform rear) (v3 90 0 0))
  ))

(defn inc-snek []
  (let [o (gobj (right))
        needed (- (count @snek)(count @bodies))]
    (swap! snek conj 
      (v3+ (last @snek)
        (v3* (.forward (.transform o)) 0.1)
        ))
    (swap! dirs conj (.rotation (.transform o)))
    (if (pos? needed)
      (dotimes [i needed] (swap! bodies conj (clone! :snek/snek))))
    #_(if (neg? needed)
      (dotimes [i (- needed)] (swap! bodies #(let [f (first %)] (destroy %) (vec (rest %))))))
    (dorun
      (map-indexed 
        (fn [i o]
          (place-segment i o))
        @bodies))
    (log "inc")
    ))

(defn start [o]
  (reset! snek [(v3 0 0 -1)])
  (reset! bodies [])
  (reset! dirs [(Quaternion.)])
  (game.std/base-vr)
  (clone! :sun)
  (clone! :grid)
  (let [session? (clone! :empty)]
    (hook+ session? :update #'update-game)
    (timeline* :loop
      (wait 0.3)
      #(if (null-obj? session?) (abort!))
      #(inc-snek))))

'(start nil)



