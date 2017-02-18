(ns game.core
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    game.vr))

(defn material-color! [o c]
  (set! (.color (.material (.GetComponent o UnityEngine.Renderer))) c))

(defn add-ball [s e]
  (let [b (clone! :ball (>v3 (gobj s)))]
    (material-color! b (color 1 0 0))
    (destroy b 10.0)))

(defn start [o]
  (clear-cloned!)
  (clone! :sun)
  (clone! :breakout-room)
  (clone! :camera-rig)
  (hand+ (right) :trigger-clicked #'add-ball))

'(hook+ (the hook) :start #'start)
'(start nil)