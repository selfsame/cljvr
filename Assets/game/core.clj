(ns game.core
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    game.std)
  (:require
    game.repl
    game.snek
    game.gun
    clojure.core.server
    arcadia.internal.hook-help)
  (:import [UnityEngine GameObject]))

(defn add-ball [s e]
  (let [b (clone! :ball (>v3 (gobj s)))]
    (material-color! b (color 0 1 1))
    (destroy b 20.0)))

(defn scene-transition [f]
  #_(dorun (map 
    #(timeline* 
      (wait (?f 0.3))
      (tween {:local {:scale (v3 0)}} % (+ 0.5 (?f 0.2)))
      (fn [] (destroy %)))
    (remove #(#{"sun" "camera-rig" "grass"} (.name %)) @CLONED)))
  #_(timeline* (wait 1.0) (f nil))
  (f nil))

(defn hover [o]
  (timeline* :loop
    #(if (state o :hovered) nil (abort!))
    (tween {:local {:scale (v3 0.25)}} o 0.2 :pow2)
    (tween {:local {:scale (v3 0.2)}} o 0.2 :pow2)))

(defn game-on-hover-enter [o c]
  (set-state! o :hovered true)
  (hover o))

(defn game-on-hover-exit [o c]
  (set-state! o :hovered false))

(defn select-game [o h]
  (if-let [f (state o :game-fn)]
    (scene-transition (eval f))))

(defn start [o]
  (game.std/base-vr)
  (clone! :sun)
  (clone! :grass)
  (clone! :skyball)
  (clone! :game1)
  (clone! :game2)
  (clone! :pingpong))



'(hook+ (the hook) :start #'start)
'(start nil)
'(breakout 0)
'(set-state! (the pingpong) :game-fn #'game.pingpong/start)


