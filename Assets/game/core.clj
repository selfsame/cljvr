(ns game.core
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    game.std)
  (:require
    arcadia.repl
    game.repl)
  (:import [UnityEngine GameObject]))

(defn material-color! [o c] (set! (.color (.material (.GetComponent o UnityEngine.Renderer))) c))


(defn add-ball [s e]
  (let [b (clone! :ball (>v3 (gobj s)))]
    (material-color! b (color 0 1 1))
    (destroy b 20.0)))




(defn start [o]
  (game.std/base-vr)
  (clone! :sun)
  (clone! :grid)
  (clone! :game1))


(defn breakout [o]
  (game.std/base-vr)
  (clone! :sun)
  (clone! :grid)
  (clone! :skyball)
  (clone! :grass))


'(hook+ (the hook) :start #'start)
'(start nil)
'(breakout 0)
'(set-state! (the game1) :fn nil)