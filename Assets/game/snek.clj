(ns game.snek
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    game.std)
  (:import [UnityEngine GameObject]))

(def direction (atom [0 0 1]))
(def controlls (atom nil))

(defn start [o]
  (game.std/base-vr)
  (clone! :sun)
  (clone! :grid)
  (reset! controlls (clone! :snek/controlls)))


(defn hover [o c]
  (timeline* (tween {:material {:color (color 0 1 0)}} o 0.2)))

(defn unhover [o c]
  (timeline* (tween {:material {:color (state o :color)}} o 0.2)))

(defn control-click [o h]
  (reset! direction (state o :dir))
  (dorun (map #(set-state! % :color (color 1 1 1)) (children @controlls)))
  (set-state! o :color (color 1 0 0))
  (dorun (map #(timeline* (tween {:material {:color (state % :color)}} % 0.05))
    (children @controlls))))

'(set-state! (the down) :dir [0 -1 0])
'(set-state! (the arrow) :on-trigger-exit #'unhover)

'(map 
  #(set-state! (object-named (str %)) :trigger-clicked #'control-click)
  '[north south east west up down])

'(start nil)

