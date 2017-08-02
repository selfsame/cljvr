(ns game.pingpong
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    game.std
    hard.physics)
  (:import [UnityEngine GameObject]))


(defn hide-controller [c]
 (.SetActive (first (children (gobj c))) false))

(defn hand-joint [c o]
  (let [mount (on-hand c (clone! :mount))
        joint (.AddComponent o UnityEngine.FixedJoint)]
    (on-hand c o)
    (set! (.parent (.transform o)) nil)
    (set! (.enablePreprocessing (cmpt o UnityEngine.FixedJoint)) false)
    (set! (.connectedBody (cmpt o UnityEngine.FixedJoint))
      (cmpt mount UnityEngine.Rigidbody))
    ))

(defn start [o]
  (game.std/base-vr)
  (dorun (map destroy (every toucher)))
  (clone! :grid)
  (clone! :sun)
  ;(hand-joint (left) (clone! :paddle))
  (hand-joint (right) (clone! :paddle))
  ;(hide-controller (left))
  (hide-controller (right))
  (hand+ (left) :trigger-clicked 
    (fn [c e] 
      (position! (clone! :ball) (>v3 (gobj c)))))
  (let [session? (clone! :empty)]
    ))

'(start nil)
