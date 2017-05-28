(ns game.gun
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    game.std
    hard.physics)
  (:import [UnityEngine GameObject]))

(def bullet-speed 25)

(defn destroy-ball [o]
  (clone! :gun/splode (>v3 o))
  (timeline*
    (timeline*
      (tween {:local {:scale (v3 0)}} o 0.2 :pow4)
      #(destroy o))))

(defn move-forward [o]
  (let [v (.position (.transform o))
        up (.up (.transform o))
        step (v3+ v (v3* up (∆ bullet-speed)))]
    (set! (.position (.transform o)) step)
    (when-let [h (hit v up (∆ bullet-speed))] 
      (destroy-ball (gobj (.collider h)))
      (destroy o))))

(defn fire-gun! [c e]
  (let [o (gobj c)
        muzzle (child-named o "muzzle")
        bullet (clone! :gun/bullet (>v3 muzzle))]
    (set! (.rotation (.transform bullet))
          (.rotation (.transform muzzle)))
    (hook+ bullet :update #'move-forward)
    (timeline* #(haptic! c)#(haptic! c)#(haptic! c)#(haptic! c))
    (destroy bullet 4.0)))

(defn raygun! [hand] 
  (if-let [model (child-named hand "Model")]
    (.SetActive model false))
  (let [gun (clone! :gun/raygun)
        trigger (child-named gun "trigger")]
    (on-hand hand gun)
    (hook+ trigger :update
      (fn [o]
        (set! (.localEulerAngles  (.transform o)) 
              (v3 (* (.x (trigger-axis hand)) 50) 0 0))))
    (timeline* :loop
      #(if (.triggerPressed hand)
           (do (fire-gun! hand nil) nil)
           true)
      (wait 0.2))))

(defn trip []
  (let [background (clone! :empty)]
    (dorun 
      (for [i (range (+ 2 (rand-int 3)))]
        (let [o (clone! :icosphere)
              r (v3 (?f -0.06 0.06)(?f -0.06 0.06)(?f -0.06 0.06))] 
          (parent! o background)
          (gradiate o)
          (timeline* #(rotate! o r)))))
    (local-scale! background (v3 20))))

(defn z-positive-on-sphere [n]
  (let [v (?on-sphere n)]
    (if (> (.z v) 0) v (z-positive-on-sphere n))))

(defn sphere! []
  (let [pos (z-positive-on-sphere 9.0)
        o (clone! :gun/sphere pos)]
    (set! (.localScale (.transform o)) (v3 0))
    (timeline*
      (tween {:local {:scale (v3 1)}} o 0.3 :pow4)))
  nil)

(defn start [o]
  (game.std/base-vr)
  (clone! :gun/sun)
  (trip)
  (raygun! (right))
  (raygun! (left))
  (let [session? (clone! :empty)]
    (timeline* :loop
      (wait 1)
      #(if (null-obj? session?) (abort!))
      #(sphere!))))





'(start nil)
