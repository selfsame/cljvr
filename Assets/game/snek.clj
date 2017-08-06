(ns game.snek
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    hard.math
    hard.mesh
    tween.core
    game.vr
    game.std)
  (:require
    [magic.api :as m])
  (:import [UnityEngine GameObject Transform Quaternion Space Mathf Mesh Vector3]
    Curve [Hard Helper]
    Snek))

(declare start)

(def snek-cnt (atom 7))
(defonce VS (atom nil))
(defonce DS (atom nil))
(defonce head (atom nil))
(defonce curve (atom nil))
(def segments (atom []))
(def bones (atom nil))

(defn prime-snek [o]
  (let [o (gobj o)
        step (* 1 UnityEngine.Time/deltaTime)
        cnt (* @snek-cnt 10)
        vs (make-array Vector3 cnt)
        rs (make-array Quaternion cnt)]
    (dotimes [i cnt]
      (aset vs i 
        (v3+ (.position (.transform o))
             (v3* (.forward (.transform o)) (* i (- step)))))
      (aset rs i (.rotation (.transform o))))
    [vs rs]))
        
(defn inc-snek []
  (let [o (gobj (right))
        -VS @VS
        cnt (* @snek-cnt 10) ;70
        step (* 0.5 UnityEngine.Time/deltaTime)
        exant-cnt (dec (.Length -VS))
        vs (make-array Vector3 cnt)
        rs (make-array Quaternion cnt)]
    (aset vs 0 
      (v3+ (Hard.Helper/Aget -VS 0)
           (v3* (.forward (.transform o)) step)))
    (aset rs 0 (.rotation (.transform o)))
    (dotimes [i (dec cnt)]
      (aset vs (inc i) (Hard.Helper/Aget -VS (Mathf/Min i exant-cnt)))
      (aset rs (inc i) (Hard.Helper/Aget @DS (Mathf/Min i exant-cnt))))
    (reset! VS vs)
    (reset! DS rs)))


(defn place-segment [offset ^UnityEngine.Transform|[]| bones vs ds] ;^int offset  ^Vector3|[]| vs  ^Quaternion|[]| ds
  (let [offset-idx (- (* offset 6) offset)
        last-idx (dec (count vs))
        ^UnityEngine.AnimationCurve curve @curve]
    (dotimes [i 6]
      (let [snake-idx       (Mathf/Min (+ (* i 2) offset-idx) last-idx)
            ^Transform bone (aget bones i)
            ^Vector3    v   (Hard.Helper/Aget vs snake-idx)
            ^Quaternion d   (Hard.Helper/Aget ds snake-idx)
            radius (.Evaluate curve (float (- 1.0 (/ snake-idx (.Length vs)))))]
        (set! (.position bone) v)
        (set! (.rotation bone) d)
        (set! (.localScale bone) (v3 radius)) ))))


(m/defn ^System.Single curve-eval [^UnityEngine.AnimationCurve curve ^System.Double n]
  (.Evaluate curve n))

(m/defn ^System.Double bad-math [^System.Single a ^System.Int32 b]
  (- 1.0 (/ a b)))

(m/defn place-bone [
    ^UnityEngine.Transform|[]| bones 
    ^UnityEngine.Vector3|[]| vs 
    ^UnityEngine.Quaternion|[]| ds 
    ^int i ^int offset-idx ^int last-idx 
    ^UnityEngine.AnimationCurve curve]
  (let [^System.Single snake-idx       (Mathf/Min (+ (* i 2) offset-idx) last-idx)
        ^Transform bone (aget bones i)
        ^Vector3    v   (aget vs snake-idx)
        ^Quaternion d   (aget ds snake-idx)
        ^System.Single radius (.Evaluate curve (bad-math snake-idx (.-Length vs)))]
    (set! (.position bone) v)
    (set! (.rotation bone) d)
    (set! (.localScale bone) (v3 radius)) ))

(m/defn place-segment [
  ^int offset 
  ^UnityEngine.Transform|[]| bones 
  ^UnityEngine.Vector3|[]| vs 
  ^UnityEngine.Quaternion|[]| ds]
  (let [^int offset-idx (- (* offset 6) offset)
        ^int last-idx (dec (count vs))
        ^UnityEngine.AnimationCurve curve @curve
        f place-bone]
    (dotimes [i 6]
      (f bones vs ds i offset-idx last-idx curve))))




(defn tubesnek [] 
  (let [vs @VS
        ds @DS
        cnt (Mathf/Ceil (* (count vs) 0.1666666))]
    (dotimes [i cnt]
      (place-segment (* i 2) (aget @bones i) vs ds))
    true))

(defn place-snake [n]
  (let []
    (position! @head (aget @VS 0))
    (rotation! @head (aget @DS 0))
    (tubesnek)))

(defn apple! []
  (let [apple (clone! :snek/apple (v3 (?f -1 1)(?f -1 1)(?f -1 1)))
        rot (v3 (?f -0.1 0.1)(?f -0.1 0.1)(?f -0.1 0.1))]
    (hook+ apple :update (fn [o] (rotate! o rot)))))

(defn update-game [o]
  (let [ro (gobj (right))]
    (inc-snek)
    (place-snake nil)))

(defn grow-snek []
  (swap! snek-cnt inc))

(defn snek-hit [o c]
  (let [thing (.gameObject c)]
    (cond (= (.name thing) "apple")
          (do (grow-snek)
              (apple!)
              (destroy thing))
          (= (.name thing) "ball")
          (if true;(every? #(not= % thing) (take 4 @balls))
              (start nil)) )))

(defn start [o]
  (game.std/base-vr)
  (let [[vs ds] (prime-snek (right))]
    (reset! VS vs)
    (reset! DS ds))
  (clone! :sun)
  (clone! :grid)
  (reset! snek-cnt 20)
  (reset! head (clone! :snek/snek-head))
  (reset! segments (into-array UnityEngine.GameObject (for [_ (range 100)]  (clone! :snek/bonesnek))))
  (reset! bones 
    (into-array UnityEngine.Transform|[]| 
      (map #(into-array UnityEngine.Transform (first (.transform %))) @segments)))
  (reset! curve (.-curve (cmpt (clone! :snek/curve) Curve)))
  
  (timeline* 
    (wait 0.2)
    #(do (hook+ @head :on-trigger-enter #'snek-hit) nil))
  (dotimes [i 10] (apple!))
  (let [session? (clone! :empty)]
    (hook+ session? :update #'update-game))
  (on-hand (right) (clone! :snek/minisnake)))


(start nil)

'(clear-cloned!)




