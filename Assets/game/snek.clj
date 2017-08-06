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

(def     snek-cnt (atom 7))
(defonce VS       (atom nil))
(defonce DS       (atom nil))
(defonce head     (atom nil))
(defonce curve    (atom nil))
(def     segments (atom []))
(def     bones    (atom nil))
(def xvs (UnityEngine.Vector3|[]|. 10000))
(def xvs-cnt (atom 0))
(def xqs (UnityEngine.Quaternion|[]|. 10000))
(def xqs-cnt (atom 0))

(m/defn inc-snek []
  (let [^UnityEngine.GameObject o (gobj (right))
        ^UnityEngine.Vector3|[]|    -VS @VS
        ^UnityEngine.Quaternion|[]| -DS @DS
        ^System.Int64 snek-cnt @snek-cnt
        cnt (* snek-cnt 10) ;70
        step (* 0.5 UnityEngine.Time/deltaTime)
        exant-cnt (dec (.-Length -VS))
        ^UnityEngine.Vector3|[]|    vs (UnityEngine.Vector3|[]|. cnt)
        ^UnityEngine.Quaternion|[]| rs (UnityEngine.Quaternion|[]|. cnt)]
    (aset vs 0 
      (v3+ (aget -VS 0)
           (v3* (.-forward (.-transform o)) step)))
    (aset rs 0 (.-rotation (.-transform o)))
    (dotimes [i (dec cnt)]
      (let [^System.Single idx (Mathf/Min i exant-cnt)
            ^System.Int64 ii (inc i)]
        (aset vs ii (aget -VS idx))
        (aset rs (inc i) (aget -DS idx))))
    (reset! VS vs)
    (reset! DS rs)))

(m/defn ^System.Double bad-math [^System.Single a ^System.Int32 b]
  (- 1.0 (/ a b)))

(m/defn place-segment [
  ^System.Int64 offset 
  ^UnityEngine.Transform|[]| bones 
  ^UnityEngine.Vector3|[]| vs 
  ^UnityEngine.Quaternion|[]| ds]
  (let [^int                        offset-idx (- (* offset 6) offset)
        ^int                        last-idx   (dec (count vs))
        ^UnityEngine.AnimationCurve curve      @curve]
    (dotimes [i 6]
      (let [^System.Single snake-idx (Mathf/Min (+ (* i 2) offset-idx) last-idx)
            ^Transform     bone      (aget bones i)
            ^Vector3       v         (aget vs snake-idx)
            ^Quaternion    d         (aget ds snake-idx)
            ^System.Single radius    (.Evaluate curve (bad-math snake-idx (.-Length vs)))
            ]
        (set! (.position bone) v)
        (set! (.rotation bone) d)
        (set! (.localScale bone) (v3 radius))))) )

(m/defn tubesnek [^UnityEngine.Vector3|[]| vs ^UnityEngine.Quaternion|[]| ds] 
  (let [^System.Single               cnt   (Mathf/Ceil (* (count vs) 0.1666666))
        ^UnityEngine.Transform|[][]| bones @bones]
    (dotimes [i cnt]
      (let [^System.Int64 offset (* i 2)
            ^UnityEngine.Transform|[]| bs (aget bones i)] 
        (place-segment offset bs vs ds)
        ))
    true))

(m/defn place-snake []
  (let [^UnityEngine.GameObject     head @head
        ^UnityEngine.Vector3|[]|    vs   @VS 
        ^UnityEngine.Quaternion|[]| ds   @DS]
    (set! (.-position (.-transform head)) (aget vs 0))
    (set! (.-rotation (.-transform head)) (aget ds 0))
    (tubesnek vs ds)))



(defn apple! []
  (let [apple (clone! :snek/apple (v3 (?f -1 1)(?f -1 1)(?f -1 1)))
        rot (v3 (?f -0.1 0.1)(?f -0.1 0.1)(?f -0.1 0.1))]
    (hook+ apple :update (fn [o] (rotate! o rot)))))

(defn update-game [o]
  (let [ro (gobj (right))]
    (inc-snek)
    (place-snake)))

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




