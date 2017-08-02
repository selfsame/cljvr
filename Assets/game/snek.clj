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
  (:require [magic.api :as m])
  (:import [UnityEngine GameObject Quaternion Space Mathf Mesh Vector3]
    Curve [Hard Helper]
    Snek))

(m/bind-basic-spells!)

(declare start)

(defn mesh! [o m] (set! (.mesh (.GetComponent o UnityEngine.MeshFilter)) m))
(defn shared-mesh! [o m] (set! (.sharedMesh (.GetComponent o UnityEngine.MeshFilter)) m))

(def snek-cnt (atom 7))
(defonce VS (atom nil))
(defonce DS (atom nil))
(defonce head (atom nil))
(defonce tail (atom nil))
(defonce curve (atom nil))
(def tubes (atom []))
(def balls (atom []))
(def raw-tube (atom nil))
(defonce tempo (atom 0))

'(* radius (.Evaluate (.curve (cmpt @curve Curve)) (float (/ % s))))

(defn trip []
  (let [background (clone! :empty)]
    (dorun 
      (for [i (range 1 #_(+ 2 (rand-int 3)) )]
        (let [o (clone! :icosphere)
              r (v3 (?f -0.06 0.06)(?f -0.06 0.06)(?f -0.06 0.06))] 
          (parent! o background)
          (gradiate o)
          (timeline* #(rotate! o r)))))
    (local-scale! background (v3 20))))

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
        -DS @DS
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
      (m/faster (aset vs (inc i) (Hard.Helper/Aget -VS (Mathf/Min i exant-cnt))))
       (aset rs (inc i) (Hard.Helper/Aget -DS (Mathf/Min i exant-cnt))))
    (reset! VS vs)
    (reset! DS rs)))


(defn snek-gizmos [o]
  #_(gizmo-color (color 0 1 1))
      (gizmo-point (first @VS) 0.05)
  #_(reduce 
    (fn [a b]
      (gizmo-color (color 1 0 1))
      (gizmo-point b 0.015)
      (gizmo-color (color 0 1 1))
      (gizmo-line a b) b)
    @VS) )


(def ^System.Int64 tube-segments 10)
(def ^System.Int64 segment-verts 12)

(defn place-tube [offset tube vs ds]
  (let [verts (vertices @raw-tube)
        offset-idx (- (* offset tube-segments) offset)
        last-idx (- (.-Length vs) 1)
        ^UnityEngine.AnimationCurve curve (.-curve (cmpt @curve Curve))
        ^UnityEngine.Mesh mesh (.-mesh (cmpt tube UnityEngine.MeshFilter))]
    (dotimes [i tube-segments]
      (let [snake-idx (Mathf/Min (+ (* i 2) offset-idx) last-idx)
            ^Vector3 v (Hard.Helper/Aget vs snake-idx)
            ^Quaternion d (Hard.Helper/Aget ds snake-idx)
            radius (* 0.05 (.Evaluate curve (float (- 1.0 (/ snake-idx (.-Length vs))))))]
      (dotimes [j segment-verts]
        (m/faster (let [^System.Int64 idx (+ (* (- 9 i) 12) j)]
          (aset verts idx 
            (v3+ v (v3* (q* d (Hard.Helper/Aget verts idx)) radius)) ))))))
    ;(vertices! tube verts)
    (set! (.-vertices mesh) verts)
    (.RecalculateNormals mesh)
    (.RecalculateBounds mesh)))

(m/defn tubecnt [vs]
  (int (Mathf/Ceil (/ (float (count vs)) (float 18.0)))))

(defn tubesnek [] 
  (let [vs @VS
        ds @DS
        spacer 10
        cnt (tubecnt vs)]
    (dotimes [i (.-Length vs)]
      (if (= (Hard.Helper/Mod i spacer) 0)
        (if-let [ball (get @balls (int (/ i spacer)))]
          (position! ball (get vs i)))))
    (dotimes [i cnt]
      (place-tube (* i 2) (get @tubes i) vs ds))
    true))

(defn place-snake [n]
  (let []
    (position! @head (first @VS))
    (rotation! @head (first @DS))
    (tubesnek)
    ))

(defn apple! []
  (let [apple (clone! :snek/apple (v3 (?f -1 1)(?f -1 1)(?f -1 1)))
        rot (v3 (?f -0.1 0.1)(?f -0.1 0.1)(?f -0.1 0.1))]
    (hook+ apple :update (fn [o] (rotate! o rot)))))

(defn update-game [o]
  (let [ro (gobj (right))]
    (inc-snek)
    (place-snake nil)
    ))



(defn grow-snek []
  (swap! snek-cnt inc))

(defn snek-hit [o c]
  (let [thing (.gameObject c)]
    (cond (= (.name thing) "apple")
          (do (grow-snek)
              (apple!)
              (destroy thing))
          (= (.name thing) "ball")
          (if (every? #(not= % thing) (take 4 @balls))
            (timeline* 
              (wait 0)
              #(do (start nil) nil))) )))

(defn tube! []
  (let [tube (clone! :snek/tube)]
    (material-color! tube (color 0.2745 0.6901 0.2196))
    tube))

(defn start [o]
  (game.std/base-vr)
  (let [[vs ds] (prime-snek (right))]
    (reset! VS vs)
    (reset! DS ds))
  (clone! :sun)
  (clone! :grid)
  (trip)
  (reset! snek-cnt 20)
  (reset! head (clone! :snek/snek-head))
  (reset! tubes (vec (for [_ (range 100)]  (tube!))))
  (reset! balls (vec (for [_ (range 100)] (clone! :snek/ball))))
  (reset! raw-tube (clone! :snek/tube))
  (reset! curve (clone! :snek/curve))
  
  (timeline* 
    (wait 0.2)
    #(do (hook+ @head :on-trigger-enter #'snek-hit) nil))
  (dotimes [i 10] (apple!))
  (let [session? (clone! :empty)]
    (hook+ session? :update #'update-game)
    (hook+ session? :on-draw-gizmos #'snek-gizmos))
  (on-hand (right) (clone! :snek/minisnake)))

'(start nil)

'(clear-cloned!)