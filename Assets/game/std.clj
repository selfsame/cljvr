(ns game.std
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    hard.seed
    game.vr)
  (:require
    game.repl
    hard.mesh)
  (:import 
    [UnityEngine GameObject Color]
    [Mathf]))

(defn material-color! [o c] (set! (.color (.material (.GetComponent o UnityEngine.Renderer))) c))

(defn on-hand [hand o]
  (parent! o (gobj hand))
  (local-position! o (v3 0))
  (rotation! o (rotation (gobj hand))))

(defn- trigger-clicked [s e] 
  (let [hand (gobj s)]
    (set-state! hand :trigger-clicked? true)
    (dorun
      (map 
        #(if-let [f (state % :trigger-clicked)] ((eval f) % hand))
        (state hand :touching)))))

(defn- base-hand [hand-controller]    
  (let [hand (gobj hand-controller)
        toucher (clone! :toucher)]
    (set-state! hand :touching #{})
    (hook+ toucher :on-trigger-enter 
      (fn [t c] 
        (let [o (.gameObject c)]
          (update-state! hand :touching #(conj % o))
          (if-let [f (state o :on-trigger-enter)] ((eval f) o c)))))
    (hook+ toucher :on-trigger-exit 
      (fn [t c] 
        (let [o (.gameObject c)]
          (update-state! hand :touching #(disj % o))
          (if-let [f (state o :on-trigger-exit)] ((eval f) o c)))))
    (on-hand hand toucher)
    (hand+ hand-controller :menu-button-clicked #'game.repl/toggle-repl)
    (hand+ hand-controller :trigger-clicked #'trigger-clicked)))

(defn base-vr []
  (reset! game.repl/repl nil)
  (destroy-immediate (the camera-rig))
  (clear-cloned!)
  (clone! :camera-rig)
  (base-hand (left))
  (base-hand (right)))


(defn gradiate [o] 
  (let [skydome o
          [c1 c2 c3 c4 c5 c6] (mapv color (sort #(< (first %1) (first %2)) 
            (repeatedly 6 #(vector (?f 0.1 1)(?f 0.1 1)(?f 0.1 1)))))
          [m1 m2 m3] (vec (shuffle [(srand 10)(srand 24)(srand 13)]))]
      (hard.mesh/vertex-colors! skydome 
        (fn [x y z i] 
          (Color/Lerp c5
            (Color/Lerp c1 
              (Color/Lerp c6 c3 (sin (* y m3)))
              (cos (* x m2)))
            (cos (* z m1)))))
      o))