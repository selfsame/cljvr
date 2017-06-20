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
  (:import [UnityEngine GameObject Quaternion Space Mathf Mesh Vector3]
    Curve))

(defn mesh! [o m] (set! (.mesh (.GetComponent o UnityEngine.MeshFilter)) m))

(def snek-cnt (atom 7))
(defonce VS (atom nil))
(defonce DS (atom nil))
(defonce head (atom nil))
(defonce tail (atom nil))
(defonce curve (atom nil))
(def tube (atom nil))

(defonce tempo (atom 0))

(def twopi (* Mathf/PI 2))

(defn vert-ring [n r point dir]
  (let [step (/ twopi n)]
    (map 
      (fn [i]
        (let [u (* i step)]
          (v3+ 
            point
            (q* dir 
                (v3 (* r (Mathf/Sin u))
                    (* r (Mathf/Cos u)) 0.0)))))
      (range n))))

(defn ring-tris [n i vs]
  (mapcat
    (fn [idx]
      (let [base (* n i)]
        (if (= idx (dec n))
          [(+ base idx)(+ base idx n)(+ base)
           (+ base idx n)(+ base n)(+ base)]
          [(+ base idx)(+ base idx n)(+ base idx 1)
           (+ base idx n)(+ base idx n 1)(+ base idx 1)])))
    (range n)))

(defn tube! [radius n points dirs]
  (let [s (count points)
        mesh (Mesh.)
        r (- 1 (* @tempo 3))

        vs (mapcat 
            #(let [;p1 (get points % (v3))
                   ;p2 (get points (+ % 1) p1)
                   ;p3 (get points (+ % 2) p2)
                   ;p4 (get points (+ % 3) p3)
                   d (get dirs (- s % 1) (Quaternion.))
                   ;d2 (get dirs (- % 1) d)
                   ;q (Quaternion/Lerp d d2 r)
                   radius2 (* radius (.Evaluate (.curve (cmpt @curve Curve)) (float (/ % s))))
                   ] 
              (vert-ring n radius2 
                (get points (- s % 1) (v3)) 
                d )) 
             (range s))
        ts (mapcat #(ring-tris n % vs) (range (dec s)))]
      (set! (.vertices mesh) (into-array Vector3 vs ))
      (set! (.triangles mesh) (into-array System.Int32 ts ))
      (.RecalculateNormals mesh)
      mesh))

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
        cnt (* @snek-cnt 10)
        step (* 0.5 UnityEngine.Time/deltaTime)
        vs (make-array Vector3 cnt)
        rs (make-array Quaternion cnt)]
    (aset vs 0 
      (v3+ (first @VS)
           (v3* (.forward (.transform o)) step)))
    (aset rs 0 (.rotation (.transform o)))
    (dotimes [i (dec cnt)]
      (aset vs (inc i) (get @VS i (last @VS)))
      (aset rs (inc i) (get @DS i (last @DS))))
    (reset! VS vs)
    (reset! DS rs)))


(defn snek-gizmos [o]
  (gizmo-color (color 0 1 1))
      (gizmo-point (first @VS) 0.05)
  (reduce 
    (fn [a b]
      (gizmo-color (color 1 0 1))
      (gizmo-point b 0.015)
      (gizmo-color (color 0 1 1))
      (gizmo-line a b) b)
    @VS) )

(defn every-nth [col n]
  (vec (for [i (range (count col))
        :when (= 0 (mod i n))
        :let [v (get col i)]] v)))

(defn tubesnek [o] 
  (let [vs (every-nth @VS 4)
        ds (every-nth @DS 4)]
    (mesh! o (tube! 0.05 6 vs ds)) 
    true))

(defn place-snake [n]
  (let []
    (position! @head (first @VS))
    (rotation! @head (first @DS))
    (tubesnek @tube)
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
              (destroy thing)))))

(defn start [o]
  (game.std/base-vr)
  (let [[vs ds] (prime-snek (right))]
    (reset! VS vs)
    (reset! DS ds))
  (clone! :sun)
  (clone! :grid)
  (clone! :skyball)
  (reset! head (clone! :snek/snek-head))
  (reset! tube (clone! :snek/tube))
  (reset! curve (clone! :snek/curve))
  (material-color! @tube (color 0.2745 0.6901 0.2196))
  (hook+ @head :on-trigger-enter #'snek-hit)
  (dotimes [i 10] (apple!))
  (let [session? (clone! :empty)]
    (hook+ session? :update #'update-game)
    (hook+ session? :on-draw-gizmos #'snek-gizmos))
  (on-hand (right) (clone! :snek/minisnake)))

(start nil)








