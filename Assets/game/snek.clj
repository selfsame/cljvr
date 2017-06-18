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
  (:import [UnityEngine GameObject Quaternion Space Mathf Mesh Vector3]))

(def snek-cnt (atom 6))
(defonce snek (atom []))
(defonce head (atom nil))
(defonce tail (atom nil))
(def tube (atom nil))
(defonce nothing (atom nil))
(defonce bodies (atom []))
(defonce dirs (atom []))
(defonce tempo (atom 0))

(defn vert-ring [n r point dir]
  (let [step (/ (* Mathf/PI 2) n)]
    (map 
      (fn [i]
        (let [u (* i step)]
          (v3+ 
            point
            (q* dir 
                (v3 (* r (Mathf/Sin u))
                    (* r (Mathf/Cos u)) 0)))))
      (range n))))

; 3-4-5
; |\|\|
; 0-1-2

(defn ring-tris [n i vs]
  (mapcat
    (fn [idx]
      (let [base (* n i)]
        (if (= idx (dec n))
          [(+ base idx)(+ base)(+ base idx n)
           (+ base idx n)(+ base)(+ base n)]
          [(+ base idx)(+ base idx 1)(+ base idx n)
           (+ base idx n)(+ base idx 1)(+ base idx n 1)])))
    (range n)))

(defn tube! [r n points dirs]
  (let [s (count points)
        mesh (Mesh.)
        vs (mapcat #(vert-ring n r (get points % (v3)) (get dirs % (Quaternion.))) (range s))
        ts (mapcat #(ring-tris n % vs) (range (dec s)))]
      (set! (.vertices mesh) (into-array Vector3 vs))
      (set! (.triangles mesh) (into-array System.Int32 ts))
      (.RecalculateNormals mesh)
      mesh))

(defn mesh! [o m] (set! (.mesh (.GetComponent o UnityEngine.MeshFilter)) m))

'(mesh! (clone! :snek/tube) (tube! 0.1 8 12))






(defn snek-gizmos [o]
  (reduce 
    (fn [a b]
      (gizmo-color (color 1 0 1))
      (gizmo-point b 0.015)
      (gizmo-color (color 0 1 1))
      (gizmo-line a b) b)
    @snek) )

(defn tubesnek [o] 
  (let [vs (reverse @snek)
        ds (reverse @dirs)]
    (mesh! o (tube! 0.05 12 (vec (take @snek-cnt vs)) (vec (take @snek-cnt ds)))) 
    true))

(defn place-snake [n]
  (let [parts (vec (concat [@tail] @bodies [@head]))
        vs (vec (take-last (inc n) @snek))
        ds (vec (take-last (inc n) @dirs))]
    (tubesnek @tube)
    (dorun
      (for [i (reverse (range n))
            :let [o (get parts i)
                  o2 (get parts (dec i))
                  v (get vs i)
                  d (get ds i)
                  front (direct-child-named o "snek-armature/front")
                  rear (direct-child-named o "snek-armature/rear")
                  va (get vs (dec i) v)
                  vb (get vs (- i 2) va)
                  vc (get vs (- i 3) vb)
                  d2 (get ds (dec i) d)
                  d3 (get ds (- i 2) d)
                  r (- 1 (* @tempo 3))]]
        (let [pos (spline r v va vb vc)
              pos2 (spline (min (+ r 1) 1.0) v va vb vc)
              q (Quaternion/Lerp d d2 r)
              q2 (Quaternion/Lerp d2 d3 r)]
          (set! (.position (.transform front)) pos)
          (set! (.rotation (.transform front)) q)
          (.Rotate (.transform front) (v3 90 0 0))
          (if o2 
            (do
              (set! (.position (.transform rear)) 
                    (.position (.transform (direct-child-named o2 "snek-armature/front"))))
              (set! (.rotation (.transform rear))
                    (.rotation (.transform (direct-child-named o2 "snek-armature/front")))))

            (do  
              (set! (.position (.transform rear)) pos2)
              (set! (.rotation (.transform rear)) q2)
              (.Rotate (.transform rear) (v3 90 0 0)))) )))))

(defn apple! []
  (let [apple (clone! :snek/apple (v3 (?f -1 1)(?f -1 1)(?f -1 1)))
        rot (v3 (?f -0.1 0.1)(?f -0.1 0.1)(?f -0.1 0.1))]
    (hook+ apple :update (fn [o] (rotate! o rot)))))

(defn update-game [o]
  (let [ro (gobj (right))]
    (swap! snek update (dec (count @snek)) 
      #(do %
        (v3+ (get @snek (- (count @snek) 2))
          (v3* (.forward (.transform ro)) 0.1))))
    (swap! dirs update (dec (count @dirs)) 
      #(do % (.rotation (.transform ro))))

    (swap! tempo #(+ % UnityEngine.Time/deltaTime))
    (if (>= (count @snek) @snek-cnt)
        (place-snake @snek-cnt))))

(defn inc-snek []
  (let [o (gobj (right))]
    (swap! snek conj 
      (v3+ (last @snek)
        (v3* (.forward (.transform o)) 0.1)))
    (swap! dirs conj (.rotation (.transform o)))
  (reset! tempo 0)) nil)

(defn grow-snek []
  (swap! snek-cnt inc)
  (swap! bodies conj (clone! :snek/snek)))

(defn snek-hit [o c]
  (let [thing (.gameObject c)]
    (cond (= (.name thing) "apple")
          (do (grow-snek)
              (grow-snek)
              (apple!)
              (destroy thing)))))

(defn start [o]
  (game.std/base-vr)
  (reset! snek [(v3 0 0 -1)])
  (reset! head (clone! :snek/snek-head))
  (reset! tail (clone! :snek/snek-tail))
  (reset! tube (clone! :snek/tube))
  (reset! bodies (vec (for [i (range (- @snek-cnt 2))] (clone! :snek/snek))))
  (reset! dirs [(Quaternion.)])
  (clone! :sun)
  (clone! :grid)
  (hook+ @head :on-trigger-enter #'snek-hit)
  (dotimes [i 10] (apple!))
  (let [session? (clone! :empty)]
    (hook+ session? :update #'update-game)
    (hook+ session? :on-draw-gizmos #'snek-gizmos)
    (timeline* :loop
      (wait 0.3)
      #(if (null-obj? session?) (abort!))
      #(inc-snek)))
(on-hand (right) (clone! :snek/minisnake))
  )

(start nil)








