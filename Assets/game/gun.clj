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
(def shapes [:gun/triangle :gun/star :gun/pentagon])
(def colors [(color 1.0 0.0 0.0) (color 1.0 0.5586207 0.0) (color 0.0 0.6323529 0.03052744) (color 0.0 0.3793104 1.0) (color 0.6137934 0.0 1.0)])
(def numbers (vec (range 1 10)))

(defn ^GameObject suite! []
  (let [shape (rand-nth shapes)
        c (rand-nth colors)
        n (rand-nth numbers)
        o (clone! :gun/sphere)
        mount (first (children o))]
    (data! o {
      :shape shape
      :color c
      :number n})
    (set! (.name (parent! (clone! shape) mount)) "shape")
    (material-color! o c)
    (text! (parent! (clone! :gun/number) mount) (.ToString n))
    o))

(defn destroy-ball [^GameObject o ^GameObject b]
  (let [os (data o)
        bs (data b)]
    (when (= (:shape os) (:shape bs))
      (data! o :shape nil)
      (destroy (child-named o "shape")))
    (when (= (:number os) (:number bs))
      (data! o :number nil)
      (destroy (child-named o "number")))
    (when (every? nil? (map (data o) [:shape :number]))
      (destroy (clone! :gun/splode (>v3 o)) 3)
      (destroy o))))

(defn move-forward [^GameObject o]
  (let [v (.position (.transform o))
        up (.up (.transform o))]
    (set! (.position (.transform o)) (v3+ v (v3* up (∆ bullet-speed))))
    (when-let [h (hit v up (∆ bullet-speed))] 
      (destroy-ball (.gameObject (.collider h)) o)
      (destroy o))))

(defn reload-gun! [^GameObject o]
  (let [^GameObject s (suite!)
        ^UnityEngine.SphereCollider sc (.GetComponent s UnityEngine.SphereCollider)]
    (set! (.enabled sc) false)
    (parent! s o)
    (set! (.localRotation (.transform s)) 
      (UnityEngine.Quaternion. -0.1979277 -0.0001618862 -0.0002829984 0.9802166))
    (set! (.localPosition (.transform s)) (v3 0.0 0.082 -0.031))
    (set! (.localScale (.transform s)) (v3 0.042 0.042 0.042))))

(defn fire-gun! [c e]
  (let [o (gobj c)
        muzzle (direct-child-named o "raygun/muzzle")
        bullet (clone! :gun/bullet (>v3 muzzle))
        s (or (direct-child-named o "raygun/sphere")
              (direct-child-named o "sphere"))]
    (set! (.rotation (.transform bullet))
          (.rotation (.transform muzzle)))
    (data! bullet (data s))
    (destroy s)
    (hook+ bullet :update #'move-forward)
    (timeline* #(haptic! c)#(haptic! c)#(haptic! c)#(haptic! c))
    (destroy bullet 4.0)
    (reload-gun! o)))

(defn raygun! [hand] 
  (if-let [model (child-named (gobj hand) "Model")]
    (.SetActive model false))
  (let [gun (clone! :gun/raygun)
        trigger (child-named gun "trigger")]
    (on-hand hand gun)
    (reload-gun! gun)
    (hook+ trigger :update
      (fn [^GameObject o]
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
        o (suite!)]
    (position! o pos)
    (look-at! o (v3 0 -1 -2))
    (.Rotate (.transform o) (v3 1 0 0) 90)
    (.Rotate (.transform o) (v3 0 1 0) 180)
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


'(mapv #(let [c (state (the colors) %)] (list 'color (.r c) (.g c) (.b c))) [:a :b :c :d :e])

;174
'(import [GC])
'(hand+ (right) :trigger-clicked 
  (fn [c e] 
    (set-state! (create-primitive :cube) :foo 5)))

'(set-state! (the joe) :a 1)