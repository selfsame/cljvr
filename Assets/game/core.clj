(ns game.core
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    repl.parser)
  (:require
    [clojure.string :as string]))

(defonce repl (atom nil))

(defn text [o] (.text (child-component o UnityEngine.TextMesh)))
(defn text! [o s] (set! (.text (child-component o UnityEngine.TextMesh)) s))
(defn material-color! [o c] (set! (.color (.material (.GetComponent o UnityEngine.Renderer))) c))

(deftween [:text :color] [this]
  {:base (.GetComponent this UnityEngine.TextMesh)
   :base-tag UnityEngine.TextMesh
   :get (.color this)
   :tag UnityEngine.Color})

(defn on-hand [hand o]
  (parent! o (gobj hand))
  (local-position! o (v3 0))
  (rotation! o (rotation (gobj hand))))

(defn add-ball [s e]
  (let [b (clone! :ball (>v3 (gobj s)))]
    (material-color! b (color 1 0 0))
    (destroy b 10.0)))

(defn repl-display [o]
  (text! (the repl) (parse (str (state o :text)))))

(defn topogolize-cursor [n s]
  (let [split-lines (string/split s #"\n")]
    (loop [n n y 0 lines split-lines]
      (if (or (empty? lines) 
              (>= (count (first lines)) n))
        [[n y] (count split-lines)]
        (recur (- n (inc (count (first lines)))) (inc y) (rest lines))))))

(defn update-cursor [o]
  (let [s (state o :text)
        n (count s)
        [[x y] lc] (topogolize-cursor n s)]
  (set-state! o :cursor n)
  (local-position! (state o :cursor-obj) 
    (v3* (v3 x (+ (- lc y) 0.1) 0)
         0.098))))



(defn init-repl [o]
  (set-state! o :cursor 0)
  (set-state! o :cursor-obj (child-named o "cursor"))
  (update-cursor o))

(defn mallet-enter [o c]
  (let [ko (.gameObject c)
        kt (gobj (.GetComponentInChildren ko UnityEngine.TextMesh))]
    (when-let [chr (state ko :key)]
      (if-let [f (state ko :f)]
        (case f
          :back (update-state! @repl :text #(subs % 0 (max 0 (dec (count %)))))
          :enter (update-state! @repl :text #(str % "\n"))
          nil)
        (update-state! @repl :text #(str % chr)))
      (repl-display @repl)
      (update-cursor @repl)
      (haptic (state o :index))
      (timeline*
        (tween {:local {:scale (v3 1.5)}
                :text {:color (color 0 1 1)}} kt 0.1)))))

(defn mallet-exit [o c]
  (let [ko (.gameObject c)
        kt (gobj (.GetComponentInChildren ko UnityEngine.TextMesh))]
    (when-let [chr (state ko :key)]
      (timeline*
        (tween {:local {:scale (v3 1)}
                :text {:color (color 1 1 1 0.25)}} kt 0.1)))))

(defn init-mallet [o hand]
  (hook-clear o :on-trigger-enter)
  (hook-clear o :on-trigger-exit)
  (set-state! o :index (.controllerIndex hand))
  (hook+ o :on-trigger-enter #'mallet-enter)
  (hook+ o :on-trigger-exit #'mallet-exit) o)

(defn mallets! []
  (on-hand (left) (init-mallet (clone! :mallet) (left)))
  (on-hand (right) (init-mallet (clone! :mallet) (right))))


(defn start [o]
  (destroy-immediate (the camera-rig))
  (clear-cloned!)
  (clone! :sun)
  (clone! :grid)
  (clone! :camera-rig)
  (reset! repl (clone! :repl))
  (init-repl @repl)
  (mallets!)
  (hand+ (right) :trigger-clicked #'add-ball))






(def keychars [
  "`1234567890-="
  "qwertyuiop[]\\"
  "asdfghjkl;'"
  "zxcvbnm,./"])

(def shift-keychars [
  "~!@#$%^&*()_+"
  "QWERTYUIOP{}|"
  "ASDFGHJKL:\""
  "ZXCVBNM<>?"])

(defn make-keyboard []
  (let [board (clone! :keyboard)]
  (dorun 
    (for [z (range (count keychars))
          :let [row (vec (nth keychars z))
                shift-row (vec (nth shift-keychars z))]]
      (dorun (for [x (range (count row))
            :let [chr (nth row x)
                  shift-chr (nth shift-row x)]]
        (let [k (clone! :key (v3* (v3 (+ x (* z 0.4)) 0 (- z)) 0.11))]
          (set-state! k :key chr)
          (set-state! k :shift-key shift-chr)
          (text! k (str chr))
          (parent! k board))))))))

'(make-keyboard)
'(hook+ (the hook) :start #'start)
'(start nil)
'(set-state! (the left-paren) :shift-key "(")
