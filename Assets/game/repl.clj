(ns game.repl
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    tween.core
    game.vr
    repl.parser)
  (:require
    arcadia.repl
    [clojure.string :as string])
  (:import [UnityEngine GameObject]))

(defonce repl (atom nil))
(def repl-env (atom (arcadia.repl/env-map)))
(def prompt  (atom 0))
(def histidx (atom 0))
(def ns-str  (atom "user=>"))
(def history (atom []))
(def shift (atom false))

(defn text [o] (.text (child-component o UnityEngine.TextMesh)))
(defn key-text! [o s] (set! (.text (child-component o UnityEngine.TextMesh)) s))

(deftween [:text :color] [this]
  {:base (.GetComponent this UnityEngine.TextMesh)
   :base-tag UnityEngine.TextMesh
   :get (.color this)
   :tag UnityEngine.Color})

(defn on-hand [hand o]
  (parent! o (gobj hand))
  (local-position! o (v3 0))
  (rotation! o (rotation (gobj hand))))

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

(defn eval-repl [o]
  (timeline*
    (wait 0)
    #(do 
      (let [form (state o :text)
            {:keys [result env]} (arcadia.repl/repl-eval-print @repl-env form)]
        (when-not (#{"" (last @history)} form)
          (swap! history conj form))
        (reset! histidx -1)
        (reset! repl-env env)
        (text! (state o :ns-obj) (str (ns-name (:*ns* @repl-env))))
        (text! (state o :publics-obj) 
          (apply str (interpose "\n" (keys (ns-publics (:*ns* @repl-env))))))
        (set-state! o :text (str form "\n" result))
        (reset! prompt (count (state o :text)))) nil)))

(defn init-repl [o]
  (set-state! o :cursor 0)
  (set-state! o :cursor-obj (child-named o "cursor"))
  (set-state! o :ns-obj (child-named o "ns"))
  (set-state! o :publics-obj (child-named o "publics"))
  (update-cursor o))

(defn toggle-shift [down]
  (log "shift")
  (reset! shift down)
  (if down
    (dorun (map #(key-text! % (str (state % :shift-key))) (every key)))
    (dorun (map #(key-text! % (str (state % :key))) (every key)))))


(defn mallet-enter [o c]
  (when (:key (state (.gameObject c)))
    (let [ko (.gameObject c)
          kt (gobj (.GetComponentInChildren ko UnityEngine.TextMesh))]
      (when-let [chr (state ko (if @shift :shift-key :key))]
        (if-let [f (state ko :f)]
          (case f
            :back (update-state! @repl :text #(subs % 0 (max 0 (dec (count %)))))
            :enter (update-state! @repl :text #(str % "\n"))
            :clear (set-state! @repl :text "")
            :eval (eval-repl @repl)
            :shift (toggle-shift true)
            nil)
          (update-state! @repl :text #(str % chr)))
        (repl-display @repl)
        (update-cursor @repl)
        (haptic (state o :index))
        (timeline*
          (tween {:local {:scale (v3 1.5)}
                  :text {:color (color 0 1 1)}} kt 0.1))))))

(defn mallet-exit [o c]
  (when (:key (state (.gameObject c)))
    (let [ko (.gameObject c)
          kt (gobj (.GetComponentInChildren ko UnityEngine.TextMesh))]
      (when-let [chr (state ko :key)]
        (if-let [f (state ko :f)]
          (case f
            :shift (toggle-shift false)
            nil))
        (timeline*
          (tween {:local {:scale (v3 1)}
                  :text {:color (color 1 1 1 0.25)}} kt 0.1))))))

(defn init-mallet [o hand]
  (hook-clear o :on-trigger-enter)
  (hook-clear o :on-trigger-exit)
  (set-state! o :index (.controllerIndex hand))
  (hook+ o :on-trigger-enter #'mallet-enter)
  (hook+ o :on-trigger-exit #'mallet-exit) o)

(defn mallets! []
  (on-hand (left) (init-mallet (clone! :mallet) (left)))
  (on-hand (right) (init-mallet (clone! :mallet) (right))))


(defn toggle-repl [s e]
  (if @repl 
    (do 
      (dorun (map destroy (every mallet)))
      (timeline* 
        (tween {:local {:scale (v3 0)}} @repl 0.6 :pow3)
        #(do 
          (reset! repl (destroy @repl)) nil)))
    (do (reset! repl (clone! :repl))
        (init-repl @repl)
        (eval-repl @repl)
        (set-state! @repl :text "")
        (mallets!)
        (let [rs (local-scale @repl)]
          (local-scale! @repl (v3 0))
          (timeline*
            (tween {:local {:scale rs}} @repl 0.6 :pow3))))))


; Editor setup

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