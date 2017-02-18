(ns game.vr
  (:use
    arcadia.core)
  (:require
    [clojure.string :as string])
  (:import 
    [UnityEngine]
    SteamVR_ControllerManager
    SteamVR_TrackedController
    ClickedEventHandler))

(defonce ^:private delegate-registry (atom {}))

(defonce ^:private hands (atom {
  :left nil
  :right nil
  :init {:left nil :right (fn [o] (log "init" o))}}))

(defn- get-hand [k]
  (let [h (k @hands)]
    (if (and h (not (null-obj? h))) h
      (let [on (str "Controller (" (name k) ")")
            ho (first (filter #(= (.name %) on) 
                  (children (gobj (object-typed SteamVR_ControllerManager)))))] 
        (when ho 
          (swap! hands assoc k (cmpt ho SteamVR_TrackedController))
          (if-let [init (-> @hands :init k)] (init ho)))
        (k @hands)))))

(defn un-camel [s]
  (-> s name
    (string/replace #"[-_]" " ")
    (string/replace #"([a-z])([A-Z])" "$1 $2")
    (string/replace #" [a-z]" string/upper-case)
    (string/replace #"^[a-z]" string/upper-case)
    (string/replace #" " "")))

(defn hand-delegate [f]
  (or (get @delegate-registry f)
    (let [rf (gen-delegate ClickedEventHandler [s e] (f s e))]
      (swap! delegate-registry assoc f rf) rf)))

(defmacro hand+ [c k f]
  (let [adder (symbol (str \. "add_" (un-camel (name k))))]
  `(~adder ~c (hand-delegate ~f))))

(defmacro hand- [c k f]
  (let [remover (symbol (str \. "remove_" (un-camel (name k))))]
  `(~remover ~c (hand-delegate ~f))))

(defn left  [] (get-hand :left))
(defn right [] (get-hand :right))

