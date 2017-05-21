(ns game.std
  (:use
    arcadia.core
    arcadia.linear
    hard.core
    game.vr)
  (:require
    game.repl)
  (:import [UnityEngine GameObject]))

(defn on-hand [hand o]
  (parent! o (gobj hand))
  (local-position! o (v3 0))
  (rotation! o (rotation (gobj hand))))

(defn base-vr []
  (reset! game.repl/repl nil)
  (destroy-immediate (the camera-rig))
  (clear-cloned!)
  (clone! :camera-rig)
  (dorun (map 
    #(do 
      (hand+ % :menu-button-clicked #'game.repl/toggle-repl)
      (let [toucher (clone! :toucher)]
        (hook+ toucher :on-trigger-enter (fn [o c] (log ["trigger" o c])))
        (on-hand % toucher)))
    [(left)(right)])))