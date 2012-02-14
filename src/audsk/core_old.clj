(ns audsk.core
  (:use [rosado.processing]
	[rosado.processing.applet]
	;; [socks]
	)
  (:require [tuio :as tuio]
	    [overtone.core :as tone]))

;;; GLOBALS
;;
;; applet  
(def ^:dynamic *width*                   (ref 800))
(def ^:dynamic *height*                  (ref 800))

;; fonts  
(def ^:dynamic *font*                   (atom nil))


;; mouse-primitives
(def ^:dynamic *mouse-dn?*              (atom nil))
(def ^:dynamic *mouse-pos*              (atom nil))
(def ^:dynamic *draw-bg?*              (atom true))


;; game loop
(def ^:dynamic *time*                   (atom   0))
(def ^:dynamic *running?*               (atom nil))

;; game world
(def ^:dynamic *entities*               (ref   {})) ; entity-id -> obj
(def ^:dynamic *quad-tree*              (ref   {})) ; 

;; TUIO
(def ^:dynamic *tuio-port* 3333)
(def ^:dynamic *tuio* (tuio/client *tuio-port*))

(defn init-tuio [port])

(defn kill-tuio []
   (tuio/disconnect! *tuio*)
   (println "tuio client disconnected"))


;; Game loop
(defn update []
  (when-not @*paused?*
    (swap! *time* inc)
    (doall (map :update @*entities*))))


(defn draw-proxy []
  )

(defn draw []
  (try (map :draw @*entities*)
       (catch Exception e
	 (.printStackTrace e))))



(defn setup)
