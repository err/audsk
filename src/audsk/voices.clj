(ns audsk.voices
  (:use [overtone.live]))

;; (definst boop [freq  440 phase 0.0] 
;;   (sin-osc freq phase))


(definst beep [freq 440]
  (sin-osc freq 0.0))

(definst bop [freq 440]
  (saw freq))

(definst meep [freq 440]
  (let [fs (map (partial * freq) [0.8 1.0 1.2])]
    (reduce + [(sin-osc (nth fs 0) 0.0)
	       (sin-osc (nth fs 1) 0.0)
	       (sin-osc (nth fs 2) 0.0)])))



