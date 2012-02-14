(ns audsk.models
  ;; (:use [audsk.protocols])
  )

(defrecord Point [pos etc])

(defrecord Path  [corner points])


;; (defrecord AudioPoint   [pos           ;; [x y]
;; 			 time          ;; long
;; 			 etc           ;; {:velocity :stroke-width :pen-id}
;; 			 ])



;; (defrecord AudioCurve   [path          ;; AudioPoint
;; 			 time-start    ;;
;; 			 time-end      ;;
;; 			 bounds        ;; {:min-x :min-y :max-x :max-y}
;; 			 width         ;; computed from bounds
;; 			 height        ;; computed from bounds
;; 			 player        ;; overtone playback fn
;; 			 ])

;; (defrecord PlaybackHead [curve         ;; AudioCurve
;; 			 tempo         ;; 
;; 			 key           ;; 
;; 			 voice         ;; 
;; 			 curr-point 
;; 			 frequency-bins])









