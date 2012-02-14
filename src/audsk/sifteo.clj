(ns audsk.sifteo
  (:use [overtone.osc]
	[audsk.core]))


(def srv (atom nil))
  
(defn on-init
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - []"
  [msg]
  (println [:INIT (:args msg)]))  

(defn on-tick
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - []"
  [msg]
  (println [:TICK (:args msg)]))  
 
(defn on-pause
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - []"
  [msg]
  (println [:PAUSE (:args msg)]))

(defn on-unpause
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - []"
  [msg]
  (println [:UNPAUSE (:args msg)]))

(defn on-new-cube
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - [id]"
  [msg]
  (let [[id] (:args msg)]
    (println [:NEW-CUBE id])))
  

(defn on-button-press
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - [id state]

     id is the cube.UniqueID
     state is integer either 0 or 1"
  [msg]
  (let [[id state] (:args msg)
	state (if (pos? state) true false)]
    (println [:BUTTON id state])))  

(defn on-shake-start
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - [id]"
  [msg]
  (let [[id] (:args msg)]
    (println [:SHAKE-START id])))  

(defn on-shake-stop
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - [id duration]"
  [msg]
  (let [[id duration] (:args msg)]
    (println [:SHAKE-STOP id duration])))  

(let [lookup {:x {0 :left
		  1 :center
		  2 :right}
      
	      :y {0 :down
		  1 :center
		  2 :up}

	      :z {0 :face-down
		  1 :on-edge
		  2 :face-up}}]
  (defn on-tilt
    "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - [id x y z]
     x {0 :left
        1 :center
        2 :right}
      
     y {0 :down
        1 :center
        2 :up}

     z {0 :face-down
        1 :on-edge
        2 :face-up}"
    [msg]
    (let [[id x y z] (:args msg)
	  x (get-in lookup [:x x])
	  y (get-in lookup [:y y])
	  z (get-in lookup [:z z])
	  tuio-id (->tuio-id id)]
      (swap! *cubes* (partial merge-with into)
	     {tuio-id {:orientation {:x x :y y :z z}
		       :time-last (time-now)}})
      (println [:TILT id [x y z]]))))
  
(defn on-flip
  "msg  - {:src-host, :src-port, :path, :type-tag, :args}
     args - [id orientation]
     orientation is integer either 0 (facing-down) or 1 (face-up)"
  [msg]
  (let [[id orientation] (:args msg)
	dir (if (pos? orientation) :face-up :face-down)]
    (println [:FILP id dir])))  
  
(defn register-sifteo-handlers [server]
  (osc-handle server "/sifteo/init" #'on-init)
  (osc-handle server "/sifteo/tick" #'on-tick)
  (osc-handle server "/sifteo/pause" #'on-pause)
  (osc-handle server "/sifteo/unpause" #'on-unpause)
  (osc-handle server "/cube/button-press" #'on-button-press)
  (osc-handle server "/cube/shake-start" #'on-shake-start)
  (osc-handle server "/cube/shake-stop" #'on-shake-stop)
  (osc-handle server "/cube/tilt" #'on-tilt)
  (osc-handle server "/cube/flip" #'on-flip))


(defn init-sifteo-server []
  (reset! srv (osc-server 6655))
  (register-sifteo-handlers @srv))


(comment (init-sifteo-server))


;; (defprotocol SifteoCube
;;   "Objects extending the SifteoCube protocol"

;;   ;; (add-neighbor   [this this-side neigh neigh-side])
;;   ;; (rem-neighbor  [this this-side neigh neigh-side])
;;   ;; (button-press [this])
;;   ;; (shake-start [this])
;;   ;; (shake-stop [this])
;;   ;; (flip      [this])
;;   ;; (tilt     [this])
;;   ;; (id      [this] "returns ID of associated sifteo") 
;;   )



;; (osc-handle @srv "/cube/button-press" 
;; 	    (fn [msg]
;; 	      (println [msg (:args msg)])))

;;  OSC-PATH            ARGS
;; "/cube/button-press" [:button-state]
;; "/cube/shake-start"  []
;; "/cube/shake-stop"   [:duration]
;; "/cube/tilt"         [:x :y :z]
;; "/cube/flip"         [:orientation ]
