(ns audsk.core
  (:use [clojure.set :only [map-invert]]
	[rosado.processing.applet]
	[rosado.processing :exclude [cursor]]
	[audsk.voices]
	[audsk.utils misc tuio-helpers])
  (:require [audsk.tuio    :as tuio]
	    [audsk.comm    :as comm]
	    [overtone.live :as tone]
	    ;; [overtone.inst.piano :as otp]
	    )
  (:import [TUIO TuioCursor TuioPoint TuioListener]
	   [codeanticode.glgraphics GLTexture GLConstants]))


(declare auto-tuned? *cubes* img *font*	->screen sketch)


(def ^:dynamic *offsb* (atom nil))

(defmacro with-graphics [[gobj] & body]
  `(binding [*applet* ~gobj]
     (.beginDraw *applet*)
     ~@body
     (.endDraw *applet*)))

(defrecord TextObj [txt pos rot img])

(defmacro with-buffer [[buf] & body]
  )


(defn make-text-image [txt buf opts]
  (let [{:keys [font size]} opts
	buf  @buf
	font @font]
    (doto buf
      (.beginDraw)
      (.background 100)
      (.textFont font) ;; ; ; ; WHOA-NOW!
      (.textSize size)
      (.fill  0)
      (.noStroke)

      (.fill  255)
      (.text  txt (float 4) (float size))
      (.endDraw))
    
    (let [gltx (GLTexture. *applet*)
	  pimg (.get buf 0 0 (+ 8 (round (.textWidth buf txt))) 18)] 
      (.putImage gltx pimg)
      gltx)))

(defn make-text-obj [& args]
  (let [m (map->TextObj (apply hash-map args))]
    (if (nil? (:img args))
      (assoc m :img (make-text-image (:txt m) *offsb* {:font *font* :size 14})); '' 'whoa-now' '' 
      m)))

(defn render [txt]
  (with-translation (->screen (:pos txt))
    (with-rotation [0]
      (when-let [img (:img txt)]
	(image-mode CENTER)
	(image img 0 0)))))

(set! *warn-on-reflection* true)

;; # UTILS
(defn half     [x] (/ x 2))
(defn time-now [ ] (System/currentTimeMillis))

;; ## Drawing macro
(defmacro shape [type & body]
  `(do
     (begin-shape ~type)
     ~@(map (fn [[x# y#]] `(vertex ~x# ~y#)) body)
     (end-shape CLOSE)))

(defmacro with-scale [[s] & body]
  `(let [s# ~s]
     (push-matrix)
     (scale s#)
     ~@body
     (pop-matrix)))

;; color-map
;; {:keys [r g b a]
;;  :or   {r 190 g 110 b 190 a 180}
;;  :as   color}
(defn draw-curve
  [pts color]
  (let [{:keys [r g b a]} color]
    (no-fill)
    (stroke-weight 1)
    (stroke;; -float
     r g b a)
    ;; (begin-shape)
    (stroke-weight 3)
    ;; (stroke-weight (mod (mod @*time* 160) 35))
    ;; (fill r g b a)
    (doseq [[[x y] t] pts
	    ;; [[x y] w] (map #(vector %1 %2)
	    ;; 		   pts
	    ;; 		   (cycle (concat (range 1 10)
	    ;; 				  (range 10 1))))
	    ]
      
      ;; (curve-vertex x y)
      
      (ellipse x y 3 3)
      
      )
    (end-shape)
    ))

;; # The necessary evils [globals]

(def ^:dynamic *world*     (ref {})) 
(def ^:dynamic *screen-width*
     1024
     ;; 1680
     ;; 1920
     ;; 800
     )
(def ^:dynamic *screen-height*
     768
     ;; 1050
     ;; 1200
     ;; 700
     )
(def ^:dynamic *time*    (atom   0))
(def ^:dynamic *fps*     (atom 0.0))
(def ^:dynamic *font*    (atom nil))

(def ^:dynamic *tuio-port* 3333)
(def ^:dynamic *tuio* (tuio/client *tuio-port*))



;; ## Coordinates
(defn ->screen [[x y]]
  [(* x *screen-width*) (* y *screen-height*)]
  ;; [(* x @WIDTH) (* y @HEIGHT)]
  )

(defn ->world [[x y]]
  [(* x *screen-width*) (* y *screen-height*)]
  ;; [(/ x @WIDTH) (/ y @HEIGHT)]
  )




(defn draw-hypno
  [x y]
  (let [nleaves 12
	t @*time*]
    (with-translation [x y]
      (with-rotation [(mod t TWO_PI)]
	(doseq [ang (range 0 TWO_PI (/ TWO_PI nleaves))]
	  (with-rotation [ang]
	    (with-translation [0 (/ (+ 150 (abs (- 60 (mod t 120)))) 2)]
	      (fill 20 100 190 100)
	      (no-stroke)
	      (ellipse 0 0 10 10)))))

      (with-rotation [(- (mod t TWO_PI))]
	(doseq [ang (range 0 TWO_PI (/ TWO_PI nleaves))]
	  (with-rotation [(- HALF_PI ang)]
	    (with-translation [0 (/ (+ 140 (abs (- 60 (mod t 120)))) 4)]
	      (fill 100 20 190 100)
	      (no-stroke)
	      (ellipse 0 0 6 6)))))
	

      (with-rotation [(mod t TWO_PI)]
	(doseq [ang (range 0 TWO_PI (/ TWO_PI nleaves))]
	  (with-rotation [ang]
	    (with-translation [0 (/ (+ 120 (abs (- 60 (mod t 120)))) 8)]
	      (fill 100 190 20 100)
	      (no-stroke)
	      (ellipse 0 0 3 3)))))

            ;; (with-rotation [(/ PI 4)]
      ;; 	(fill 220 0 90 90)
      ;; 	(rect-mode CORNER)
      ;; 	;; (rect  0 0 20 20)

      ;; 	;; (shape :triangles
      ;; 	;;      [ 0     30]
      ;; 	;;      [-15   -30]
      ;; 	;;      [ 15   -30])
      ;; 	)
      )))



(defn hgraph
  [& {:keys [f1 f2 phase1 phase2 r1 r2 thyme]
      :or {f1 440
	   f2 440
	   phase1 1
	   phase2 0
	   r1     200
	   r2     200
	   thyme  333}}]
  (let [thyme         thyme
	[w h :as dim] [*screen-width* *screen-height*]
	a             0	;; (/ w 4)
	b             0	;; (/ h 4)
	r1            r1 
	r2            r2
	f1            f1
	f2            f2
	damp          (/ 1 (Math/pow thyme 1.11))
	phase1        phase1
	phase2        phase2
	d             1]

    (map first
	 (drop 1 (reductions (fn [[pos d] t]
			       (let [t (* 0.01 t)]
				 [[(+ (+ a (* d (* r1 (cos (+ (* f1 t) phase1)))))
				      (+ b (* d (* r2 (cos (+ (* f2 t) phase2))))))
				   (+ (+ a (* d (* r1 (sin (+ (* f1 t) phase1)))))
				      (+ b (* d (* r2 (sin (+ (* f2 t) phase2))))))]
				  (- d damp)]))
			     [1 0]
			     (range 0 thyme))))))


;; # Handling cursors
(def ^:dynamic *cursors*   (atom {}))

;; ## Constructor
(defn new-cursor
  [& {:keys [pos id vel status]}]
  {:pos (or pos [0 0])
   :id  (or id  -1)
   :vel (or vel [0 0])
   :last-seen {:pos  (or pos [0 0])
	       :time (time-now)}
   :status (or status :added)})

(defn add-cursor
  [c]
  (swap! *cursors* assoc (:id c) c)) ;don't just blindly assoc -- consider merge 

(defn upd-cursor
  [c]
  (let [id (:id c)]
    (swap! *cursors* update-in [id] merge c)))

(defn rem-cursor
  ([id]
     (swap! *cursors* update-in [id] merge {:status :removed}))
  ([id pos time]
     (swap! *cursors* update-in [id] merge {:status   :removed
					    :last-seen {:pos pos
							:time time}})))

;;  ## Measuring framerate
(let [counter   (atom  0)
      prev-time (atom -1)
      max-count      120]

  (defn compute-fps
    []
    (let [now (time-now)
	  prev @prev-time
	  count @counter]
      (if (= count max-count)
	(do (reset! *fps* (* 1000.0 (/ count (- now prev))))
	    (reset! prev-time now)
	    (reset! counter     0))
	(swap! counter inc)))))


(defn draw-grid
  []
  (let [dx 15
  	dy 15]
    (doseq [i (range) :while (< (* i dx) *screen-width*)]
      (with-translation [(* i dx) 0]
  	(stroke-weight 1)
  	(stroke;; -float
	 55)
  	(line 0.5 0.5 0.5 *screen-height*)))

    (doseq [j (range) :while (< (* j dy) *screen-height*)]
      (with-translation [0 (* j dy)]
  	(stroke-weight 1)
  	(stroke;; -float
	 55)
  	(line 0.5 0.5 *screen-width* 0))))) 



(def display-curve?  (atom  true))
(def display-hypno?  (atom false))
(def display-bounds? (atom false))

(def default-player  #'meep)

(def default-players {0 #'beep
		      1 #'meep
		      2 #'bop})

(def ^{:dynamic true} *players*
      (atom {0 #'beep
	     1 #'meep
	     2 #'bop}))

(defn reset-player-defaults
  []
  (reset! *players* default-players))

(defn add-player
  [id play-fn]
  (swap! *players* assoc id play-fn)
  id)

(defn rem-player
  [id]
  (swap! *players* assoc id
	 (get default-players id
	      (rand-nth (vals default-players))))
  id)

(defn get-player
  [id]
  (get @*players* id default-player))

(defn draw-cursor-path
  [c {:keys [r g b a] :as color}]
  (let [[x y] (:pos c)
	path  (:path c)
	path-length (count path)
	current-pos (mod @*time* path-length)
	[[xa ya] t] (nth path current-pos)
	{:keys [bounds width height]} c]
    (when @display-bounds?
     (rect-mode CORNER)
     (no-fill)
     (stroke 12 5 215 189)
     (rect (:min-x bounds) (:min-y bounds) width height))
    (when @display-curve?
      (draw-curve path color))
    (when @display-hypno?
      (try
	(draw-hypno xa ya)
	(no-stroke)
	(fill 24 175 195 190)
	(ellipse xa ya 14 14)
	(let [base  (frequency :a 4)
	      freq  (-> (abs (- (:max-y (:bounds c)) ya))
			(/ (:height c))
			(* base)
			(+ base))
	      tuned (auto-tune freq :eb :pentatonic)
	      hz    (if (auto-tuned?) tuned freq)
	      plyfn (get-player (:id c))
	      ;; plyfn (())
	      ]
	  (cond (= 1 path-length)
	  	:nothing
		
	  	(zero? current-pos)
	  	(meep hz)
		
	  	(= current-pos (dec path-length))
	  	(tone/stop)

	  	:otherwise
	  	(tone/ctl meep :freq hz))
	  )

	;; (println [current-pos path-length])
	;; (when (and (ot/connected?) (= current-pos (dec path-length)))
	;;   ;; (otp/piano (+ 44 (rand-int 44)))
	;;   )
	 
	(catch Exception e (.printStackTrace e))))
    ;; (with-translation [x y]
    ;;   (fill-float 190 110 190 180)
    ;;   (no-stroke)
    ;;   (ellipse-mode CENTER)
    ;;   (ellipse 0 0 5 5))
    ))

(let [tuio->pen  (atom {})
      pen->color (atom {0     {:r 120 :g  50  :b 150 :a 255}
			1     {:r  50 :g 200  :b  50 :a 255}
			2     {:r  50 :g  50  :b 200 :a 255}
			3     {:r 200 :g  50  :b  50 :a 255}
			4     {:r  50 :g 200  :b  150 :a 255}
			5     {:r  150 :g 150  :b 200 :a 255}
			6     {:r  50 :g  250  :b 200 :a 155}
			:none {:r  20 :g 210  :b 190 :a 180}})]
  (defn tuioID->penID
    "returns the current penID registered to a given TUIO-ID "
    [id]
    (get @tuio->pen id))
  
  (defn set-penID!
    "sets the tuio-id to point to the given pen-id"
    [tuio-id pen-id]
    (swap! tuio->pen assoc tuio-id pen-id))

  (defn penID->color [id]
    {:r 120 :g  50  :b 150 :a 255}
    ;; (get @pen->color id {:r 0 :g 255 :b 20 :a 180})
    ))

(defn draw-active-paths
  []
  (doseq [[k c] @*cursors*
	  :let [pid (tuioID->penID (:id c))
		color (penID->color pid)]]
    (if (not= :removed (:status c))
      (draw-cursor-path c color))))



(def ^:dynamic *curves* (atom    #{}))

(let [scalar 500]
  (defn d-point
    ([p]
       (d-point p 4))
    
    ([p s]
       (let [ ;;[w h] (normalize (:veloc p) 5 15)
	     ;; scalr0 (mod @*time* 400)
	     ]
	 (no-fill)
	 (stroke 120 50 150 255)
	 (ellipse 0 0 s s))))

  (defn d-path
    "a path has an origin, points, scale-factor"
    [path-map]
    (let [{:keys [origin width height path]} path-map
	  path-origin (->screen origin)]
      (with-translation path-origin

	(when @display-bounds?
	  (rect-mode CORNER)
	  (no-fill)
	  (stroke 12 5 215 189)
	  (rect 0 0 (* width *screen-width*) (* height *screen-height*)))

	(stroke-weight 4)
	;; (begin-shape)
	(doseq [point path]
	  (let [[x y] (:pos point)
		sx    (* x width  *screen-width*)
		sy    (* y height *screen-height*)
		;; sc    (mod @*time* 40) ; controls ellipse size 
		]
	    (with-translation [sx sy]
	      ;; (curve-vertex 0 0)
	      (d-point point) 
				 )))
	;; (end-shape)
	)))

  (defn d-layer
    [l]
    (doseq [c (:contents l)]
      ((:draw c) c))))

(def ^:dynamic *committed-paths* (atom {}))

(defn draw-committed-paths
  []
  (doseq [[k p] @*committed-paths*]
    (when-let [color (and (:visible? p) (penID->color (:owner p)))]
      (draw-cursor-path p color) 
				    )))


; ## book-keeping
(let [path-counter (atom 0)]
  
  (defn new-path-id
    []
    (dosync
     (let [id @path-counter]
       (swap! path-counter inc)
       id)))

  (defn reset-path-counter!
    []
    (reset! path-counter 0))

  (defn commit-path
    [path {:keys [owner visible? sys-time tuio-time]
	   :or   {owner     :none
		  visible?  true
		  sys-time  (time-now)
		  tuio-time (tuio/tuio-time)}
	   :as config}]

    (let [id     (new-path-id)
	  bounds (path-bounds path)
	  width  (unchecked-subtract (bounds :max-x) (bounds :min-x))
	  height (max 1 (unchecked-subtract (bounds :max-y) (bounds :min-y)))]
      (swap! *committed-paths* assoc id {:id       id
					 :path     path
					 :bounds   bounds
					 :width    width
					 :height   height
					 :owner    owner
					 :visible? visible?
					 :time     {:system-time sys-time 
						    :tuio-time   tuio-time}})))
     
  
  (defn clear-path-history
    []
    (dosync
     (reset! *committed-paths* {})
     (reset-path-counter!))))
  




;; # Scene-Graph

;; (def *entities* (atom {}))

;; (defn entity [{:keys [val update draw]}]
;;   {:val    val
;;    :update update
;;    :draw   draw})

;; (defn draw-entity [e]
;;   ((:draw e) (:val e)))


(def display-fps?   (atom true))
(def display-grid?  (atom false ;; true
			  ))

(defn update
  []
  (when @display-fps? (compute-fps))
  (swap! *time* inc)
  )

(defn draw-fps [fps pos]
  (when @*font*
    (text-font @*font* 20)
    (fill 255)
    (with-translation pos
      ;; (text (format "FPS: %.2f" fps) 0 0)
      )))

(def clear-screen?  (atom true))
(def paused?       (atom false))


(defn draw-clock [x y]
  (with-translation [x y]
    (with-rotation [0]
      (no-stroke)	  
      (fill 120 220 190 255)
      (let [lo      0
	    hi    400
	    step    8]
	(doseq [r (range lo hi step)]
	  (let [x (* 10 (sin (- (->new-range r [lo hi] [0 (* 1 TWO_PI)])
				  PI)))
		w (->new-range r [lo hi] [4 8])]
	    (ellipse x r w w)))))))



(defn interpolate
  "returns a seq of points between point1 and point2"
  [p1 p2 step]
  (let [[x1 y1] p1
	[x2 y2] p2
	m (/ (- y2 y1)
	     (- x2 x1))]
    ;; step from
    (comment "http://www.npr.org/2011/06/12/137138008/silk-road-not-your-fathers-amazon-com")
    ))

(let [innr 100
      outr 300
      diff (- outr innr)]
  (defn d-cube [c]
    (let [[h k] (-> c :pos ->screen)
	  big   [(- outr) (- k outr)]
	  lil   [(- innr) (- k outr)]]
      (rect-mode CENTER)
      (no-fill)
      (stroke-weight 4)
      (stroke 125 125 225 200)
      (with-translation [h k]
	(rect 0 0 outr outr)
	(rect 0 0 innr innr)
	(line (- (half outr)) (- (half outr))
	      (- (half innr)) (- (half innr)))))))

(def txts (atom nil))

(defn draw-proxy []
  (if @paused?
    (do
      (update)

      (when @clear-screen?
	(background 0)
	(when @display-grid?
	  (draw-grid)))
      (when @display-fps?
	(draw-fps @*fps* [90 30]))

      (with-translation [(half *screen-width*)
			 (half *screen-height*)]
	(with-scale [(nth (cycle (range 0.15 0.95 0.02))
			  (mod @*time* 200))]
	  ;; (let [i @img]
	  ;;   (if (and (> (.width  i) 0)
	  ;; 	     (> (.height i) 0))
	  ;;     (image i 0 0)))
	  ))

      (doseq [t @txts]
	(render t))

      (draw-active-paths)
      (draw-committed-paths)
			    
      (doseq [c @*curves*] ;; curves stored in a Set
	(d-path c))

      (doseq [c @*cubes*] ;; cubes stored in a Map
	(d-cube (val c)))
      ;; (doseq [ys (range (-> *screen-height* (/ 2))
      ;; 			(-> *screen-height* (/ 2) (+ 40))
      ;; 			5)]
      ;; 	(with-translation [(half *screen-width*) ys]
      ;; 	  (draw-curve ezhk {:r 15 :g  100  :b 122 :a 180 })))
      
     
      ;; (draw-clock (half *screen-width*) (half *screen-height*))
      
      )
    ;; (do
    ;;   (with-translation [(/ *screen-width* 2) (/ *screen-height* 2)]
    ;; 	(text (format "PAUSED at time: %d" @*time*) 0 0)))
    ))

(defn draw [state]
  (draw-proxy)
  state)

(defn init-world []
  (reset! *time* 0)
  ;; (reset! *entities* {})
  )


(defn add-tuio-cursor [c]
  (let [id  (.getCursorID ^TuioCursor c)
	pos (tuio-point->pos c *screen-width* *screen-height*)
	now (tuio/tuio-time)
	known-cursor (get @*cursors* id)
	bounds (path-bounds [[pos now]])
	width  (unchecked-subtract (bounds :max-x) (bounds :min-x))
	height (max 1 ;; avoid the dreaded height 0 
		    (unchecked-subtract (bounds :max-y) (bounds :min-y)))]
    ;;;; Determine which pen this cursor corresponds to
    ;; - via known-cursor?
    ;; - check for recently-removed cursors in local region.
    ;;    -- is this actually a new cursor? should this be considered a cursor update instead?
    ;; - 
    (set-penID! id id)
    (add-cursor {:id  id
		 :pos pos
		 :player (get-player id) 
		 :status :added
		 :bounds   bounds
		 :width    width
		 :height   height
		 :path   [[pos now]]
		 :last-seen {:pos pos
			     :time now}})))


;; handle non-responsive cursors
;; 

(let [thresh 4000]
  (defn commit-inactive-cursors []
    (let [now  (tuio/tuio-time)]
      (doseq [c (-> *cursors* deref vals)]
	(let [then (-> c :last-seen :time)]
	  (when (< thresh (- now then))
	    (commit-path (:path c) {})
	    (swap! *cursors* dissoc (:id c))))))))


(defn upd-tuio-cursor [c]
  (let [id  (.getCursorID ^TuioCursor c)
	pos (tuio-point->pos c *screen-width* *screen-height*)
	now (tuio/tuio-time)
	known-cursor (get @*cursors* id)]
    (upd-cursor {:id     id
		 :pos    pos
		 :status :updated
		 :path   (if known-cursor
			   (conj (:path known-cursor) [pos now])
			   (tuio-path->clj (.getPath ^TuioCursor c)))
		 :last-seen {:pos pos
			     :time now}})))
(defn rem-tuio-cursor [c]
  (let [id (.getCursorID ^TuioCursor c)
	pos (tuio-point->pos c *screen-width* *screen-height*)
	now (tuio/tuio-time)
	known-cursor (get @*cursors* id)]
    ;; (println "remove cursor: " id)
    (when known-cursor
      (let [path (:path known-cursor)
	    new-path (conj path [pos now])]
	(upd-cursor {:id id
		     :pos pos
		     :status :removed
		     :path new-path
		     :last-seen  {:pos pos :time now}})
	(commit-path new-path {:owner (tuioID->penID id)
			       :sys-time (time-now)
			       :tuio-time now})
	(swap! *curves* conj (TuioPath->Path (.getPath c)))))))





;;; Sifteo Cubes
;;

(def ^{:dynamic true} *cubes* (atom  {}))


(def  ^{:dynamic true} *id-mapping*
      (atom {"32ffd7053148303247282143"   85
	     "39ffda053148393850351243"   86}))
;;           ;Sifteo                     ;TUIO
;; hard-coded for now... TODO: sifteo-tuio-id-pairing
;; .. include a watcher that recomputes *id-mapping*'s inverse on changes ... (maybe)

(defn ->tuio-id
  [cube-id]
  (get @*id-mapping* cube-id) :not-found)

(let [reverse-mapping (map-invert @*id-mapping*)]
  (defn ->cube-id
    [tuio-id]
    (get reverse-mapping tuio-id :not-found)))


(defmacro cube
  ;; Perhaps this should accept sifteo-IDs as well as tuio-IDs?
  [id]
  `(get @*cubes* ~id))


;;;; a with-tuio macro may really clean up these event-handlers
;;
;; (with-tuio [t tobj]
;;   (swap! *cubes* (partial merge-with into) {(:id t) t}))
;;;;


(defn add-tuio-object [tobj]
  (let [fid (.getSymbolID tobj)
	angle (.getAngle tobj)
	pos [(.getX tobj) (.getY tobj)]
	moving? (.isMoving tobj)
	mov-speed (.getMotionSpeed tobj)
	mov-accel (.getMotionAccel tobj)
	rot-speed (.getRotationSpeed tobj)
	rot-accel (.getRotationAccel tobj)]
    (swap! *cubes* (partial merge-with into)
	   {fid {:id fid
		 :pos pos
		 :speed mov-speed
		 :accel mov-accel
		 :angle angle
		 :angular-speed rot-speed
		 :angular-accel rot-accel
		 :moving? moving?
		 :status :add
		 :time-last (time-now)}})))

(defn upd-tuio-object [tobj]
  (let [fid (.getSymbolID tobj)
	angle (.getAngle tobj)
	pos [(.getX tobj) (.getY tobj)]
	moving? (.isMoving tobj)
	mov-speed (.getMotionSpeed tobj)
	mov-accel (.getMotionAccel tobj)
	rot-speed (.getRotationSpeed tobj)
	rot-accel (.getRotationAccel tobj)]
    (swap! *cubes* (partial merge-with into)
	   {fid {:id fid
		 :pos pos
		 :speed mov-speed
		 :accel mov-accel
		 :angle angle
		 :angular-speed rot-speed
		 :angular-accel rot-accel
		 :moving? moving?
		 :tuio-status :update
		 :time-last (time-now)}})))

(defn rem-tuio-object [tobj]
  (let [fid (.getSymbolID tobj)
	angle (.getAngle tobj)
	pos [(.getX tobj) (.getY tobj)]
	moving? (.isMoving tobj)
	mov-speed (.getMotionSpeed tobj)
	mov-accel (.getMotionAccel tobj)
	rot-speed (.getRotationSpeed tobj)
	rot-accel (.getRotationAccel tobj)]
    (swap! *cubes* (partial merge-with into)
	   {fid {:id fid
		 :pos pos
		 :speed mov-speed
		 :accel mov-accel
		 :angle angle
		 :angular-speed rot-speed
		 :angular-accel rot-accel
		 :moving? moving?
		 :tuio-status :remove
		 :time-last (time-now)}})))

(defn init-tuio []
  (try (tuio/connect! *tuio*)
       (doto *tuio*
	 (tuio/on-add-cursor!    curs (add-tuio-cursor curs))
	 (tuio/on-update-cursor! curs (upd-tuio-cursor curs))
	 (tuio/on-remove-cursor! curs (rem-tuio-cursor curs))
	 (tuio/on-add-object!    tobj (add-tuio-object tobj))
	 (tuio/on-update-object! tobj (upd-tuio-object tobj))
	 (tuio/on-remove-object! tobj (rem-tuio-object tobj)))
       (println "TUIO init successful.")
       (catch Exception e (.printStackTrace e))))




;; OSC message handlers

;; (def voices #{ beep bop })
(let [key* (atom {:chroma :eb :scale  :pentatonic})  ;; this could/"should" be moved to global scopre
      auto-tune?*   (atom true)
      octv-shift*   2 ;; nick's program currently sends octaves in range 1-4
      ]


  ;; (defn quick-beep [4 ]
  ;;   (let
  ;; 	[{:keys [chroma scale]}  @key*n
  ;; 	 ;; [octave ratio :as args] (:args msg)
  ;; 	 ;; (unchecked-add octave octv-shift)
  ;; 	 octave 4 
  ;; 	 ratio (abs (- ))
  ;; 	 base  (frequency chroma octave)
  ;; 	 freq  (-> ratio (* base) (+ base))
  ;; 	 tuned (auto-tune freq chroma scale)
  ;; 	 hz    (if @auto-tune? tuned freq)])
  ;;   (println "MSG: "            args
  ;; 	     "\tTime: "         (System/currentTimeMillis)
  ;; 	     "\n Base  freq: "   base
  ;; 	     "\n Raw   freq: "   freq
  ;; 	     "\n Tuned freq: "  tuned))

  (defn auto-tuned? [] @auto-tune?*)
  
  ;; and this
  (defn set-auto-tune! [tf] 
    (reset! auto-tune?* tf))

  ;; and this
  (defn set-key!
    ([k]
       (set-key! k :ionian))
    ([k s]
       (swap! key* assoc :chroma k
   	                 :scale  s)))

  ;; and this
  (defn set-scale! [s]
    (swap! key* assoc :scale s))  

  
  (defn curve-hdlr
    "Called when we receive /playCurve message.
     The /playCurve message contains 
      the following floating-point  arguments:
      [ octave, begin/end, path-length, avg-velocity ]

      where octave is interpreted as the octave curve will be played,
       and begin/end serves as a true/false flag for starting/stopping
       the instrument.

     *- octave is shifted by an amount [octv-shift*]
        defined in the enclosing scope"
    [msg]
    (let [[octave on-off curve-id] (:args msg)
	  curve-id (int curve-id)
    	  octave   (unchecked-add octave octv-shift*)
    	  freq     (frequency (:chroma @key*) octave)
	  player   (get-player curve-id)
	  ;; _      (println "on/off: " on-off " curve: " curve-id " player: " player)
	  ;; _      (println "curveMSG: [octave on/off id]: " (:args msg)
	  ;; 		  "\t base freq: "              freq)
	  ]
      (if (pos? on-off)
	(add-player curve-id (player freq))
	(do ;; (println "!!!!!__ " curve-id " , " on-off " , " player "__!!!!!")
	    (tone/kill player)
	    (rem-player curve-id)))))

  (defn note-hdlr 
    [msg]
    (let [{:keys [chroma scale]}  @key*
    	  [octave ratio dist vel curve-id :as args] (:args msg)
	  curve-id (int curve-id)
    	  octave (unchecked-add octave octv-shift*)
    	  base  (frequency chroma octave)
    	  freq  (-> ratio (* base) (+ base))
	  player (get-player curve-id)
    	  tuned (auto-tune freq chroma scale)
    	  hz    (if (auto-tuned?) tuned freq)]
      ;; (println "noteMSG: "            args
      ;; 	       ;; "\tTime: "         (System/currentTimeMillis)
      ;; 	       ;; "\n Base  freq: "   base
      ;; 	       ;; "\n Raw   freq: "   freq
      ;; 	       ;; "\n Tuned freq: "  tuned
      ;; 	       )
      (when (number? player)
	(tone/ctl player :freq hz)))))

(defn init-osc []
  (try (comm/init-osc)
       (comm/handle "/playCurve" #'curve-hdlr)
       (comm/handle "/playNote"  #'note-hdlr)
       (println "osc-handler init successful.")
       (catch Exception e (.printStackTrace e))))

(def img (atom nil))
(defn load-image-assets []
  (reset! img (request-image "protoman.jpg" "jpg")))

(defn init []
  (init-world)
  (init-tuio)
  (smooth)
  (framerate 60)  
  (reset! *font* (create-font  "Monaco-48.vlw" 24))
  (init-osc)
  (load-image-assets)
  ;; (ot/connect 2345)
  )

(defn kill-tuio []
   (tuio/disconnect! *tuio*)
   (println "tuio client disconnected."))

(defn kill-osc []
  (comm/kill-osc)
  (println "osc connections closed."))

(defn kill [app]
  (kill-tuio)
  (kill-osc)
  ;; (ot/quit)
  (stop app))

;; (defn erase-board [])

(defn key-pressed-proxy [;; evt
			 ]
  (let [char (last-key)      ;; (.getKeyChar evt)
	code (last-key-code) ;; (.getKeyCode evt)
	]
    (println "key-pressed: " char)
    (case char
	  \space  (do (clear-path-history)
		      (reset! *curves* #{})
		      (tone/stop)
		      (reset-player-defaults))
	  (\a \A) (swap! display-bounds? not) 
	  (\b \B) (swap! clear-screen?  not)
	  (\c \C) (swap! display-curve? not)
	  (\f \F) (swap! display-fps?   not)
	  (\g \G) (swap! display-grid?  not)
	  (\h \H) (swap! display-hypno? not)
	  (\p \P) (swap! paused?        not)
	  :else-do-nothing)))

(defn key-pressed [state]
  (key-pressed-proxy)
  state)

(defn text-image
  [s]
  (make-text-obj :txt s
		 :pos [(rand) (rand)]
		 :rot 0.0))

(defn text-images
  " [String] -> [GLTexture] "
  [ss]
  (doall (map text-image ss)))


(defn add-text! [s]
  (if (seq? s)
    (text-images s)
    (text-image s)))


(defn setup [s]
  (size *screen-width* *screen-height* GLConstants/GLGRAPHICS
				       ;; OPENGL
	                                        )

  (println ";;;;;;;;;;;;;;;;|GROKBOARD|;;;;;;;;;;;;;;;;;") ;
  (println ";;;;;;;;;;;;;;;;|____.____|;;;;;;;;;;;;;;;;;")
  (init)
  (reset! *offsb* (create-graphics 500 200 P2D))
  (reset! txts (doall (map #(make-text-obj :txt %1
					   :pos [(rand) (rand)]
					   :rot 0.0)
			   (map str (-> *ns* ns-publics keys sort)))))
  (println ";;;;;;;;;;;;;;;;|____.____|;;;;;;;;;;;;;;;;;")
  (println ";;;;;;;;;;;;;;;;|!!LIVES!!|;;;;;;;;;;;;;;;;;")
  s)


(defapplet sketch
  :title "Clojure Multi-Touch Canvas"
  :setup setup
  :draw  draw
  :size  [*screen-width* *screen-height*]
  ;; :mouse-moved mouse-moved
  ;; :mouse-pressed mouse-pressed
  ;; :mouse-released mouse-released
  ;; :mouse-dragged mouse-dragged
  ;; :mouse-entered mouse-entered
  ;; :mouse-exited mouse-released
  :key-pressed key-pressed
  ;; :key-released key-released
			       )

 (run sketch :interactive)
;(kill sketch)
;(stop sketch)

;;TUIOUpdate---.
;;              \
;;               \
;;               |
;;             |/
;;;; The case for channels ;;;;
;;                           ;;
;; -  Gesture Recognition  - ;;
;; -  Collision Detection  - ;;
;; -  Force Application    - ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Component ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - id                                     ;;
;; - pos                                    ;;
;; - preferred-size                         ;;
;; - current-size  (derive a SCALE factor)  ;;
;; - render-fn                              ;;
;; - event-handlers                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gesture-Recognition ;;;;
;; - id ;;;;;;;;;;;;;;;;;;;;;
;; - pattern
;; - timeout mechanisms (timeouts vary from pattern to pattern?)
;; - tracking
;; - ;;
;; - ;;
;; - ;;
;; - ;;
