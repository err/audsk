(ns audsk.tuio
  (:import [TUIO TuioClient]
	   [TUIO TuioContainer]
	   [TUIO TuioCursor]
	   [TUIO TuioListener]
	   [TUIO TuioObject]
	   [TUIO TuioPoint]
	   [TUIO TuioTime]))

(defmacro on-add-object! [client data cb]
  `(. ~client addTuioListener
      (reify TuioListener 
       (TuioListener/addTuioObject [self d#] (let [~data d#] ~cb))
       (updateTuioObject [self d#] )
       (removeTuioObject [self d#] )
       (addTuioCursor    [self d#] )
       (updateTuioCursor [self d#] )
       (removeTuioCursor [self d#] )
       (refresh          [self d#] ))))

(defmacro on-update-object! [client data cb]
  `(. ~client addTuioListener
      (reify TuioListener 
        (addTuioObject    [self d#] )
        (updateTuioObject
	 [self d#]
	 (let [~data d#] ~cb)
	 ;; (println "\nx: " (.getScreenX d# 1024)
	 ;; 	  " y: " (.getScreenY d# 768))
	 )
        (removeTuioObject [self d#] )
        (addTuioCursor    [self d#] )
        (updateTuioCursor [self d#] )
        (removeTuioCursor [self d#] )
        (refresh [self d#]))))

(defmacro on-remove-object! [client data cb]
  `(. ~client addTuioListener
      (reify TuioListener
        (addTuioObject    [self d#] )
        (updateTuioObject [self d#] )
        (removeTuioObject
	 [self d#]
	 (let [~data d#] ~cb))
        (addTuioCursor    [self d#] )
        (updateTuioCursor [self d#] )
        (removeTuioCursor [self d#] )
        (refresh [self d#]))))

(defmacro on-add-cursor! [client data cb]
  `(. ~client addTuioListener
      (reify TuioListener
        (addTuioObject    [self d#] )
        (updateTuioObject [self d#] )
        (removeTuioObject [self d#] )
        (addTuioCursor
	 [self d#]
	 (let [~data d#] ~cb))
        (updateTuioCursor [self d#] )
        (removeTuioCursor [self d#] )
        (refresh [self d#])
	)))

(defmacro on-update-cursor! [client data cb]
  `(. ~client addTuioListener
      (reify TuioListener
        (addTuioObject    [self d#] )
        (updateTuioObject [self d#] )
        (removeTuioObject [self d#] )
        (addTuioCursor    [self d#] )
        (updateTuioCursor
	 [self d#]
         (let [~data d#] ~cb))
        (removeTuioCursor [self d#] )
        (refresh [self d#]))))

(defmacro on-remove-cursor! [client data cb]
  `(. ~client addTuioListener
      (reify TuioListener
        (addTuioObject    [self d#] )
        (updateTuioObject [self d#] )
        (removeTuioObject [self d#] )
        (addTuioCursor    [self d#] )
        (updateTuioCursor [self d#] )
        (removeTuioCursor
	 [self d#]
	 (let [~data d#] ~cb))
        (refresh [self d#]))))

(defmacro on-refresh! [client data cb]
  `(. ~client addTuioListener
      (reify TuioListener
        (addTuioObject    [self d#] )
        (updateTuioObject [self d#] )
        (removeTuioObject [self d#] )
        (addTuioCursor    [self d#] )
        (updateTuioCursor [self d#] )
        (removeTuioCursor [self d#]  )
        (refresh
	 [self d#]
	 (let [~data d#] ~cb)))))

(defn remove-all-listeners! [c]
  (.removeAllTuioListeners c))

(defn client  
  ([] (TuioClient.))
  ([port] (TuioClient. port)))

;
(defn connect! [c]
  (.connect c)
  (TuioTime/initSession))

(defn disconnect! [c] (.disconnect c))

(defn connected? [c]
  (.isConnected? c))

(defn cursors [c]
  (.getTuioCursors c))

(defn objects [c]
  (.getTuioObjects c))

(defn cursor
  "Gets a TUIO cursor on ID."
  [c id]
  (first (filter (fn [c]
                   (let [id2 (.getCursorID c)]
                     (= id2 id)))
                 (cursors c))))

(defn tuio-time []
  (. (TuioTime/getSessionTime) getTotalMilliseconds))


(comment
  (def c (client))

  (doto c
    (on-add-object! #(println "add object"))
    (on-update-object! #(println "update object"))
    (on-remove-object! #(println "remove object"))
    (on-add-cursor! #(println "add cursor"))
    (on-update-cursor! ;; #(println "\nx: " (.getScreenX d# 1024)
		       ;; 		 " y: " (.getScreenY d# 768))
		        (println "update cursor"))
    (on-remove-cursor! #(println "remove cursor"))
    (on-refresh! #(println "refresh")))

  (connect! c)
  (disconnect! c))

