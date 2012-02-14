(ns audsk.utils.misc
  (:use [overtone.live]))


;; (hz-field :c :minor 4 2)
(defn hz-field
  "Returns a list of frequencies for the supplied key & scale.
   
   Usage: (hz-field :a :minor 4 1)
           => (440.0 493.88 523.25 587.32 659.25 698.45 783.99)"
  ([root scale-name start-oct]
     (hz-field root scale-name start-oct 1))
  ([root scale-name start-oct num-octs]
     (let [scale-len (count (scale-name SCALE))
	   begin     (* scale-len (inc start-oct))
	   amount    (* scale-len num-octs)
	   m-notes   (subvec (vec (scale-field root scale-name)) 
			     begin
			     (+ begin amount))
	   freqs     (map midi->hz m-notes)]
       freqs)))

(defn- auto-tune-helper
  "Helper function. Prefer the more user-friendly (auto-tune) function."
  [freq hz-field]
  (letfn [(nearest-freq
	   [f hzs]
	   (second
	    (reduce (fn [[f fa] fb]
		      (if (= :none fa)
			[f fb]
			(let [dfa (Math/abs (- fa f))
			      dfb (Math/abs (- fb f))]
			  (if (< dfb dfa)
			    [f fb]
			    [f fa]))))
			 
		    [f :none]
		    hzs)))]
    (nearest-freq freq hz-field)))

(defn auto-tune [f root scale-name]
  (let [hz-field (map midi->hz (scale-field root scale-name))]
    (auto-tune-helper f hz-field)))

(defn frequency
  "Given chroma and octave, return Hz.

  Usage: (frequency :a 4)
          => 440.0"
  [chroma octave]
  (let [name (str (name chroma) (int octave))
	midi (note name)]
    (midi->hz midi)))

(def pos-infinity Double/POSITIVE_INFINITY)
(def neg-infinity Double/NEGATIVE_INFINITY)

(defn path-bounds
  "Returns map containing the following keys: (:min-x :min-y :max-x :max-y)"
  [pts]
  (reduce (fn [m [p t]]
	    (assoc m
	      :min-x (min (first  p) (:min-x m))
	      :min-y (min (second p) (:min-y m))
	      :max-x (max (first  p) (:max-x m))
	      :max-y (max (second p) (:max-y m))))
	  {:min-x pos-infinity
	   :min-y pos-infinity
	   :max-x neg-infinity 
	   :max-y neg-infinity}
	  pts))



(defmacro normalize
  [value origin range]
  `(-> ~value
       (- ~origin)
       (/ ~range)))

(defn ->new-range
  "maps value from range [lo1 hi1] to range [lo2 hi2]."
  [val [lo1 hi1] [lo2 hi2]]
  (let [part1  (- val lo1)
	range1 (- hi1 lo1)
	range2 (- hi2 lo2)]
    (-> val 
	(- lo1)
	(/ range1)
	(* range2)
	(+ lo2))))



;; (defn average-vel )