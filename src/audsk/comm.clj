(ns audsk.comm
  (:use	[overtone.osc]))


(def SRV-PORT              5995)
(def CLI-PORT             12000)
(def CLI-ADDR "143.215.108.242")

(def nick-srv (atom nil))
(def nick-cli (atom (osc-client CLI-ADDR CLI-PORT)))

(defn init-osc
  ([]
     (init-osc SRV-PORT))
  ([port]
     (reset! nick-srv (osc-server port))))

(defn kill-osc []
  (try (osc-close @nick-srv)
       (osc-close @nick-cli)
    (catch Exception e
      (.printStackTrace e))))

(defn handle
  "The handler takes a message map with the following keys:
   {:src-host, :src-port, :path, :type-tag, :args}"
  [path handler]
  (osc-handle @nick-srv path handler))


(comment
  ;; recv
  (osc-handle @nick-srv "/playNote" (fn [msg] (println (:args msg))))

  ;; send
  (doseq [val (range 10)]
    (osc-send @nick-cli "/test" "i" val)))