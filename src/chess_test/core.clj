(ns chess-test.core
  (:require
   [org.httpkit.server :as http]
   [compojure.core     :as cmpj]
   [chess-test.moves   :as m]
   [chess-test.board   :as b]
   [chess-test.state   :as s]))

(defonce channels (atom #{}))

(defn ws!
  "Basic websocket dispatcher"
  [action msg & status]
  (case action
    :connect    [(println "Channel open: " msg)
                 (swap! channels conj msg)]
    :disconnect [(println "Channel closed: " status)
                 (swap! channels #(remove #{msg} %))]
    :notify     [(println "Notifiying channels: " msg)
                 (doseq [channel @channels] (http/send! channel msg))]
    :no-op))

(defn ws-handler
  "Handles all websocket requests"
  [req]
  (println "Hitting ws-handler. REQUEST: " req)
  (http/with-channel req channel
    (ws! :connect channel)
    (http/on-close channel (partial ws! :disconnect channel))
    (http/on-receive channel #(ws! :notify %))))

;; TODO Convert responses to JSON strings to be consumed by client

(defn start-game []
  (s/new-game!)
  (str "<h1> New Game Started! </h1>"))

(defn board [action]
  (str "<div>" (b/->board (:board @s/state) action) "</div>"))

(defn move [[sx sy] [ex ey]]
  (let [-sx (keyword sx)
        -sy (keyword sy)
        -ex (keyword ex)
        -ey (keyword ey)
        res (m/move [-sx -sy] [-ex -ey] s/state)]
    (str "<div>" res "</div>")))

(cmpj/defroutes routes
  (cmpj/GET "/ws"                [req]         (ws-handler req))
  (cmpj/GET "/start"             []            (start-game))
  (cmpj/GET "/display-board"     []            (board :display))
  (cmpj/GET "/move/:sx:sy:ex:ey" [sx sy ex ey] (move [sx sy] [ex ey])))

(defn -main [& args]
  (println "Starting server on port 9000")
  (http/run-server routes {:port 9000}))
