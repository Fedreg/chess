(ns chess-test.core
  (:require
   [clojure.spec.alpha     :as sp]
   [clojure.data.json      :as json]
   [compojure.core         :as cmpj]
   [compojure.route        :as route]
   [org.httpkit.client     :as http]
   [org.httpkit.server     :as serv]
   [ring.middleware.params :as params]
   [ring.middleware.cors   :as cors]
   [cognitect.transit      :as ts]
   [hiccup.core            :refer :all]
   [hiccup.page            :as page]

   [chess-test.moves       :as m]
   [chess-test.board       :as b]
   [chess-test.views       :as v]
   [chess-test.state       :as s])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Responses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def not-found-page
  {:status  404
   :headers {"Content-Type" "application/json; charset=utf-8"}
   :body    (html [:h3 "the page you navigated to does not exist! You'vek made a mistake"])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn serialize [data]
  (let [out    (ByteArrayOutputStream. 4096)
        writer (ts/writer out :json)]
    (ts/write writer data)
    (.toString out)))

(defn start-game [req]
  (println "STARTING NEW GAME")
  (s/new-game!)
  (serialize
   (:board @s/state)))

(defn board [req]
  (html [:div (b/->board (:board @s/state) :display)]))

(defn move [xy]
  (println "MOVES!" xy)
  (let [sx  (keyword (subs xy 0 1))
        ex  (keyword (subs xy 2 3))
        sy  (keyword (subs xy 1 2))
        ey  (keyword (subs xy 3 4))
        res (m/move [sx sy] [ex ey] s/state)]
    (println "MOVES:" sx sy ex ey)
    (if (not= :illegal res)
      (serialize (:board @s/state))
      (serialize :illegal))))

(m/move [:2 :a] [ :4 :a] s/state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def router
  (cors/wrap-cors
   (params/wrap-params
   (cmpj/routes
    (cmpj/GET "/start"               []   start-game)
    (cmpj/GET "/display-board"       []   board)
    (cmpj/GET "/move"                [xy] (move xy))
    (cmpj/ANY "/js/chess-scripts.js" []   (slurp "resources/public/js/chess-scripts.js"))
    (route/not-found                 not-found-page)))
   :access-control-allow-origin [#".*"]
   :access-control-allow-methods [:get :post]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (println "Starting server on port 9000")
  (serv/run-server router {:port 9000}))

(comment

  (-main)

  (move "2a3a")

  @(http/get "http://localhost:9000/start")

  @(http/get "http://localhost:9000/display-board")
  @(http/get "http://localhost:9000/move?xy=2a4a")

  :end)
