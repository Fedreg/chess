(ns chess-test.core
  (:require
   [clojure.spec.alpha     :as sp]
   [compojure.core         :as cmpj]
   [compojure.route        :as route]
   [org.httpkit.client     :as http]
   [org.httpkit.server     :as serv]
   [ring.middleware.params :as params]
   [hiccup.core            :refer :all]

   [chess-test.moves       :as m]
   [chess-test.board       :as b]
   [chess-test.views       :as v]
   [chess-test.state       :as s]))

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

(defn start-game [req]
  (s/new-game!)
  (html (v/board nil)))
  ;; #_(html [:h1 "New Game Started!"]))

(start-game nil)

(defn board [req]
  (html [:div (b/->board (:board @s/state) :display)]))

(defn move [sx sy ex ey]
  (let [-sx (keyword sx)
        -sy (keyword sy)
        -ex (keyword ex)
        -ey (keyword ey)
        res (m/move [-sx -sy] [-ex -ey] s/state)]
    (html [:div res])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def router
  (params/wrap-params
   (cmpj/routes
    (cmpj/GET "/start"         []            start-game)
    (cmpj/GET "/display-board" []            board)
    (cmpj/GET "/move"          [sx sy ex ey] (move sx sy ex ey))
    (route/not-found           not-found-page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -main [& args]
  (println "Starting server on port 9000")
  (serv/run-server router {:port 9000}))

(comment

  (-main)

  (move "2" "a" "3" "a")

  @(http/get "http://localhost:9000/start")
  @(http/get "http://localhost:9000/display-board")
  @(http/get "http://localhost:9000/move?sx=2&sy=a&ex=4&ey=a")

  :end)
