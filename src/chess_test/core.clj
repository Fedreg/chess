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
   [chess-test.state       :as s]
   [hiccup.page :as page]))

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
  (v/page (s/new-game!)))

(defn board [req]
  (html [:div (b/->board (:board @s/state) :display)]))

(def letters [:a :b :c :d :e :f :g :h])

(defn move [xy]
  (let [row [:a :b :c :d :e :f :g :h]
        sx  (keyword (subs xy 0 1))
        ex  (keyword (subs xy 2 3))
        sy  (->> (subs xy 1 2) Integer/parseInt (nth row) keyword)
        ey  (->> (subs xy 3 4) Integer/parseInt (nth row) keyword)
        res (m/move [sx sy] [ex ey] s/state)]
    (println "MOVES:" sx sy ex ey)
    (if (not= :illegal res)
      (-> @s/state :board v/board page/html5)
      "illegal")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def router
  (params/wrap-params
   (cmpj/routes
    (cmpj/GET "/start"               []   start-game)
    (cmpj/GET "/display-board"       []   board)
    (cmpj/GET "/move"                [xy] (move xy))
    (cmpj/ANY "/js/chess-scripts.js" []   (slurp "resources/public/js/chess-scripts.js"))
    (route/not-found                 not-found-page))))

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
