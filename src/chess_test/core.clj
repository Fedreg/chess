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

   [chess-test.ai          :as ai]
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
   :body    (html [:h3 "the page you navigated to does not exist! You've made a mistake"])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn start-game [req]
  (println "STARTING NEW GAME")
  (v/page (s/new-game!)))

(defn board [req]
  (-> @s/state v/page page/html5))

(defn undo [req]
  (s/update! {:action :undo})
  (-> @s/state v/page page/html5))

(defn redo [req]
  (s/update! {:action :redo})
  (-> @s/state v/page page/html5))

(defn random-move []
  (ai/random-move 0)
  (-> @s/state v/page page/html5))

(defn move [xy]
  (let [row [:a :b :c :d :e :f :g :h]
        x   (keyword (subs xy 0 1))
        y   (->> (subs xy 1 2) Integer/parseInt (nth row) keyword)
        res (m/move [x y] s/state)]
    (-> @s/state v/page page/html5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def router
  (cors/wrap-cors
   (params/wrap-params
   (cmpj/routes
    (cmpj/GET "/"                    []   start-game)
    (cmpj/GET "/move"                [xy] (move xy))
    (cmpj/GET "/random-move"         []   (random-move))
    (cmpj/GET "/undo"                []   undo)
    (cmpj/GET "/redo"                []   redo)
    (cmpj/GET "/board"               []   board)
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

  (m/move [:2 :a] [ :4 :a] @s/state)
  (s/new-game!)
  (-main)

  @(http/get "http://localhost:9000/start")

  @(http/get "http://localhost:9000/display-board")
  @(http/get "http://localhost:9000/move?xy=4355")
  @(http/get "http://localhost:9000/move2?xy=43")

  :end)

;; TODO
;; Factor out updates in update-fns
;; Pass state around consistently.  Either pass it always, never pass it (i.e. subscriptions)
