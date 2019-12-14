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

(defn move2 [xy]
  (let [row [:a :b :c :d :e :f :g :h]
        sx  (keyword (subs xy 0 1))
        ex  (keyword (subs xy 2 3))
        sy  (->> (subs xy 1 2) Integer/parseInt (nth row) keyword)
        ey  (->> (subs xy 3 4) Integer/parseInt (nth row) keyword)
        res (m/move [sx sy] [ex ey] s/state)]
    (if (not= :illegal res)
      (-> @s/state v/page page/html5)
      "illegal")))

(defn move [xy]
  (let [row [:a :b :c :d :e :f :g :h]
        x  (keyword (subs xy 0 1))
        y  (->> (subs xy 1 2) Integer/parseInt (nth row) keyword)
        res (m/move [x y] s/state)]
    (if (not= :illegal res)
      (-> @s/state v/page page/html5)
      "illegal")))

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
    (cmpj/GET "/move2"               [xy] (move2 xy))
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

  (m/move [:2 :a] [ :4 :a] s/state)
  (s/new-game!)
  (-main)

  (move2 "22")

  @(http/get "http://localhost:9000/start")

  @(http/get "http://localhost:9000/display-board")
  @(http/get "http://localhost:9000/move?xy=4355")
  @(http/get "http://localhost:9000/move2?xy=43")

  :end)

;; TODO
;; Possible-moves does not include kills options (including diag kills for pawns)
;; Clean up JS if condtions and url
;; Consolidate move & move-start
;; Factor out updates in update-fns
;; Pass state around consistently.  Either pass it always, never pass it (i.e. subscriptions)
