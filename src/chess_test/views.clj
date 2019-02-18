(ns chess-test.views
  (:require
   [hiccup.core    :refer :all]
   [hiccup.page    :as page]
   [clojure.string :as str])
  (:import java.net.URI))

(declare square)
(declare square-row-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn style
  "Converts an attractive edn representation of style attribute to string"
  [m]
  {:style (.trim (reduce-kv (fn [m k v] (str m (name k) ":" v "; "))
             ""
             m))})

(defn get-pieces
  "gets the pieces from the state.board and inserts them into dom"
  [row data]
  (let [base  (map-indexed
               (fn [idx [k v]] (square row idx v))
               (get-in data [:board (-> row str keyword)]))
        final (reduce (fn [acc v] (conj acc v))
                      [:div square-row-style]
                      base)]
    final))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square-style [row n]
  (style
   {:box-sizing       "border-box"
    :width            "60px"
    :height           "60px"
    :border           "1px solid #aaa"
    :padding          "20px 25px"
    :background-color (cond
                        (and (odd?  row) (odd?  n)) "#fff"
                        (and (even? row) (even? n)) "#fff"
                        :else                       "#bbb")}))

(def square-row-style
  (style
   {:display        "flex"
    :flex-direction "row"}))

(def board-style
  (style
   {:display        "flex"
    :flex-direction "column"}))

(def file-row-style
  (style
   {:color "#ccc"
    :padding "10px 26px"}))

(def rank-row-style
  (style
   {:color "#ccc"
    :padding "20px 10px"}))

(def move-button-style
  (style
   {:padding "20px"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square
  "View for each square of the board"
  [row n piece]
  [:div (square-style row n) piece])

(def move-button
  "The button that triggers a move"
  [:div move-button-style
    "from:  "
    [:input#move-start {:type "text" :value "2d"}]
    "to:  "
    [:input#move-end   {:type "text" :value "4d"}]
    [:button {:onclick "sendMove();"} "move"]])

(def file-row
  "In chess, the horizontal rows are called 'file'"
  [:div
   [:span file-style "a"]
   [:span file-style "b"]
   [:span file-style "c"]
   [:span file-style "d"]
   [:span file-style "e"]
   [:span file-style "f"]
   [:span file-style "g"]
   [:span file-style "h"]])

(defn board
  "Main board view"
  [data]
  [:div board-style
   [:div square-row-style (get-pieces 8 data)
    [:span rank-row-style "8"]]
   [:div square-row-style (get-pieces 7 data)
    [:span rank-row-style "7"]]
   [:div square-row-style (get-pieces 6 data)
    [:span rank-row-style "6"]]
   [:div square-row-style (get-pieces 5 data)
    [:span rank-row-style "5"]]
   [:div square-row-style (get-pieces 4 data)
    [:span rank-row-style "4"]]
   [:div square-row-style (get-pieces 3 data)
    [:span rank-row-style "3"]]
   [:div square-row-style (get-pieces 2 data)
    [:span rank-row-style "2"]]
   [:div square-row-style (get-pieces 1 data)
    [:span rank-row-style "1"]]
   file-row])

(defn page
  "Main page view"
  [data]
  (page/html5
   [:head
    (page/include-js "js/chess-scripts.js")]
   [:body
    [:div#body
     (board data)]
    move-button]))


(comment
  (def samp-board
  {:board {:8 {:a :r, :b :b, :c :k, :d :Q, :e :K, :f :k, :g :b, :h :r},
           :7 {:a :p, :b :p, :c :p, :d :p, :e :p, :f :p, :g :p, :h :p},
           :6 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
           :5 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
           :4 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
           :3 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
           :2 {:a :p, :b :p, :c :p, :d :p, :e :p, :f :p, :g :p, :h :p},
           :1 {:a :r, :b :b, :c :k, :d :Q, :e :K, :f :k, :g :b, :h :r}}})

  (html (square-row nil))
  (board samp-board)

  :end-comment)
