(ns chess-test.views
  (:require
   [hiccup.core      :refer :all]
   [hiccup.page      :as page]
   [chess-test.board :as b]
   [clojure.string   :as str])
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
               (fn [idx [k v]]
                 (let [name      (:name     v)
                       clicked?  (:clicked? v)
                       possible? (:possible? v)
                       color     (:color    v)]
                   (square row idx name color clicked? possible?)))
               (get-in data [(-> row str keyword)]))
        final (reduce (fn [acc v] (conj acc v))
                      [:div square-row-style]
                      base)]
    final))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square-style [row n color name clicked? possible?]
  (style
   {:box-sizing       "border-box"
    :color            (cond 
                        clicked?         "#ddd"
                        possible?        "#fff"
                        (= :black color) "#0c60f0"
                        :else            "#ddd")
    :width            "60px"
    :height           "60px"
    :border           "1px solid #0c60f0"
    :margin           "6px 6px 0 0"
    :padding          "20px 25px"
    :background-color (cond
                        clicked?                    "#0c60f0"
                        possible?                   "#0c60f0"
                        (and (odd?  row) (odd?  n)) "#222"
                        (and (even? row) (even? n)) "#222"
                        :else                       "#333")}))

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
   {:color "#555"
    :padding "10px 29px"}))

(def rank-row-style
  (style
   {:color "#555"
    :padding "22px 10px"}))

(def page-style
  (style
   {:height           "100%"
    ;; :width            "100%"
    ;; :margin           "100px"
    :background-color "#222"}))

(def body-style
  (style
   {:background-color "#222"}))

(defn turn-style [round]
  (style
   {:color "#fff"
    :font-size "20px"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square
  "View for each square of the board"
  [row n name color clicked? possible?]
  (let [id  (str "div#" row n)
        div (keyword id)]
  [div
   (assoc
    (square-style row n color name clicked? possible?)
    :onclick
    "selectSquare(this);")
   name]))

(def file-row
  "In chess, the horizontal rows are called 'file'"
  [:div
   [:span file-row-style "a"]
   [:span file-row-style "b"]
   [:span file-row-style "c"]
   [:span file-row-style "d"]
   [:span file-row-style "e"]
   [:span file-row-style "f"]
   [:span file-row-style "g"]
   [:span file-row-style "h"]])

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

(defn turn
  "Displays whose turn it is"
  [round]
  [:div (turn-style round) [:div round] [:div (if (odd? round) :white :black)]])

(defn page
  "Main page view"
  [data]
  (page/html5
   [:head
    (page/include-js "js/chess-scripts.js")]
   [:body body-style
    [:div#body page-style
     (board (:board data))
     (turn  (:round data))]]))
