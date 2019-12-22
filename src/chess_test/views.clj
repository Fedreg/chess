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
                   (square row idx v))
               (get-in data [(-> row str keyword)]))
        final (reduce (fn [acc v] (conj acc v))
                      [:div square-row-style]
                      base)]
    final))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square-style [row n {:keys [color name clicked? possible? illegal? check?]}]
  (style
   {:box-sizing       "border-box"
    :color            (cond
                        clicked?         "#fff"
                        check?           "red"
                        (= :black color) "#0c60f0"
                        :else            "#000")
    :width            "60px"
    :height           "60px"
    :border           "1px solid #0c60f0"
    :margin           "6px 6px 0 0"
    :padding          "20px 25px"
    :background-color (cond
                        clicked?                    "deeppink"
                        possible?                   "deeppink"
                        check?                      "purple"
                        illegal?                    "red"
                        (and (odd?  row) (odd?  n)) "#eee" ;"#eee"
                        (and (even? row) (even? n)) "#eee"
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
   {:color "#ddd"
    :padding "10px 29px"}))

(def rank-row-style
  (style
   {:color "#fff"
    :padding "22px 10px"}))

(def page-style
  (style
   {:height           "100%"
    :background-color "#fff"}))

(def body-style
  (style
   {:background-color "#fff"
    :margin           "150px 25%"}))

(defn turn-style []
  (style
   {:color "#777"
    :font-size "20px"}))

(def button-style
  (style
   {:color "#000"
    :cursor "pointer"}))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn undo []
  [:button {:onclick "undo();"} "UNDO"])

(defn redo []
  [:button {:onclick "redo();"} "REDO"])

(defn square
  "View for each square of the board"
  [row n piece]
  (let [id  (str "div#" row n)
        div (keyword id)]
  [div
   (assoc
    (square-style row n piece)
    :onclick
    "selectSquare(this);")
   (:name piece)]))

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

(defn undo []
  [:button {:onclick "undo();"} "UNDO"])

(defn redo []
  [:button {:onclick "redo();"} "REDO"])

(defn auto-match []
  [:button {:onclick "autoMatch();"} "AUTO MATCH"])

(defn turn
  "Displays whose turn it is"
  [round]
  [:div (turn-style) [:div "ROUND " round] [:div (if (odd? round) :white :black)]])

(defn check 
  "Displays whose turn it is"
  [check]
  [:div (turn-style) [:div "CHEK" check]])

(defn points
  "Displays points per side"
  [points]
  [:div (turn-style)
   [:div "POINTS: "
    [:span "WHITE: " (:white points)]
    [:span ", BLACK: " (:black points)]]])

(defn page
  "Main page view"
  [data]
  (page/html5
   [:head
    (page/include-js "js/chess-scripts.js")]
   [:body body-style
    [:div#body page-style
     (board  (:board  data))
     (points (:points data))
     (turn   (:round  data))
     (check  (:check  data))
     (undo)
     (redo)
     (auto-match)]]))
