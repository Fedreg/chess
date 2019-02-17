(ns chess-test.views
  (:require
   [hiccup.core :refer :all]
   [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn style [m]
  {:style (.trim (reduce-kv (fn [m k v] (str m (name k) ":" v "; "))
             ""
             m))})

(defn square-style [n]
  (style
   {:width  "60px"
    :height "60px"
    :border "1px solid #aaa"
    :background-color (if (odd? n) "fff" "bbb")}))

(def square-row-style
  (style
   {:display "flex"
    :flex-direction "column"}))

(def board-style
  (style
   {:display "flex"
    :flex-direction "row"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn square [n]
  [:div (square-style n)])

(defn square-row [n]
  [:div square-row-style
   (square (inc n))
   (square n)
   (square (inc n))
   (square n)
   (square (inc n))
   (square n)
   (square (inc n))
   (square n)
   (square (inc n))
   (square n)])

(defn board [data]
  [:div board-style
   (square-row 1)
   (square-row 2)
   (square-row 1)
   (square-row 2)
   (square-row 1)
   (square-row 2)
   (square-row 1)
   (square-row 2)
   (square-row 1)
   (square-row 2)])

(comment
  (html (square-row nil))
  (html (board nil))

  :end-comment)

