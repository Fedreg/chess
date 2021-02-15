(ns chess-test.board
  (:require
   [chess-test.pieces :as p]))

(def blank-board
  {:8 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :7 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :6 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :5 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :4 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :3 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :2 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :1 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}})

(defn ->board
  "Board manipulations"
  [board action]
  (let [f (case action
            :display  #(or (:name %2) "")
            :colorize #(p/colorize %1 %2)
            :info     (fn [_ v] v))]
    (reduce-kv
     (fn [m k v]
       (assoc m k (reduce-kv
                   (fn [-m -k -v] (assoc -m -k (f k -v)))
                   {}
                   v)))
     {}
     board)))
