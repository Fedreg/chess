(ns chess-test.pieces)

(def pawn   {:max 2 :direction :straight :attack :diagonal :name :p :value 1})
(def bishop {:max 8 :direction :diagonal :attack :diagonal :name :b :value 3})
(def knight {:max 2 :direction :el       :attack :el       :name :k :value 3})
(def rook   {:max 8 :direction :straight :attack :straight :name :r :value 5})
(def queen  {:max 8 :direction :any      :attack :multi    :name :Q :value 9})
(def king   {:max 1 :direction :any      :attack :multi    :name :K :value :mate})

(defn name->piece [name]
  (case name
    :p pawn
    :b bishop
    :k knight
    :r rook
    :Q queen
    :K king
    nil))

(defn colorize
  "Applies white or black color to piece on game start"
  [k v]
  (if (= "" v) "" (assoc v :color (if (#{:1 :2} k)
                                      :white
                                      :black))))

