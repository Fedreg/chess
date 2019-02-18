(ns chess-test.state
  (:require [chess-test.board :as b]
            [chess-test.pieces :as p]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def init
  {:board   b/blank-board
   :round   1
   :kills   {:white []
             :black []}
   :points  {:white 0
             :black 0}
   :history {}})

(def state (atom init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-game! []
  (reset! state init)
  (let [board (-> @state
                  :board
                  (assoc :1 {:a p/rook :b p/bishop :c p/knight :d p/queen :e p/king :f p/knight :g p/bishop :h p/rook})
                  (assoc :2 {:a p/pawn :b p/pawn :c p/pawn :d p/pawn :e p/pawn :f p/pawn :g p/pawn :h p/pawn })
                  (assoc :7 {:a p/pawn :b p/pawn :c p/pawn :d p/pawn :e p/pawn :f p/pawn :g p/pawn :h p/pawn })
                  (assoc :8 {:a p/rook :b p/bishop :c p/knight :d p/queen :e p/king :f p/knight :g p/bishop :h p/rook})
                  (b/->board :colorize))]
    (swap! state assoc-in [:board] board))
  #_{:round (:round @state)
   :board (b/->board (:board @state) :display)})

(defn update-piece!
  "Update the state after moving a piece (non kill)"
  [[sx sy] [ex ey] piece]
  (swap! state #(-> %
                    (assoc-in [:board ex ey] piece)
                    (assoc-in [:board sx sy] ""))))

(defn update-kill!
  "If a piece is taken on a move, add points and kill"
  [piece]
  (let [col (if (= :white (:color piece)) :black :white)]
    (swap! state #(-> %
                      (update-in [:kills  col] conj (:name piece))
                      (update-in [:points col] + (:value piece))))))

(defn update-move!
  "Update the state after moving a piece"
  [[sx sy] [ex ey] piece]
  (when (not= ""  (get-in (:board @state) [ex ey]))
    (update-kill! (get-in (:board @state) [ex ey])))
  (swap! state #(-> %
                    (assoc-in [:board ex ey] piece)
                    (assoc-in [:board sx sy] "")
                    (assoc-in [:history (-> (:round @state) str keyword)] (:board @state))
                    (update-in [:round] inc)))
  {:move {:piece (:name piece) :from [sx sy] :to [ex ey]}
   :round   (:round   @state)
   :points  (:points  @state)
   :kills   (:kills   @state)
   :board   (b/->board (:board @state) :display)})


(comment
  (new-game!)

  (update-move! [:1 :c] [:3 :b] p/king)

  (update-kill! (assoc p/rook :color :white))

  (update-in {:dog 0} [:dog] + 6)

  :end-comment)
