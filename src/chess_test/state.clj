(ns chess-test.state
  (:require [chess-test.board :as b]
            [chess-test.pieces :as p]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def init
  {:board        b/blank-board
   :round        1
   :kills        {:white []
                  :black []}
   :points       {:white 0
                  :black 0}
   :current-move []
   :history      {}})

(def state (atom init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-game! []
  (reset! state init)
  (let [board (-> @state
                  :board
                  (assoc :1 {:a p/rook,
                             :b p/knight,
                             :c p/bishop,
                             :d p/queen,
                             :e p/king,
                             :f p/bishop,
                             :g p/knight,
                             :h p/rook})
                  (assoc :2 {:a p/pawn,
                             :b p/pawn,
                             :c p/pawn,
                             :d p/pawn,
                             :e p/pawn,
                             :f p/pawn,
                             :g p/pawn,
                             :h p/pawn})
                  (assoc :7 {:a p/pawn,
                             :b p/pawn,
                             :c p/pawn,
                             :d p/pawn,
                             :e p/pawn,
                             :f p/pawn,
                             :g p/pawn,
                             :h p/pawn})
                  (assoc :8 {:a p/rook,
                             :b p/knight,
                             :c p/bishop,
                             :d p/queen,
                             :e p/king,
                             :f p/bishop,
                             :g p/knight,
                             :h p/rook})
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
  (println "KILL!")
  (let [col (if (= :white (:color piece)) :black :white)]
    (swap! state #(-> %
                      (update-in [:kills  col] conj (:name piece))
                      (update-in [:points col] + (:value piece))))))

(defn update-possible-moves
  "Adds or removes the :possible? tag from the board"
  [x y action]
  (let [color (get-in @state [:board x y :color])
        moves (:possible-moves @state)]
    (when (not-empty moves)
      (if (= :add action)
        (mapv (fn [[r f]] (swap! state update-in [:board r f]
                                 (fn [p] (if (map? p)
                                           (assoc p :possible? true)
                                           {:possible? true :color color}))))
              moves)
        (mapv (fn [[r f]] (swap! state assoc-in [:board r f] "")) moves)))))

(defn move-res []
  {:round  (:round   @state)
   :points (:points  @state)
   :kills  (:kills   @state)
   :board  (b/->board (:board @state) :display)})

(defn update-move!
  "Update the state after moving a piece"
  [[sx sy] [ex ey] piece]
  (when (and (not= "" (get-in (:board @state) [ex ey]))
             (not= (:color (get-in (:board @state) [sx sy]))
                   (:color (get-in (:board @state) [ex ey]))))
    (update-kill! (get-in (:board @state) [ex ey])))
  (update-possible-moves sx sy :delete)
  (swap! state #(-> %
                    (assoc-in [:board ex ey] (assoc piece :clicked? false))
                    (assoc-in [:board sx sy] "")
                    (assoc-in [:history (-> (:round @state) str keyword)] (:board @state))
                    (assoc-in [:current-move] [])
                    (assoc-in [:possible-moves] [])
                    (update-in [:round] inc)))
  (merge (move-res)
         {:move {:piece (:name piece) :from [sx sy] :to [ex ey]}}))

(defn update-move-start!
  "Update the state after clicking the piece to move.  Shows possible moves. Move not complete until end location clicked (..which calls update-move!)"
  [x y possible-moves]
  (swap! state #(-> %
                 (update-in [:current-move] conj [x y])
                 (assoc-in  [:possible-moves] possible-moves)
                 (assoc-in  [:board x y :clicked?] true)))
  (update-possible-moves x y :add)
  (move-res))

(defn update-noop!
  "resets the move state if the move-start and move-end are the same piece"
  [x y]
  (prn "NO OP!" x y)
  (update-possible-moves x y :delete)
  (swap! state #(-> %
                 (assoc-in [:current-move] [])
                 (assoc-in [:possible-moves] [])
                 #_(assoc-in [:board x y :clicked?] false)))
  (when (get-in @state [:board x y :clicked?])
    (swap! state #(assoc-in % [:board x y :clicked?] false)))
  (move-res))

(comment
  (new-game!)
  (update-move! [:1 :c] [:3 :b] p/king)
  (update-move-start! :2 :a [[:3 :a] [:4 :a]])
  (update-noop! :3 :f)


  (update-kill! (assoc p/rook :color :white))
  @state

  (update-in {:dog 0} [:dog] + 6)

  :end-comment)
