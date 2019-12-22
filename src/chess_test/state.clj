(ns chess-test.state
  (:require [chess-test.board :as b]
            [chess-test.pieces :as p]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def init
  {:board          b/blank-board
   :round          1
   :kills          {:white []
                    :black []}
   :points         {:white 0
                    :black 0}
   :king-pos       {:white [:1 :e] :black [:8 :e]}
   :check          nil
   :possible-moves []
   :current-move   []
   :illegal-moves  []
   :history        {}})

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
  (let [col (if (= :white (:color piece)) :black :white)]
    (swap! state #(-> %
                      (update-in [:kills  col] conj (:name piece))
                      (update-in [:points col] + (:value piece))))))

(defn clear-illegal-moves
  ([]
   (clear-illegal-moves (:illegal-moves @state)))
  ([illegal-moves]
   (mapv
    (fn [[x y]]
      (swap! state #(-> %
                        (assoc-in [:board x y :illegal?] false)
                        (assoc-in [:illegal-moves] (filterv
                                                    (fn [im]
                                                      (not= im [x y]))
                                                    (:illegal-moves @state))))))
    illegal-moves)))

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
        (mapv (fn [[r f]] (swap! state update-in [:board r f]
                                 (fn [p] (if (:name p)
                                           (dissoc p :possible?)
                                           ""))))
              moves)))))

(defn move-res []
  {:round  (:round   @state)
   :points (:points  @state)
   :kills  (:kills   @state)
   :board  (b/->board (:board @state) :display)})

(defn update-move-end!
  "Update the state after moving a piece"
  [[sx sy] [ex ey]]
  (let [s-piece (get-in @state [:board sx sy])
        e-piece (get-in @state [:board ex ey])]
  (when (and (not= "" e-piece)
             (not= (:color s-piece)
                   (:color e-piece)))
    (update-kill! (get-in (:board @state) [ex ey])))
  (update-possible-moves sx sy :delete)
  (swap! state #(-> %
                    (assoc-in [:board ex ey] (assoc s-piece :clicked? false))
                    (assoc-in [:board sx sy] "")
                    (assoc-in [:history (-> (:round @state) str keyword)] @state)
                    (assoc-in [:current-move] [])
                    (assoc-in [:possible-moves] [])
                    (update-in [:round] inc)))
  (merge (move-res)
         {:move {:piece (:name s-piece) :from [sx sy] :to [ex ey]}})))

(defn update-move-start!
  "Update the state after clicking the piece to move.  Shows possible moves. Move not complete until end location clicked (..which calls update-move!)"
  [[x y] possible-moves]
  (swap! state #(-> %
                    (update-in [:current-move] conj x y)
                    (assoc-in  [:possible-moves] possible-moves)
                    (assoc-in  [:board x y :clicked?] true)))
  (clear-illegal-moves)
  (update-possible-moves x y :add)
  (move-res))

(defn update-noop!
  "resets the move state if the move-start and move-end are the same piece"
  [[x y]]
  (update-possible-moves x y :delete)
  (when (get-in @state [:board x y :clicked?])
    (swap! state #(assoc-in % [:board x y :clicked?] false)))
  (when-let [cur (not-empty (:current-move @state))]
    (swap! state #(assoc-in % [:board
                               (first cur)
                               (last cur)
                               :clicked?]
                            false)))
  (swap! state #(-> %
                 (assoc-in [:current-move]   [])
                 (assoc-in [:possible-moves] [])))
  (move-res))

(defn update-illegal-move!
  "When a move is illegal the piece will be marked red"
  [[x y]]
  (if (not-empty (filter #(= [x y] %) (:illegal-moves @state)))
    (clear-illegal-moves [[x y]])
    (swap! state #(-> %
                      (update-in [:board x y]
                                (fn [p]
                                  (if (map? p)
                                           (assoc p :illegal? true)
                                           {:illegal? true})))
                      (update-in [:illegal-moves] conj [x y])))))

(defn update-undo!
  "Moves the board back a round"
  []
  (let [cur-rnd    (:round @state)
        past-state (get-in @state [:history (-> cur-rnd dec str keyword)])]
    (when past-state
      (reset! state (assoc past-state :history (:history @state))))
    (move-res)))

(defn update-redo!
  "Moves the board forward a round"
  []
  (let [cur-rnd    (:round @state)
        next-state (get-in @state [:history (-> cur-rnd inc str keyword)])]
    (when next-state
      (reset! state (assoc next-state :history (:history @state))))
    (move-res)))

(defn update!
  "Global update fn"
  [{:keys [action start end possible-moves]}]
  (prn action start end possible-moves)
  (case action
    :illegal    (update-illegal-move! start)
    :noop       (update-noop!       end)
    :move-start (update-move-start! start possible-moves)
    :move-end   (update-move-end!   start end)
    :undo       (update-undo!)
    :redo       (update-redo!)))

(comment
  (new-game!)
  (update-move! [:1 :c] [:3 :b] p/king)
  (update-move-start! :2 :a [[:3 :a] [:4 :a]])
  (update-noop! :3 :f)


  (update-kill! (assoc p/rook :color :white))
  @state

  (update-in {:dog 0} [:dog] + 6)

  ;; TODO
  ;; Need to move move-res logic into state
  ;; Separate check? fn that can always check if any king is in check after each move
  ;; For checki...
  ;; WHen a piece gets in check we should add a :check key to the state with the checker and the king in check
  ;; Then after each move we should check the 'check-res' from the checker to the checkee to see if he's still in check.
  ;; Moves should be illegal if the check is not broken
  
  :end-comment)
