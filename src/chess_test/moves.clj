(ns chess-test.moves
  (:require
   [chess-test.state :as s]
   [chess-test.board :as b]
   [chess-test.pieces :as p]))

;; In chess, "file" refers to vertical position; "rank" refers to horizontal
(def rank [:1 :2 :3 :4 :5 :6 :7 :8])
(def file [:a :b :c :d :e :f :g :h])

(defn diff [a b]
  (if (> a b) (- a b) (- b a)))

(defn straight?
  "Is move a stright move? .i.e. rook"
  [[sx sy] [ex ey]]
  (or (and (= sx ex)
           (not= sy ey))
      (and (not= sx ex)
           (= sy ey))))

(defn diagonal?
  "Is move a diagonal move? .i.e. bishop"
  [[sx sy] [ex ey]]
  (and (not= sx ex)
       (not= sy ey)
       (= (diff (.indexOf rank sx) (.indexOf rank ex))
          (diff (.indexOf file sy) (.indexOf file ey)))))

(defn el?
  "Is move an 'L' move like a knight can make?"
  [[sx sy] [ex ey]]
  (and (not= sx ex)
       (not= sy ey)
       (= 1 (diff (diff (.indexOf rank sx) (.indexOf rank ex))
                  (diff (.indexOf file sy) (.indexOf file ey))))))

(defn multi? [[sx sy] [ex ey]]
  (or (straight? [sx sy] [ex ey])
      (diagonal? [sx sy] [ex ey])))

(defn blocked?
  "Is any other piece blocking the movement of piece"
  ;; TODO This works for ascending moves, make sure you're always calculating as such
  ;; TODO Clean this up!!!  Terrible code
  [[sx sy] [ex ey] board]
  (let [x-loop #(loop [x %
                       y (file (inc (.indexOf file sy)))]
                  (when (<= (.indexOf file y) (.indexOf file ey))
                    (if (not= "" (get-in board [x y]))
                      :block
                      (recur x (file (inc (.indexOf file sy)))))))
        y-loop #(loop [y %
                       x (rank (inc (.indexOf rank sx)))]
                  (when (<= (.indexOf rank x) (.indexOf rank ex))
                    (if (not= "" (get-in board [x y]))
                      :block
                      (recur y (rank (inc (.indexOf rank x)))))))
        x-y-loop #(loop [x (rank (inc (.indexOf rank %1)))
                         y (file (inc (.indexOf file %2)))]
                    (when (and (<= (.indexOf rank x) (.indexOf rank ex))
                               (<= (.indexOf file y) (.indexOf file ey)))
                      (if (not= "" (get-in board [x y]))
                        :block
                        (recur (rank (inc (.indexOf rank x)))
                               (file (inc (.indexOf file y)))))))
        block (cond
                (el? [sx sy] [ex ey]) nil
                (= sx ex)             (x-loop sx)
                (= sy ey)             (y-loop sy)
                :else                 (x-y-loop sx sy))]
    block))

(defn turn?
  "is it the color's turn to move"
  [round piece]
  (cond
    (and (odd?  round) (= :white (:color piece))) true
    (and (even? round) (= :black (:color piece))) true
    :else false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Move Func
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sx, sy = start x, y;  ex, ey = end x, y
(defn move [[sx sy] [ex ey] state]
  (let [board   (:board @state)
        piece   (get-in board [sx sy])
        e-piece (get-in board [ex ey])
        dir     (:direction piece)
        max     (:max       piece)
        max?    (and max
                     (>= max (diff (.indexOf rank sx) (.indexOf rank ex)))
                     (>= max (diff (.indexOf file sy) (.indexOf file ey))))
        valid?  (case dir
                  :straight (and max? (straight? [sx sy] [ex ey]))
                  :diagonal (and max? (diagonal? [sx sy] [ex ey]))
                  :el       (and max? (el?       [sx sy] [ex ey]))
                  :multi    (and max? (multi?    [sx sy] [ex ey]))
                  false)
        free?   (and (or (= "" e-piece)
                         (if (= :white (:color piece))
                           (= :black (:color e-piece))
                           (= :white (:color e-piece))))
                     (not (blocked? [sx sy] [ex ey] board)))]
    (if (and valid? free?)
      (if (turn? (:round @state) piece)
        (s/update-move!  [sx sy] [ex ey] piece)
        :other-player)
      :illegal)))

(comment
  ;; State
  (dissoc @s/state :board :history)

  ;; Start a new game
  (s/new-game!)

  ;; the board at current state
  (b/->board (:board @s/state) :display)

  (b/->board (:board @s/state) :info)

  ;; Ignore these...
  @board
  (straight? [:1 :a] [:1 :c])
  (diagonal? [:3 :a] [:1 :c])
  (el?       [:2 :c] [:1 :a])
  (multi?    [:2 :c] [:7 :b])
  (blocked?  [:3 :e] [:5 :d] (:board @s/state))

  ;; All you need is this to move the pawns
  (move      [:3 :e] [:5 :d] s/state)
  
  (move      [:7 :a] [:6 :a] s/state)


  :end-comment)
