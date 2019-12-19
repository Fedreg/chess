(ns chess-test.moves
  (:require
   [chess-test.state  :as s]
   [chess-test.board  :as b]
   [chess-test.pieces :as p]))

;; In chess, "file" refers to vertical position; "rank" refers to horizontal
(def rank [:1 :2 :3 :4 :5 :6 :7 :8])
(def file [:a :b :c :d :e :f :g :h])

(defn -diff [a b]
  (if (> a b) (- a b) (- b a)))

(defn ior
  "indexOf rank"
  [x]
  (.indexOf rank x))

(defn ior-diff
  "indexOf rank diff"
  [sx ex]
  (-diff (ior sx) (ior ex)))

(defn iof
  "indexOf file"
  [y]
  (.indexOf file y))

(defn iof-diff
  "indexOf file diff"
  [sy ey]
  (-diff (iof sy) (iof ey)))

(defn pawn-attack?
  "Unlike other pieces, pawns attack differently from their move; this caluclates their attack"
  [[sx sy] [ex ey] board]
  (and (= 1 (ior-diff sx ex))
       (= 1 (iof-diff sy ey))
       (not= "" (get-in board [ex ey]))))

(defn straight?
  "Is move a stright move? .i.e. rook"
  [[sx sy] [ex ey]]
  (or (and (= sx ex)
           (not= sy ey))
      (and (not= sx ex)
           (= sy ey))))

(defn pawn?
  "Is move a stright pawn move?"
  [[sx sy] [ex ey] board color]
  (and (not= sx ex)
         (or (pawn-attack? [sx sy] [ex ey] board)
       (= sy ey))
       (if (= :white color)
         (> (ior ex) (ior sx))
         (< (ior ex) (ior sx)))
       (cond
         (pawn-attack? [sx sy] [ex ey] board)
         true

         (and (= sx :2)
              (= :white color))
         (<= (ior-diff sx ex) 2)

         (and (not= sx :2)
              (= :white color))
         (<= (ior-diff sx ex) 1)

         (and (= sx :7)
              (= :black color))
         (<= (ior-diff sx ex) 2)

         (and (not= sx :2)
              (= :black color))
         (<= (ior-diff sx ex) 1)

         :else false)))

(defn diagonal?
  "Is move a diagonal move? .i.e. bishop"
  [[sx sy] [ex ey]]
  (and (not= sx ex)
       (not= sy ey)
       (= (ior-diff sx ex)
          (iof-diff sy ey))))

(defn el?
  "Is move an 'L' move like a knight can make?"
  [[sx sy] [ex ey]]
  (and (not= sx ex)
       (not= sy ey)
       (= 1 (-diff (ior-diff sx ex)
                   (iof-diff sy ey)))))

(defn multi? [[sx sy] [ex ey]]
  (or (straight? [sx sy] [ex ey])
      (diagonal? [sx sy] [ex ey])))

(defn loop-res
  "Final logic that determines block in each loop fn"
  [[sx sy] [ex ey] [x y] board]
  (cond
    (true? (get-in board [x y :possible?]))
    nil

    (and (not= [ex ey] [x y])
         (some? (get-in board [x y :color])))
    true

    (and (= [ex ey] [x y])
         (some? (get-in board [x y :color]))
         (not= (get-in board [sx sy :color])
               (get-in board [x y   :color])))
    nil

    (and (= [ex ey] [x y])
         (some? (get-in board [x y :color]))
         (= (get-in board [sx sy :color])
            (get-in board [x y   :color])))
    true))

(defn x-loop
  "Check for any pieces blocking horizontal moves"
  [[sx sy] [ex ey] board]
  (try
    (let [dir  (if (< (iof sy) (iof ey)) inc dec)
          comp (if (< (iof sy) (iof ey)) <= >=)]
      (loop [x sx
             y (file (dir (iof sy)))]
        (when (comp (iof y) (iof ey))
          (if (or (not= "" (get-in board [x y]))
                  (= :a y)
                  (= :h y))
            (loop-res [sx sy] [ex ey] [x y] board)
            (recur x (file (dir (iof y))))))))
    (catch Exception e
      (println "ERROR X-LOOP" [sx sy] [ex ey])
      true)))

(defn y-loop
  "Check for any pieces blocking vertical moves"
  [[sx sy] [ex ey] board]
  (try
    (let [dir  (if (< (ior sx) (ior ex)) inc dec)
          comp (if (< (ior sx) (ior ex)) <= >=)]
      (loop [y sy
             x (rank (dir (ior sx)))]
        (when (and (>= (ior x) 1)
                   (comp (ior x) (ior ex)))
          (if (or (not= "" (get-in board [x y]))
                  (= :1 x)
                  (= :8 x))
            (loop-res [sx sy] [ex ey] [x y] board)
            (recur y (rank (dir (ior x))))))))
    (catch Exception e
      (println "ERROR Y-LOOP" [sx sy] [ex ey])
      true)))

(defn x-y-loop
  "Check for any pieces blocking diagonal moves"
  [[sx sy] [ex ey] board]
  (try
    (let [x-dir  (if (< (ior sx) (ior ex)) inc dec)
          y-dir  (if (< (iof sy) (iof ey)) inc dec)
          x-comp (if (< (ior sx) (ior ex)) <= >=)
          y-comp (if (< (iof sy) (iof ey)) <= >=)]
      (loop [x (rank (x-dir (ior sx)))
             y (file (y-dir (iof sy)))]
        (when (and (>= (ior x) 1)
                   (x-comp (ior x) (ior ex))
                   (y-comp (iof y) (iof ey)))
          (if (or (not= "" (get-in board [x y]))
                  (= :a y)
                  (= :h y)
                  (= :1 x)
                  (= :8 x))
            (loop-res [sx sy] [ex ey] [x y] board)
            (recur (rank (x-dir (ior x)))
                   (file (y-dir (iof y))))))))
    (catch Exception e
      (println "ERROR X-Y-LOOP" [sx sy] [ex ey])
      true)))

(defn blocked?
  "Is any other piece blocking the movement of piece"
  [[sx sy] [ex ey] board]

  (let [piece       (get-in board [sx sy])
        e-piece     (get-in board [ex ey])
        piece-col   (:color piece)
        e-piece-col (:color e-piece)
        same-color? (and (= piece-col e-piece-col)
                         (not= true (:possible? e-piece)))
        block       (cond
                      (el? [sx sy] [ex ey])
                      (when same-color? true)

                      (= sx ex)
                      (x-loop [sx sy] [ex ey] board)

                      (= sy ey)
                      (y-loop [sx sy] [ex ey] board)

                      :else
                      (x-y-loop [sx sy] [ex ey] board))]
    block))

(defn turn?
  "is it the color's turn to move"
  [round color]
  (cond
    (and (odd?  round) (= :white color)) true
    (and (even? round) (= :black color)) true
    :else false))

(defn valid-attack?
  "Can the attacking piece legally complete the kill?"
  [[sx sy] [ex ey] piece e-piece board]
  (let [attack-mode (:attack piece)
        attack-color (:color piece)
        victim-color (:color e-piece)]
    (and (not= attack-color victim-color)
         (case attack-mode
           :pawn      (pawn-attack? [sx sy] [ex ey] board)
           :straight  (straight?    [sx sy] [ex ey])
           :diagonal  (diagonal?    [sx sy] [ex ey])
           :el        (el?          [sx sy] [ex ey])
           :multi     (or (straight? [sx sy] [ex ey])
                          (diagonal? [sx sy] [ex ey]))
           false))))

(defn valid-move? [[sx sy] [ex ey] {:keys [dir max? color board]}]
  (case dir
    :straight (and max? (straight? [sx sy] [ex ey]))
    :diagonal (and max? (diagonal? [sx sy] [ex ey]))
    :el       (and max? (el?       [sx sy] [ex ey]))
    :multi    (and max? (multi?    [sx sy] [ex ey]))
    :pawn     (and max? (pawn?     [sx sy] [ex ey] board color))
    false))

(defn possible-moves
  "Determines all the possible moves for a given piece"
  [[x y] state]
  (let [piece (get-in @state [:board x y])
        max   (:max piece)
        arr   []]
    (apply concat
           (filter identity
                   (for [ex [:1 :2 :3 :4 :5 :6 :7 :8]
                         ey [:a :b :c :d :e :f :g :h]]
                     (when (and (valid-move? [x  y]
                                             [ex ey]
                                             {:dir   (:direction piece)
                                              :color (:color piece)
                                              :board (:board @state)
                                              :max?  (and max
                                                          (>= max (ior-diff x ex))
                                                          (>= max (iof-diff y ey)))})
                                (let [e-piece (get-in (:board @state) [ex ey])]
                                  (and (or (= "" e-piece)
                                           (:possible? e-piece)
                                           (pawn-attack?  [x y] [ex ey] (:board @state))
                                           (valid-attack? [x y] [ex ey] piece e-piece (:board @state)))
                                       (not (blocked?     [x y] [ex ey] (:board @state))))))
                       (conj arr [ex ey])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Move Func
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn move-res
  "Calculate the result of a move before updating the state"
  ([[x y] state]
   (let [current (:current-move @state)
         end?    (not-empty current)
         sx      (if end? (first current) x)
         sy      (if end? (last  current) y)
         ex      (when end? x)
         ey      (when end? y)]
     (move-res [sx sy] [ex ey] state)))
  ([[sx sy] [ex ey] state]
   ;; (println "MOVE REQ" [sx sy] "=>" [ex ey])
   (cond
     (= "" (get-in @state [:board sx sy :color]))
     :illegal

     (not (turn? (:round @state) (get-in @state [:board sx sy :color])))
     :illegal

     (or (nil? ex) (nil? ey))
     :move-start

     (and (= sx ex) (= sy ey))
     :noop

     :else
     (let [board   (:board @state)
           piece   (get-in board [sx sy])
           color   (:color     piece)
           e-piece (get-in board [ex ey])
           dir     (:direction piece)
           max     (:max       piece)
           color   (:color     piece)
           max?    (and max
                        (>= max (ior-diff sx ex))
                        (>= max (iof-diff sy ey)))
           valid?  (valid-move?  [sx sy] [ex ey] {:dir dir :max? max? :color color})
           p-kill? (pawn-attack? [sx sy] [ex ey] board)
           block?  (blocked? [sx sy] [ex ey] board)
           free?   (and (or (= "" e-piece)
                            (:possible? e-piece)
                            (if (= :white (:color piece))
                              (= :black (:color e-piece))
                              (= :white (:color e-piece))))
                        (not block?))]
       ;; (println {:free free? :p-kill p-kill? :valid valid? :block block?})
       (if (or (and valid? free?) p-kill?)
         :move-end
         :noop)))))

  (defn move
  "Determines if a move is legal. sx, sy = start x, y;  ex, ey = end x, y"
  ([[x y] state]
   (let [current (:current-move @state)
         end?    (not-empty current)
         sx      (if end? (first current) x)
         sy      (if end? (last  current) y)
         ex      (when end? x)
         ey      (when end? y)]
     (move [sx sy] [ex ey] state)))
    ([[sx sy] [ex ey] state]
     (let [res (move-res [sx sy] [ex ey] state)]
       (s/update! (merge
                   {:action res
                    :start  [sx sy]
                    :end    [ex ey]}
                   (when (= :move-start res)
                    {:possible-moves (possible-moves [sx sy] state)}))))))

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
  (blocked?  [:2 :e] [:4 :e] (:board @s/state))

  ;; All you need is this to move the pawns
  (move      [:2 :d] [:4 :d] s/state)
  (move      [:8 :b] [:6 :c] s/state)
  (move      [:2 :d] s/state)

  (:board @s/state)
  (:possible-moves @s/state)

  :end-comment)
