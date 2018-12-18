(ns chess-test.pieces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pieces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pawn   {:max 2 :direction :straight :attack :diagonal :name :p})
(def bishop {:max 8 :direction :diagonal :attack :diagonal :name :b})
(def knight {:max 2 :direction :el       :attack :el       :name :k})
(def rook   {:max 8 :direction :straight :attack :straight :name :r})
(def queen  {:max 8 :direction :any      :attack :multi    :name :Q})
(def king   {:max 1 :direction :any      :attack :multi    :name :K})

(defn name->piece [name]
  (case name
    :p pawn
    :b bishop
    :k knight
    :r rook
    :Q queen
    :K king
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In chess, "file" refers to vertical position; "rank" refers to horizontal
(def rank [:1 :2 :3 :4 :5 :6 :7 :8])
(def file [:a :b :c :d :e :f :g :h])

(def blank-board
  {:8 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :7 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :6 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :5 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :4 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :3 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :2 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :1 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}})

(def board (atom blank-board))

(defn new-game []
  (reset! board blank-board)
  (swap! board assoc-in [:1] {:a :r :b :b :c :k :d :Q :e :K :f :k :g :b :h :r})
  (swap! board assoc-in [:2] {:a :p :b :p :c :p :d :p :e :p :f :p :g :p :h :p})
  (swap! board assoc-in [:7] {:a :p :b :p :c :p :d :p :e :p :f :p :g :p :h :p})
  (swap! board assoc-in [:8] {:a :r :b :b :c :k :d :Q :e :K :f :k :g :b :h :r}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  [[sx sy] [ex ey]]
  (let [x-loop #(loop [x %
                       y (file (inc (.indexOf file sy)))]
                  (when (<= (.indexOf file y) (.indexOf file ey))
                    (if (not= "" (get-in @board [x y]))
                      :block
                      (recur x (file (inc (.indexOf file sy)))))))
        y-loop #(loop [y %
                       x (rank (inc (.indexOf rank sx)))]
                  (when (<= (.indexOf rank x) (.indexOf rank ex))
                    (if (not= "" (get-in @board [x y]))
                      :block
                      (recur y (rank (inc (.indexOf rank x)))))))
        x-y-loop #(loop [x (rank (inc (.indexOf rank %1)))
                         y (file (inc (.indexOf file %2)))]
                    (when (and (<= (.indexOf rank x) (.indexOf rank ex))
                               (<= (.indexOf file y) (.indexOf file ey)))
                      (if (not= "" (get-in @board [x y]))
                        :block
                        (recur (rank (inc (.indexOf rank x)))
                               (file (inc (.indexOf file y)))))))
        block (cond
                (= sx ex) (x-loop sx)
                (= sy ey) (y-loop sy)
                :else     (x-y-loop sx sy))]
    block))

;; sx, sy = start x, y;  ex, ey = end x, y
(defn move [[sx sy] [ex ey]]
  (let [piece   (name->piece (get-in @board [sx sy]))
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
        free?   (and (not= :k (:name piece))
                     (not (blocked? [sx sy] [ex ey])))]
    (if (and valid? free?)
      (do
        (swap! board assoc-in [ex ey] (:name piece))
        (swap! board assoc-in [sx sy] "")
        {:piece (:name piece) :from [sx sy] :to [ex ey]})
      :illegal)))


(comment

  (new-game)

  @board

  (straight? [:1 :a] [:1 :c])
  (diagonal? [:3 :a] [:1 :c])
  (el?       [:2 :c] [:1 :a])
  (multi?    [:2 :c] [:7 :b])
  (blocked?  [:2 :a] [:3 :a])

  (move      [:1 :a] [:2 :a])

  :end-comment)
(defn salty [n]
  (loop [x n]
    (when (< 0 x)
      (println "x" x)
      (recur (dec x)))))

