(ns chess-test.ai
  (:require
   [chess-test.state :as s]
   [chess-test.moves :as m]))

(defn get-pieces
  "Gets all positions of pieces on board by color"
  [color]
  (filter
   identity
   (for [x [:1 :2 :3 :4 :5 :6 :7 :8]
         y [:a :b :c :d :e :f :g :h]]
     (let [p (get-in (:board @s/state) [x y])]
       (when (= color (:color p))
         [x y (:name p)])))))

(defn most-favorable-move
  "Looks at possible moves and tries to kill most
  valuable piece out of those possibilites"
  [possible-moves]
  (let [pieces (reduce
                (fn [acc [x y]]
                  (conj acc [x y (get-in @s/state [:board x y :value])]))
                []
                possible-moves)]
    (->> pieces
        (sort-by last)
        reverse
        first)))

(defn best-start []
  ;; TODO Clean up!!
  "Best piece to move if there is a possible kill. Random if not."
  (let [pieces   (get-pieces :black)
        options  (reduce (fn [acc p]
                           (conj acc {:start p :pos (m/possible-moves p s/state)}))
                         []
                         pieces)
        -options (filterv #(not-empty (:pos %)) options)
        mf       (reduce (fn [acc m]
                           (conj acc {:start (:start m)
                                      :mf    (most-favorable-move (:pos m))}))
                         []
                         -options)
        kills?   (->> mf
                     (mapv :mf)
                     (mapv last)
                     (remove nil?)
                     not-empty)]
    (if kills?
      (->> mf
           (sort-by #(-> % :mf last))
           reverse
           first
           :start)
      (rand-nth pieces))))

(defn random-move
  "Basic, lame AI... but it's a start!"
  [try]
  (when (<= try 25)
    (let [p     (best-start)
          start [(first p) (second p)]
          pos   (m/possible-moves start s/state)]
      (if (not-empty pos)
        (let [end (most-favorable-move pos)]
          (if (#{:noop :illegal} (m/move-res start end s/state))
            (random-move (inc try))
            (m/move start end s/state)))
        (random-move (inc try))))))

(comment
  (m/move [:2 :a] [:4 :a] s/state)
  (get-pieces :black)
  (random-move 0)
  (best-start)

  :end)
