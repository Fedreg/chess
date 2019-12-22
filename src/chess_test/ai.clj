(ns chess-test.ai
  (:require
   [chess-test.state :as s]
   [chess-test.board :as b]
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

(defn best-start [color]
  ;; TODO Clean up!!
  "Best piece to move if there is a possible kill. Random if not."
  (let [pieces   (get-pieces color)
        options  (reduce (fn [acc p]
                           (conj acc {:start p :pos (m/possible-moves p (:board @s/state))}))
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
  [& [try]]
  (let [try (or try 0)]
  (when (<= try 25)
    (let [rnd   (:round @s/state)
          p     (best-start (if (odd? rnd) :white :black))
          start [(first p) (second p)]
          pos   (m/possible-moves start (:board @s/state))]
      (if (not-empty pos)
        (let [end (most-favorable-move pos)]
          (if (#{:noop :illegal} (m/move-res start end s/state))
            (random-move (inc try))
            (m/move start end s/state)))
        (random-move (inc try)))))))

(defn auto-match
  "Make the computer play itself!"
  [& [sleep]]
  (let [w-pieces (get-pieces :white)
        b-pieces (get-pieces :black)]
  (if-not (or (= 0 (count w-pieces))
              (= 0 (count b-pieces))
              (and (= 1 (count w-pieces))
                   (= :p (-> w-pieces first last)))
              (and (= 1 (count b-pieces))
                   (= :p (-> b-pieces first last))))
    (do
      (random-move)
      (println "ROUND" (:round @s/state) "SCORE" (:points @s/state))
      (when sleep (Thread/sleep sleep))
      (auto-match sleep))
    (prn "GAME OVER" (:kills @s/state) (:points @s/state)))))

(comment
  (s/new-game!)
  (m/move [:2 :a] [:4 :a] s/state)
  (get-pieces :black)
  (random-move)
  (auto-match)
  (best-start)

  :end)
