(ns chess-test.match-test
  (:require
   [clojure.test     :refer :all]
   [chess-test.moves :as m]
   [chess-test.state :as s]
   [chess-test.board :as b]
   [chess-test.core  :as c]))

(def board
  {:8 {:a :r :b :k :c :b :d :Q :e :K :f :b :g :k :h :r}
   :7 {:a :p :b :p :c :p :d :p :e :p :f :p :g :p :h :p}
   :6 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :5 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :4 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :3 {:a "" :b "" :c "" :d "" :e "" :f "" :g "" :h ""}
   :2 {:a :p :b :p :c :p :d :p :e :p :f :p :g :p :h :p}
   :1 {:a :r :b :k :c :b :d :Q :e :K :f :b :g :k :h :r}})

(def test-board (atom board))

(defn tb [name [sx sy] [ex ey]]
  (reset! test-board
          (-> @test-board
              (assoc-in [ex ey] name)
              (assoc-in [sx sy] ""))))

(deftest match-test
  (testing  "Simulates a short match and tests in between"
    (s/new-game!)
    (reset! test-board board)
    (is (= test-board1  (b/->board (:board @s/state) :display)))

    (m/move [:2 :g] [:4 :g] s/state)
    (is (= (tb :p [:2 :g] [:4 :g])  (b/->board (:board @s/state) :display)))
    (is (= 2 (:round @s/state)))

    (m/move [:7 :f] [:5 :f] s/state)
    (is (= (tb :p [:7 :f] [:5 :f])  (b/->board (:board @s/state) :display)))
    (is (= 3 (:round @s/state)))

    (m/move [:4 :g] [:5 :f] s/state)
    (is (= (tb :p [:4 :g] [:5 :f])  (b/->board (:board @s/state) :display)))
    (is (= 4 (:round @s/state)))
    (is (= [:p] (get-in @s/state [:kills :white])))
    ))
