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
    (is (= @test-board  (b/->board (:board @s/state) :display)))

    ;; NOTE Simulating first and second click in all these moves to test possible-moves, etc
    ;; Thus each move has 2 parts:
    ;; 1st click  ex (m/move [:2 :d] [nil nil])
    ;; 2nd click  ex (m/move [:2 :d] [:4 :d])

    ;; White pawn start
    (m/move [:2 :d] [nil nil] s/state)
    (is (= 2 (count (:possible-moves @s/state))))
    (m/move [:2 :d] [:4 :d] s/state)
    (is (= (tb :p [:2 :d] [:4 :d])  (b/->board (:board @s/state) :display)))
    (is (= 2 (:round @s/state)))

    ;; Black pawn
    (m/move [:7 :d] [nil nil] s/state)
    (is (= 2 (count (:possible-moves @s/state))))
    (m/move [:7 :e] [:5 :e] s/state)
    (is (= (tb :p [:7 :e] [:5 :e])  (b/->board (:board @s/state) :display)))
    (is (= 3 (:round @s/state)))

    ;; White pawn kills black pawn
    (m/move [:4 :d] [nil nil] s/state)
    (is (= 2 (count (:possible-moves @s/state))))
    (m/move [:4 :d] [:5 :e] s/state)
    (is (= (tb :p [:4 :d] [:5 :e])  (b/->board (:board @s/state) :display)))
    (is (= 4 (:round @s/state)))
    (is (= [:p] (get-in @s/state [:kills :white])))
    (is (= {:white 1 :black 0} (:points @s/state)))

    ;; Next black pawn advances
    (m/move [:7 :d] [nil nil] s/state)
    (is (= 2 (count (:possible-moves @s/state))))
    (m/move [:7 :d] [:5 :d] s/state)
    (is (= (tb :p [:7 :d] [:5 :d])  (b/->board (:board @s/state) :display)))
    (is (= 5 (:round @s/state)))

    ;; White Q kills plack pawn
    (m/move [:1 :d] [nil nil] s/state)
    (is (= 4 (count (:possible-moves @s/state))))
    (m/move [:1 :d] [:5 :d] s/state)
    (is (= (tb :Q [:1 :d] [:5 :d])  (b/->board (:board @s/state) :display)))
    (is (= 6 (:round @s/state)))
    (is (= [:p :p] (get-in @s/state [:kills :white])))
    (is (= {:white 2 :black 0} (:points @s/state)))

    ;; Black Q clicks to move then changes mind (noop)
    (m/move [:8 :d] [nil nil] s/state)
    (is (= 7 (count (:possible-moves @s/state))))
    ;; Unchanged since last time
    (is (= (tb :Q [:1 :d] [:5 :d])  (b/->board (:board @s/state) :display)))
    (m/move [:8 :d] [:8 :d] s/state) ; => noop
    (is (= (tb :Q [:1 :d] [:5 :d])  (b/->board (:board @s/state) :display)))
    ))

(:possile-move @s/state)
