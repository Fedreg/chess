(ns chess-test.core-test
  (:require
   [clojure.test     :refer :all]
   [chess-test.moves :as m]
   [chess-test.core  :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moves tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest diff-test
  (testing "diff fn"
    (is (= 1 (m/diff 1 2)))
    (is (= 1 (m/diff 2 1)))
    (is (= 8 (m/diff 1 9)))
    (is (= 8 (m/diff 9 1)))
    ))

(deftest ior-diff-test
  (testing "ior-diff fn"
    (is (= 1 (m/ior-diff :1 :2)))
    (is (= 1 (m/ior-diff :2 :1)))
    (is (= 7 (m/ior-diff :1 :8)))
    (is (= 7 (m/ior-diff :8 :1)))
    ))

(deftest iof-dff-test
  (testing "iof-diff fn"
    (is (= 1 (m/iof-diff :a :b)))
    (is (= 1 (m/iof-diff :b :a)))
    (is (= 7 (m/iof-diff :a :h)))
    (is (= 7 (m/iof-diff :h :a)))
    ))

(deftest straight-test
  (testing "striaght? fn"
    (is (= true  (m/straight? [:1 :a] [:7 :a])))
    (is (= true  (m/straight? [:1 :a] [:1 :h])))
    (is (= false (m/straight? [:1 :a] [:7 :h])))
    (is (= false (m/straight? [:1 :a] [:2 :h])))
    ))

(deftest pawn-test
  (testing "pawn? fn"
    (is (= true  (m/pawn? [:2 :a] [:3 :a] :white)))
    (is (= true  (m/pawn? [:2 :a] [:4 :a] :white)))
    (is (= true  (m/pawn? [:7 :a] [:6 :a] :black)))
    (is (= false (m/pawn? [:7 :a] [:3 :a] :black)))
    (is (= false (m/pawn? [:2 :a] [:7 :a] :white)))
    (is (= false (m/pawn? [:2 :a] [:4 :a] :black)))
    (is (= false (m/pawn? [:2 :a] [:2 :h] :white)))
    ))

(deftest diagonal-test
  (testing "diagonal? fn"
    (is (= true  (m/diagonal? [:1 :a] [:8 :h])))
    (is (= true  (m/diagonal? [:8 :a] [:1 :h])))
    (is (= false (m/diagonal? [:1 :a] [:1 :h])))
    (is (= false (m/diagonal? [:1 :a] [:1 :h])))
    ))

(deftest el-test
  (testing "el? fn"
    (is (= true  (m/el? [:1 :a] [:3 :b])))
    (is (= true  (m/el? [:3 :b] [:1 :a])))
    (is (= false (m/el? [:1 :a] [:1 :h])))
    (is (= false (m/el? [:1 :a] [:1 :h])))
    ))

(deftest multi-test
  (testing "multi? fn"
    (is (= true  (m/multi? [:1 :a] [:8 :h])))
    (is (= true  (m/multi? [:1 :a] [:1 :h])))
    (is (= false (m/multi? [:1 :a] [:3 :b])))
    (is (= false (m/multi? [:1 :a] [:2 :h])))
    ))

(def w-bishop
   {:max 8,
    :direction :diagonal,
    :attack :diagonal,
    :name :b,
    :value 3,
    :color :white})

(def b-pawn
  {:max 2,
    :direction :straight,
    :attack :diagonal,
    :name :p,
    :value 1,
    :color :black})

(def b-knight
  {:max 2,
   :direction :el,
   :attack :el,
   :name :k,
   :value 3,
   :color :black})

(def w-queen
  {:max 8,
   :direction :multi,
   :attack :multi,
   :name :Q,
   :value 9,
   :color :white})

(def w-pawn
  {:max 2,
    :direction :straight,
    :attack :diagonal,
    :name :p,
    :value 1,
    :color :white})

(def test-board1
  {:8 {:a :r, :b :b, :c b-knight, :d :Q, :e :K, :f :k, :g :b, :h :r},
   :7 {:a :p, :b :p, :c :p, :d :p, :e :p, :f :p, :g :p, :h b-pawn},
   :6 {:a "", :b "", :c "", :d b-pawn, :e "", :f "", :g "", :h ""},
   :5 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
   :4 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
   :3 {:a "", :b "", :c :p, :d "", :e "", :f "", :g "", :h ""},
   :2 {:a :p, :b :p, :c "", :d :p, :e :p, :f :p, :g :p, :h :p},
   :1 {:a :r, :b w-bishop, :c :k, :d :Q, :e :K, :f :k, :g :b, :h :r}})

(def test-board2
  {:8 {:a :r, :b :b, :c :k, :d :Q, :e :K, :f :k, :g :b, :h :r},
   :7 {:a "", :b "", :c :p, :d b-pawn, :e :p, :f :p, :g :p, :h :p},
   :6 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
   :5 {:a "", :b :p, :c "", :d "", :e "", :f "", :g "", :h ""},
   :4 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
   :3 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
   :2 {:a "", :b :p, :c w-pawn, :d "", :e :p, :f :p, :g :p, :h :p},
   :1 {:a :r, :b :b, :c :k, :d w-queen, :e :K, :f :k, :g :b, :h :r}})

(def test-board3
  {:8 {:a :r, :b "", :c "", :d "", :e :K, :f "", :g :b, :h :r},
   :7 {:a :p, :b :p, :c "", :d "", :e "", :f :p, :g "", :h :p},
   :6 {:a "", :b "", :c "", :d "", :e :k, :f "", :g :p, :h ""},
   :5 {:a "", :b "", :c "", :d :p, :e "", :f "", :g "", :h ""},
   :4 {:a "", :b "", :c "", :d "", :e "", :f "", :g "", :h ""},
   :3 {:a "", :b :p, :c "", :d "", :e :p, :f "", :g "", :h :p},
   :2 {:a :p, :b :p, :c "", :d "", :e "", :f :K, :g :p, :h ""},
   :1 {:a :r, :b "", :c "", :d "", :e "", :f "", :g "", :h :r}})

;; 2a -> 3b or , that was already done and now, 1a -> 7a = Index out of bounds exception
(deftest blocked-test
  (testing "blocked? fn"
    (is (= true  (m/diagonal? [:1 :b] [:7 :h])))
    (is (= nil   (m/blocked?  [:1 :b] [:7 :h] test-board1)))
    (is (= true  (m/straight? [:2 :a] [:4 :a])))
    (is (= true  (m/straight? [:1 :a] [:7 :a])))
    (is (= true  (m/pawn?     [:2 :a] [:3 :a] :white)))
    (is (= false (m/pawn?     [:2 :a] [:2 :b] :white)))
    (is (= nil   (m/blocked?  [:2 :a] [:4 :a] test-board1)))
    (is (= true  (m/el?       [:8 :c] [:6 :d])))
    (is (= true  (m/blocked?  [:8 :c] [:6 :d] test-board1)))
    (is (= nil   (m/blocked?  [:8 :c] [:6 :b] test-board1)))
    (is (= true  (m/blocked?  [:1 :d] [:2 :c] test-board2)))
    (is (= nil   (m/blocked?  [:1 :d] [:7 :d] test-board2)))
    ))
