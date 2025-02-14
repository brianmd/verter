(ns verter.test
  (:require [clojure.test :as t :refer [deftest testing is]]
            [verter.test.tools :as tt]
            [verter.core :as v]
            [verter.store :as vs]))

(t/use-fixtures :each (partial tt/with-db "multiverse"))

(def test-ids [:universe/one
               [1 :a "a"]
               123
               "123"
               "123abc"
               :abc
               "abc"
               'abc
               #uuid "5f62d4c0-c5ca-4431-ac33-610825f9ef36"])

(deftest should-connect-to-db
  (is (tt/is-connected tt/conn)))

(deftest distinct-ids-test
  (let [verter-ids (map v/->verter-id test-ids)]
    (is (= (-> test-ids set count) (-> verter-ids set count)))))

(deftest should-record-facts
  (doseq
    [id test-ids]
    (testing (str "   --- testing: " (v/->verter-id id))
      (is (= id (v/verter-id-> (v/->verter-id id))))
      (v/add-facts tt/conn [{:verter/id id :suns 12 :planets #{:one :two :three}}
                            [{:verter/id :universe/two :suns 3 :life? true} #inst "2019-09-09"]
                            {:verter/id :universe/sixty-six :answer 42}])
      (v/add-facts tt/conn [{:verter/id id :suns 42 :planets #{:one :two :three}}
                            [{:verter/id :universe/two :suns 3 :life? true} #inst "2020-09-09"]
                            {:verter/id :universe/sixty-six :answer 42}])
      (let [result (tt/without-ts
                     (v/facts tt/conn id))
            ids    (->> result (map :verter/id) set)]
        (is (= #{id} ids))
        (is (= [{:suns      12,
                 :planets   #{:one :three :two},
                 :verter/id id}
                {:suns      42,
                 :planets   #{:one :three :two},
                 :verter/id id}]
               result))))))
