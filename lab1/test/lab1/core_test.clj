(ns lab1.core-test
  (:require [clojure.test :refer :all]
            [lab1.core :refer :all]))

(deftest parseLine-test
  (is (= [3 6 9 5] (parseLine "3,6,9,5" 0 4)))
  (is (= [1.2 2.4 3.6] (parseLine "a,1.2,2.4,3.6,b,c" 1 3)))
  (is (= [10 5.5] (parseLine "1,2,3,10,5.5,6,8.1" 3 2))))

(deftest transposeMatrix-test
  (is (= ['(1) '(2)] (transposeMatrix [[1 2]])))
  (is (= ['(1 3) '(2 4)] (transposeMatrix [[1 2] [3 4]])))
  (is (= ['(1 3) '(2 4) '(5 6)] (transposeMatrix [[1 2 5] [3 4 6]]))))

(deftest normalizeVector-test
  (is (= [0.5 1.0] (normalizeVector [1 2])))
  (is (= [0.1 0.2 0.4 1.0] (normalizeVector [1 2 4 10])))
  (is (= [0.1 0.25 0.5 1.0] (normalizeVector [3 7.5 15 30]))))

(deftest getDistance-test
  (is (= 8 (getDistance [0 0] [2 2] 0)))
  (is (= 13 (getDistance [1 1] [4 3] 0)))
  (is (= 7 (getDistance [0 0] [4 3] 1))))


(run-tests 'lab1.core-test)
