(ns aoc2020-1
  (:require [clojure.math.combinatorics :as combo]
            [utils :refer [read-resource]]))

(def inputs (read-resource "aoc.2020.day1.sample.txt"))

;; # Day 1

;; [https://adventofcode.com/2020/day/1](https://adventofcode.com/2020/day/1)

;; ## 파트 1
;; 더해서 2020이 되는 두 숫자의 곱을 구하시오. (두 숫자는 유일하다고 가정)

;; 예) 1721 979 366 299 675 1456 의 경우 1721 * 299 = 514579 를 출력

(defn sum-eq?
  "numbers를 전부 더했을 때 expect와 일치하는지 검사"
  [expect & [numbers]]
  (= expect (apply + numbers)))

(defn solve-1-1
  "https://adventofcode.com/2020/day/1 참조
   expect는 두 숫자를 더해서 일치하려고 하려는 정수"
  [inputs expect]
  (->> inputs
       (map #(Integer/parseInt %)) ; Parse
       (#(combo/combinations % 2)) ; Processing
       (filter (partial sum-eq? expect))
       (apply concat) ; Aggregate
       (reduce *)))

(comment
  (solve-1-1 ["1721"
              "979"
              "366"
              "299"
              "675"
              "1456"] 2020)
  (solve-1-1 inputs 2020))

;; ## 파트 2
;; 같은 입력이 주어질 때, 더해서 2020이 되는 세 숫자의 합을 구하시오.

;; 예) 2020 = 979 + 366 + 675, 곱하면 241861950 을 출력

(defn solve-1-2
  "https://adventofcode.com/2020/day/1#part2 참조
   expect는 세 숫자를 더해서 일치하려고 하려는 정수"
  [inputs expect]
  (->> inputs
       (map #(Integer/parseInt %)) ; Parse
       (#(combo/combinations % 3)) ; Processing
       (filter (partial sum-eq? expect))
       (apply concat) ; Aggregate
       (reduce *)))

(comment
  (solve-1-2 ["1721"
              "979"
              "366"
              "299"
              "675"
              "1456"] 2020)
  (solve-1-2 inputs 2020))