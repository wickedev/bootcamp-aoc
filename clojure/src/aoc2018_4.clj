(ns aoc2018-4
  (:require [utils :refer [read-resource]]
            [clojure.core.match :refer [match]]))

(def input (read-resource "aoc2018_4_sample.txt"))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

;; 함수를 나누는 모범사례 단일 / 다중 항목

(def record-pattern #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\]\s(.*)")

(defn parse-record
  [input]
  (let [[_ year month day hour minute log] (re-find record-pattern input)]
    [{:year (read-string year)
      :month (read-string month)
      :day (read-string day)
      :hour (read-string hour)
      :minute (read-string minute)} log]))

(defn parse-records [inputs] (map parse-record inputs))

(def guard-shift-pattern #"Guard #(\d+) begins shift")

(defn parse-id [log]
  (let [matched (last (re-find guard-shift-pattern log))]
    (if (seq matched) (read-string matched) nil)))

(comment
  (parse-id "Guard #10 begins shift")
  (parse-id "falls asleep"))

(defn set-top
  [coll x]
  (conj (pop coll) x))

(defn collapse-records
  [records]
  (reduce (fn [acc [datetime log]]
            (let [id (parse-id log)]
              #_(println :id id :datetime datetime :log log)
              (match [log]
                ["falls asleep"] (let [elem (last acc)]
                                   (conj acc {:id (:id elem) :sleep datetime}))
                ["wakes up"] (let [elem (last acc)]
                               (conj acc {:id (:id elem) :awake datetime}))
                :else (conj acc {:id id
                                 :awake datetime}))))
          [] records))

(defn collapse-timelines
  [timeline]
  (let [timelines' (map #(dissoc % :id) timeline)]
    (reduce (fn [_ x]
              {:id (:id x) :timelines timelines'})
            {}
            timeline)))

(defn solve-4-1 [inputs]
  (->> inputs
       (map parse-record)
       collapse-records
       (partition-by :id)
       (map collapse-timelines)))

(comment
  (solve-4-1
   ["[1518-11-01 00:00] Guard #10 begins shift"
    "[1518-11-01 00:05] falls asleep"
    "[1518-11-01 00:25] wakes up"
    "[1518-11-01 00:30] falls asleep"
    "[1518-11-01 00:55] wakes up"
    "[1518-11-01 23:58] Guard #99 begins shift"
    "[1518-11-02 00:40] falls asleep"
    "[1518-11-02 00:50] wakes up"
    "[1518-11-03 00:05] Guard #10 begins shift"
    "[1518-11-03 00:24] falls asleep"
    "[1518-11-03 00:29] wakes up"
    "[1518-11-04 00:02] Guard #99 begins shift"
    "[1518-11-04 00:36] falls asleep"
    "[1518-11-04 00:46] wakes up"
    "[1518-11-05 00:03] Guard #99 begins shift"
    "[1518-11-05 00:45] falls asleep"
    "[1518-11-05 00:55] wakes up"])
  (solve-4-1 input))



