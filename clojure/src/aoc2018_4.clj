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

(def record-pattern #"\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\]\s(.*)")
(def guard-shift-pattern #"Guard #(\d+) begins shift")

(defn parse-record
  "[1518-11-01 00:00] x 형식의 레코드를 {:year 0 :month 0 :day 0 :hour 0 :minute 0} 형식으로 파싱"
  [input]
  (let [[_ year month day hour minute log] (re-find record-pattern input)]
    {:log log
     :datetime {:year (read-string year)
                :month (read-string month)
                :day (read-string day)
                :hour (read-string hour)
                :minute (read-string minute)}}))

(defn parse-id
  "Guard #id begins shift 형식 로그에서 id를 파싱"
  [log]
  (let [matched (last (re-find guard-shift-pattern log))]
    (when (seq matched) (read-string matched))))

(comment
  (parse-id "Guard #10 begins shift")
  (parse-id "falls asleep"))

(defn set-top
  "컬렉션 마지막에 원소를 삽입한 새로운 컬랙션을 반환"
  [coll x]
  (conj (pop coll) x))

(defn collapse-records
  [records]
  (reduce (fn [acc {datetime :datetime , log :log}]
            (let [id (parse-id log)]
              #_(println :id id :datetime datetime :log log)
              (match [log]
                ["falls asleep"] (let [elem (last acc)]
                                   (conj acc {:id (:id elem) :timestamp datetime}))
                ["wakes up"] (let [elem (last acc)]
                               (conj acc {:id (:id elem) :timestamp datetime}))
                :else (conj acc {:id id
                                 :timestamp datetime}))))
          [] records))

(defn collapse-timelines
  "id를 기준으로 하나의 맵으로 병합하고 :timeline 에 :timestamp를 모아 리스트로 반환 {:id 1 :timestap: ({...})}"
  [timeline]
  (let [timelines' (map :timestamp timeline)]
    (reduce (fn [_ x]
              {:id (:id x) :timelines timelines'})
            {}
            timeline)))

(defn datetime->minute
  "{:year 0 :month 0 :day 0 :hour 0 :minute 0} 형식의 맵을 분으로 변환"
  [{year :year, month :month, day :day, hour :hour, minute :minute}]
  (+ (* year 525960)
     (* month 43800)
     (* day 1440)
     (* hour 60)
     minute))

(comment
  (datetime->minute {:year 1518, :month 11, :day 1, :hour 0, :minute 0}))

(defn calc-diff-minutes
  "timelines 간에 diff 시간을 리스트로 반환"
  [id timelines diff]
  (let [curr (first timelines)
        next (first (next timelines))]
    (cond
      (nil? next) diff
      (seq timelines) (recur id (rest timelines) (conj diff (- next curr)))
      :else nil)))

(comment
  ;; 각 시간의 차이 만큼 출력 (25 5 20 5)
  (calc-diff-minutes  10 '(798890520 798890525 798890545 798890550 798890575) '()))


(defn diffs->wake-and-sleep
  "diffs (25 5 20 5) 같이 사이의 갭을 짝수 인덱스는 wakes로 홀수는 sleeps로 합산하여 반환 {:wakes 45 :sleeps 10}"
  [diffs]
  (let [spitted-diffs (->> diffs
                           (map-indexed vector)
                           (group-by (fn [x] (odd? (first x))))
                           vals
                           (map (fn [x] (map last x))))
        wakes (first spitted-diffs)
        sleeps (last spitted-diffs)]
    {:wakes (apply + wakes) :sleeps (apply + sleeps)}))

;;; WIP
(defn solve-4-1 [inputs]
  (->> inputs
       (map parse-record)
       collapse-records
       (partition-by :id)
      ;;  (({:id 10, :timestamp {:year 1518, :month 11, :day 1, :hour 0, :minute 0}}
      ;;    {:id 10, :timestamp {:year 1518, :month 11, :day 1, :hour 0, :minute 5}}
      ;;    {:id 10, :timestamp {:year 1518, :month 11, :day 1, :hour 0, :minute 25}}
      ;;    {:id 10, :timestamp {:year 1518, :month 11, :day 1, :hour 0, :minute 30}}
      ;;    {:id 10, :timestamp {:year 1518, :month 11, :day 1, :hour 0, :minute 55}})
      ;;   ({:id 99, :timestamp {:year 1518, :month 11, :day 1, :hour 23, :minute 58}}
      ;;    {:id 99, :timestamp {:year 1518, :month 11, :day 2, :hour 0, :minute 40}}
      ;;    {:id 99, :timestamp {:year 1518, :month 11, :day 2, :hour 0, :minute 50}})
      ;;   ({:id 10, :timestamp {:year 1518, :month 11, :day 3, :hour 0, :minute 5}}
      ;;    {:id 10, :timestamp {:year 1518, :month 11, :day 3, :hour 0, :minute 24}}
      ;;    {:id 10, :timestamp {:year 1518, :month 11, :day 3, :hour 0, :minute 29}})
      ;;   ({:id 99, :timestamp {:year 1518, :month 11, :day 4, :hour 0, :minute 2}}
      ;;    {:id 99, :timestamp {:year 1518, :month 11, :day 4, :hour 0, :minute 36}}
      ;;    {:id 99, :timestamp {:year 1518, :month 11, :day 4, :hour 0, :minute 46}}
      ;;    {:id 99, :timestamp {:year 1518, :month 11, :day 5, :hour 0, :minute 3}}
      ;;    {:id 99, :timestamp {:year 1518, :month 11, :day 5, :hour 0, :minute 45}}
      ;;    {:id 99, :timestamp {:year 1518, :month 11, :day 5, :hour 0, :minute 55}}))
       (map collapse-timelines)
      ;;  ({:id 10,
      ;;    :timelines
      ;;    ({:year 1518, :month 11, :day 1, :hour 0, :minute 0}
      ;;     {:year 1518, :month 11, :day 1, :hour 0, :minute 5}
      ;;     {:year 1518, :month 11, :day 1, :hour 0, :minute 25}
      ;;     {:year 1518, :month 11, :day 1, :hour 0, :minute 30}
      ;;     {:year 1518, :month 11, :day 1, :hour 0, :minute 55})}
      ;;   {:id 99,
      ;;    :timelines
      ;;    ({:year 1518, :month 11, :day 1, :hour 23, :minute 58}
      ;;     {:year 1518, :month 11, :day 2, :hour 0, :minute 40}
      ;;     {:year 1518, :month 11, :day 2, :hour 0, :minute 50})}
      ;;   {:id 10,
      ;;    :timelines
      ;;    ({:year 1518, :month 11, :day 3, :hour 0, :minute 5}
      ;;     {:year 1518, :month 11, :day 3, :hour 0, :minute 24}
      ;;     {:year 1518, :month 11, :day 3, :hour 0, :minute 29})}
      ;;   {:id 99,
      ;;    :timelines
      ;;    ({:year 1518, :month 11, :day 4, :hour 0, :minute 2}
      ;;     {:year 1518, :month 11, :day 4, :hour 0, :minute 36}
      ;;     {:year 1518, :month 11, :day 4, :hour 0, :minute 46}
      ;;     {:year 1518, :month 11, :day 5, :hour 0, :minute 3}
      ;;     {:year 1518, :month 11, :day 5, :hour 0, :minute 45}
      ;;     {:year 1518, :month 11, :day 5, :hour 0, :minute 55})})
       (map #(update % :timelines (fn [x] (map datetime->minute x))))
      ;;  ({:id 10, :timelines (798890520 798890525 798890545 798890550 798890575)}
      ;;   {:id 99, :timelines (798891958 798892000 798892010)}
      ;;   {:id 10, :timelines (798893405 798893424 798893429)}
      ;;   {:id 99, :timelines (798894842 798894876 798894886 798896283 798896325 798896335)})
       (map (fn [{id :id, timelines :timelines}]
              (let [diffs (calc-diff-minutes id timelines '())]
                {:id id, :diffs (diffs->wake-and-sleep diffs)})))))
      ;; ({:id 10, :diffs {:wakes 45, :sleeps 10}}
      ;;  {:id 99, :diffs {:wakes 10, :sleeps 42}}
      ;;  {:id 10, :diffs {:wakes 5, :sleeps 19}}
      ;;  {:id 99, :diffs {:wakes 1441, :sleeps 52}})

;; diffs 를 인덱스로 분할하려고 함

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

;; 한타
;; Q1. 함수를 나누는 모범사례 단일 / 다중 항목
;; A. 단일 인자 함수를 만들고 map 을 쓰는 것을 권장
;; Q2. collapse-records 처럼 특정 키 값으로 합치는 로직이 지저분
;; 마지막 원소에서 id를 가져오는 것이 함수형 스럽지 않음. 좀더 함수형다운 접근이 가능하지 않을지?
;; Q3. frequencies 내에서 persistent! assoc! 같은 최적화 기법이 많이 쓰이는지
;; A. 현업 수준의 최적화는 보통 hashmap 구조를 바꾸거나 너무 당연한 db query 최적화 정도
;;    리턴받는 데이터 양이 많아서 비동기가 필요하거나 캐시 처럼 쓸 때는 delay, future 같은 비동기 관련 함수


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

