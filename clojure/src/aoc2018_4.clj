(ns aoc2018-4
  (:require [utils :refer [read-resource]]
            [clojure.string :as string]))

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

(def record-pattern #"\[\d{4}-\d{2}-\d{2} \d{2}:(\d{2})\]\s[\w]+ ?\#?(\d+)?.+")
(def guard-shift-pattern #"Guard #(\d+) begins shift")

(defn parse-record
  "[1518-11-01 00:00] x 형식의 레코드를 {log: \"\" :datetime {:year 0 :month 0 :day 0 :hour 0 :minute 0}} 형식으로 파싱"
  [input]
  (let [[_ minute guard] (re-find record-pattern input)
        shift? (not (nil? guard))]
    {:minute (Integer/parseInt minute)
     :shift? shift?
     :guard (when shift? (read-string guard))}))

;; Integer/parseInt 통일

(defn filling-guards
  "raw records ({:minute 0, :shift? true, :guard 10} 
   {:guard nil, :hour nil, :minute 5, :shift? false}) 중
   guard가 nil 인 경우 직전 레코드 id로 채워 넣음"
  [raw-records]
  (reductions (fn [acc {:keys [guard shift? hour minute]}]
                (let [id (:guard acc)]
                  {:guard (or guard id) :hour hour :minute minute :shift? shift?}))
              raw-records))

(defn aggregate-records
  "raw record ({:minute 0, :shift? true, :guard 10}...)를
   {:guard 10, :minutes (0 5 25), :sleep-ranges ((5 25)), :sleep 20} 형식으로 반환"
  [raw-record]
  (println raw-record)
  (let [sleep-ranges (->>  raw-record
                           (filter (fn [x] (not (:shift? x))))
                           (map :minute)
                           (partition 2))]
    {:guard (:guard (first raw-record)),
     :minutes (map :minute raw-record)
     :sleep-ranges sleep-ranges
     :sleep (->>  sleep-ranges
                  (map (partial apply -))
                  (map abs)
                  (apply +))}))

(defn group-by-guard
  "원본 레코드 ({:minute 0, :shift? true, :guard 10} ...) 를 guard 별로 묶음"
  [records]
  (reduce (fn [acc {:keys [guard minutes sleep sleep-ranges]}]
            (let [minutes' (get acc :minutes)
                  sleep' (get acc :sleep)
                  sleep-between' (get acc :sleep-ranges)]
              {:guard guard
               :minutes (concat minutes' minutes)
               :sleep-ranges (concat sleep-between' sleep-ranges)
               :sleep (+ sleep' sleep)})) records))

(defn sleep-sumaries
  "raw-records ({:minute 0, :shift? true, :guard 10} {:guard 10, :hour nil, :minute 5, :shift? false})
   형식의 데이터를 ({:guard 10, :minutes (0 5 25), :sleep-ranges ((5 25)), :sleep 20} ...)
   형식으로 요약"
  [raw-records]
  (->> raw-records
       filling-guards
       (partition-by :guard)
       (map aggregate-records)
       (group-by :guard)
       vals
       (map group-by-guard)))

(defn sleepiest-guard-in
  "정리한 기록 ({:total-sleep ...}) 중에 가장 많이 잔 guard의 기록을 반환
   {:guard 10, :minutes (0 5 25), :sleep-ranges ((5 25)), :sleep 20}"
  [sumaries]
  (println sumaries)
  (->> sumaries
       (sort-by :sleep >)
       first))

(defn sleepiest-minute-in
  "((5 25) (10 30)) 형식의 sleep minute 사이에
   가장 많이 잔 minute을 뽑아서
   [45 1] 처럼 first 에는 minute 두번째에는 빈도를 반환"
  [sleep-ranges]
  (->> sleep-ranges
       (map #(range (first %) (second %)))
       flatten
       frequencies
       (sort-by val >)
       first))

(defn solve-4-1-v2 [inputs]
  (let [records (->> inputs
                     sort
                     (map parse-record))
        sumaries (sleep-sumaries records)
        sleepiest-guard (sleepiest-guard-in sumaries)
        sleep-ranges (:sleep-ranges sleepiest-guard)
        sleepiest-guard-id (:guard sleepiest-guard)
        sleepiest-minute (first (sleepiest-minute-in sleep-ranges))]
    (println sumaries)
    (* sleepiest-guard-id sleepiest-minute)))

(comment
  (solve-4-1-v2
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
  (solve-4-1-v2 input))

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

(defn freq-asleep-same-minute-guard
  "동일한 시간에 잔 빈도가 가장 높은 가드를 반환"
  [sumaries] (->> sumaries
                  (map (fn [x]
                         (let [freq (last (get x :sleepiest-minute  [0]))]
                           [(if (nil? freq) 0 freq) x])))
                  (sort-by first >)
                  (map last)
                  first))

(defn with-sleepiest-minute
  "기존 summaries에 가장 많이 잔 시간을 추가"
  [summaries]
  (map (fn [x]
         (let [sleep-ranges (:sleep-ranges x)
               sleepiest-minute (sleepiest-minute-in sleep-ranges)]
           (assoc x :sleepiest-minute sleepiest-minute)))
       summaries))

(defn solve-4-2 [inputs]
  (let [records (->> inputs
                     sort
                     (map parse-record))
        sumaries (->> records
                      sleep-sumaries
                      with-sleepiest-minute)
        freq-sleep-guard (freq-asleep-same-minute-guard sumaries)
        sleep-ranges (:sleep-ranges freq-sleep-guard)
        freq-sleep-guard-id (:guard freq-sleep-guard)
        freq-sleep-minute (first (sleepiest-minute-in sleep-ranges))]
    (* freq-sleep-guard-id freq-sleep-minute)))

(comment
  (solve-4-2
   ["[1518-11-01 00:00] Guard #10 begins shift"
    "[1518-11-01 00:05] falls asleep"
    "[1518-11-01 00:25] wakes up"
    "[1518-11-01 00:30] falls asleep"
    "[1518-11-01 00:55] wakes up"
    "[1518-11-01 23:58] Guaræd #99 begins shift"
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
  (solve-4-2 input))