(ns aoc2018-7
  (:require [utils :refer [read-resource]]
            [clojure.set :refer [difference]]))

(def inputs (read-resource "aoc.2018.day7.sample.txt"))

;; # Day 7

;; [https://adventofcode.com/2018/day/7](https://adventofcode.com/2018/day/7)

;; ## 파트 1

;; 스케줄이 주어질 때, 일이 처리되는 순서를 반환하시오.
;; 알파벳 캐릭터 하나로 대표되는 일(work)이 주어지고, 각 일을 처리하기 위해서 선행되어야 하는 일들이 스케줄 형식으로 주어짐.
;; ```
;; Step C must be finished before step A can begin.
;; Step C must be finished before step F can begin.
;; Step A must be finished before step B can begin.
;; Step A must be finished before step D can begin.
;; Step B must be finished before step E can begin.
;; Step D must be finished before step E can begin.
;; Step F must be finished before step E can begin.
;; ```
;; 위와 같은 입력의 경우 아래 형태의 그래프가 만들어짐.


;; ```
;;   -->A--->B--
;;  /    \      \
;; C      -->D----->E
;;  \           /
;;   ---->F-----
;; ```

;; 순서는 아래와 같음.
;; - 처음엔 C만 가능함. C에서 시작. 만약 다른 시작점이 존재한다면 알파벳 순서로 진행.
;; - C 다음으로 A와 F가 가능한데, A가 알파벳 우선순위가 높으므로 A로 감.
;; - A 를 완료하면 B, D, F가 가능한데, 역시 알파벳 우선순위가 높은 B가 수행됨.
;; - 남은 D와 F중에서 D가 수행됨
;; - F가 수행됨
;; - E가 수행됨 (E는 B, D, F 모두가 선행되어야 수행될 수 있음)

;; 결과: `CABDFE`

;; instructions [[:C :A], [:C :F], [:A :B], [:A :D], [:B :E], [:D :E], [:F :E]]
;; letters [:A :B :C :D :E :F]
;; group-by instructions letters
;; {:C [], :A [:C], :F [:C], :B [:A], :D [:A], :E [:B :D :F]}
;; flatten-deps
;; {:C [], :A [:C], :F [:C], :B [:A :C], :D [:A :C], :E [:A :B :C :D :F]} **** (part 2)
;; reduce generate-steps
;; [] <= {:C [], :A [:C], :F [:C], :B [:A :C], :D [:A :C], :E [:A :B :C :D :F]}
;; [:C] <= {:A [], :F [], :B [:A], :D [:A], :E [:A :B :D :F]}
;; [:C :A] <= {:F [], :B [], :D [], :E [:B :D :F]}
;; [:C :A :B] <= {:F [], :D [], :E [:D :F]}
;; [:C :A :B :D] <= {:F [], :E [:F]}
;; [:C :A :B :D :F] <= {:E []}
;; [:C :A :B :D :F :E] <= {}


(def instruction-pattern #"Step (\w) must be finished before step (\w) can begin.")

(defn parse-instruction
  "Step C must be finished before step A can begin.
   형식의 input을 입력 받아 [A C] 형식의 instruction으로 반환"
  [input]
  (->> input
       (re-find instruction-pattern)
       (drop 1)
       (mapv seq)
       (apply concat)))

(defn grouping-instructions
  "[[A C] [A B] [B C]] 형식의 instructions을 입력받아
   {A #{C B}, B #{C}} 형식으로 그루핑하여 반환"
  [instructions]
  (let [letters (->> instructions
                     (apply concat)
                     set)
        generate-deps (fn [acc [letter deps]]
                        (let [deps' (set (apply concat deps))]
                          (assoc acc letter (disj deps' letter))))
        deps (->> instructions
                  (group-by #(last %))
                  (reduce generate-deps {}))]
    (merge
     (reduce #(assoc %1 %2 #{}) {} letters)
     deps)))

(defn generate-high-priorities
  "백터 acc와 [A #{C B}] 형식의 requirement를 입력으로 받아
   requirement의 두번째인 deps가 비었다면 requirement를 acc에 추가"
  [acc requirement]
  (let [deps (second requirement)]
    (cond
      (empty? deps) (conj acc requirement)
      :else acc)))

(defn picking-high-priority-requirement
  "{A #{C}, B #{A}, C #{}} 형식의 requirements 중
   가장 우선 순위가 높은 requirements 백터를 반환"
  [requirements]
  (when (not-empty requirements)
    (->> requirements
         (reduce generate-high-priorities []))))

(defn ordering-requirements
  "{A #{C}, B #{A}, C #{}} 형식의 requirements와
   스탭 순서를 결과로 받을 빈 백터 ordered-steps를
   가지는 맵을 입력으로 받아 requirements 중 가장 우선 순위가 높은
   requirement를 빼내 ordered-steps에 채워 반환"
  [state]
  (let [{:keys [requirements ordered-steps]} state
        letter (->> requirements
                    picking-high-priority-requirement
                    (apply min-key #(int (key %)))
                    first)
        requirements' (->
                       requirements
                       (dissoc letter)
                       (update-vals #(disj % letter)))
        ordered-steps' (conj ordered-steps letter)]
    (-> state
        (assoc :requirements requirements')
        (assoc :ordered-steps ordered-steps'))))

(defn solve-7-1
  "https://adventofcode.com/2018/day/7 참고"
  [inputs]
  (let [requirements (->> inputs
                          (mapv parse-instruction)
                          grouping-instructions)] ;; Parsing
    (->> {:requirements requirements ;; Processing
          :ordered-steps []}
         (iterate ordering-requirements)
         (drop-while #(not-empty (:requirements %)))
         first
         :ordered-steps ;; Aggregate
         (apply str))))

(comment
  (solve-7-1 inputs)) ;; Print

;; ## 파트 2

;; 파트 1에서는 일을 워커(worker)가 하나였지만, 파트 2는 5명. 즉, 동시에 5개의 일을 처리할 수 있게 됨.
;; 그리고 각각의 일 (A\~Z)은 처리하는데 (60+1\~60+26)의 시간이 걸림. B는 62초, D는 64초, etc.

;; 이 때, 주어진 모든 일을 처리하는데 걸리는 시간을 구하시오.

;; 예)

;; 아래는 파트 1의 예시에서 워커가 2명이 된 케이스이다.
;; ```
;; Second   Worker 1   Worker 2   Done
;;    0        C          .        
;;    1        C          .        
;;    2        C          .        
;;    3        A          F       C
;;    4        B          F       CA
;;    5        B          F       CA
;;    6        D          F       CAB
;;    7        D          F       CAB
;;    8        D          F       CAB
;;    9        D          .       CABF
;;   10        E          .       CABFD
;;   11        E          .       CABFD
;;   12        E          .       CABFD
;;   13        E          .       CABFD
;;   14        E          .       CABFD
;;   15        .          .       CABFDE
;; ```
;; 15초가 걸리므로 답은 15

(defn get-assinable-requirements [requirements workers]
  (let [idle-count (count (filter empty? workers))
        working-workers (->> workers
                             (map first)
                             (filter #(not (nil? %)))
                             set)
        none-working-requirements (apply
                                   dissoc
                                   requirements
                                   working-workers)]
    (->> none-working-requirements
         picking-high-priority-requirement
         (take idle-count)
         (map first))))

(defn remove-requirements [requirements, remove-requirements]
  (apply dissoc requirements remove-requirements))

(defn remove-done
  [requirements done]
  (update-vals requirements #(difference % (set (map first done)))))

(defn working [worker]
  (cond
    (empty? worker) []
    (let [remaining (second worker)] (<= remaining 1)) []
    :else [(first worker) (dec (second worker))]))

(defn index-of [pred coll]
  (first (keep-indexed (fn [idx x] (when (pred x) idx)) coll)))

(defn assign-to-worker
  [letter sec-for-step workers]
  (let [assignable-idx (index-of empty? workers)
        working-times (- (int letter) (- 64 sec-for-step))]
    (if (nil? assignable-idx)
      workers
      (assoc
       workers
       assignable-idx
       [letter working-times]))))

(defn assign-to-workers [requirements sec-for-step workers]
  (let [requirement (first requirements)]
    (if
     (empty? requirements) workers
     (recur (rest requirements)
            sec-for-step
            (assign-to-worker
             requirement
             sec-for-step
             workers)))))

(defn done? [[_ remaining]] (and (not (nil? remaining)) (<= remaining 1)))

(defn do-work
  [sec-for-step state]
  (let [{:keys [sec requirements workers done]} state
        workers' (mapv working workers)
        assinable-requirements (get-assinable-requirements requirements workers')
        assigned-workers (assign-to-workers assinable-requirements sec-for-step workers')
        done' (filter done? assigned-workers)
        requirements'  (-> requirements
                           (remove-requirements  assinable-requirements)
                           (remove-done  done'))]
    (-> state
        (assoc :sec (inc sec))
        (assoc :requirements requirements')
        (assoc :workers assigned-workers)
        (assoc :done (concat done (mapv first done'))))))

(defn working? [{:keys [workers requirements]}]
  (or (boolean (some seq workers)) (boolean (seq requirements))))

(defn solve-7-2 [inputs number-of-workers sec-for-step]
  (let [requirements (->> inputs
                          (mapv parse-instruction)
                          grouping-instructions);; Parsing
        do-work' (partial do-work sec-for-step)]

    (->> {:sec 0
          :workers (->> (range)
                        (take number-of-workers)
                        (mapv (fn [_] [])))
          :requirements requirements
          :done '()}
         (iterate do-work')
         (take-while working?)
         last
         :sec)))

(comment
  (solve-7-2 ["Step C must be finished before step A can begin."
              "Step C must be finished before step F can begin."
              "Step A must be finished before step B can begin."
              "Step A must be finished before step D can begin."
              "Step B must be finished before step E can begin."
              "Step D must be finished before step E can begin."
              "Step F must be finished before step E can begin."] 2 0)
  (solve-7-2 inputs 5 60))