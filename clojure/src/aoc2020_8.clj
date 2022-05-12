(ns aoc2020_8
  (:require
   [utils :refer [read-resource]]
   [clojure.core.match :refer [match]]))

(def inputs (read-resource "2020.day8.input.txt"))

;; # Day 8

;; [https://adventofcode.com/2020/day/8](https://adventofcode.com/2020/day/8)

;; ## 파트 1
;; 일련의 지시가 입력으로 주어진다.
;; - **acc**는 전역 변수를 증가/감소 시키는 역할을 한다. acc +7은 accumulator를 7 증가 시킨다. accumulator는 0에서 시작한다.
;; - **jmp**는 현재 위치에 기반하여 새로운 지시로 넘어간다. jmp +1은 바로 다음의 지시로 넘어가는 것이고, jmp +2는 바로 다음의 지시는 건너뛰고 그 다음의 지시를 실행하는 것이다.
;; - **nop** 는 아무것도 하지 않는다.
;;   아래는 예시이다.
;; ```
;; nop +0
;; acc +1
;; jmp +4
;; acc +3
;; jmp -3
;; acc -99
;; acc +1
;; jmp -4
;; acc +6
;; ```
;; 위의 예시는 아래의 순서로 실행된다.
;; ```
;; nop +0  | 1
;; acc +1  | 2, 8(!)
;; jmp +4  | 3
;; acc +3  | 6
;; jmp -3  | 7
;; acc -99 |
;; acc +1  | 4
;; jmp -4  | 5
;; acc +6  |
;; ```
;; 이 지시들은 무한히 반복된다.

;; 한 지시가 정확히 **두번 실행되는 시점 바로 전**의 acc의 값을 반환하라.
;; 위의 예시에선 acc +1이 8번째 틱에서 정확히 두번 실행되고, 이 때의 acc의 값은 5이다.

(def instruction-pattern #"([a-z]{3}) ([+-]\d+)")

(defn parse-instruction
  "acc -3 형식의 문자열을 {:ptr 0 :operation :acc :argument -3} 형식으로 반환"
  [input]
  (let [[_ operation argument] (re-find instruction-pattern input)]
    {:operation (keyword operation)
     :argument (Integer/parseInt argument)}))

(defn infinity-loop?
  "{:instructions [{:operation :nop, :argument 0} ...],
   :offset 2, :visited [0 1], :result 1} 형식의 상태를 입력 받아
   visited 중에 offset이 있는지 여부를 검사. 만약 이미 있다면 무한 루프"
  [{:keys [visited offset]}]
  (some #(= offset %) visited))

(defn execute-acc
  "{:instructions [{:operation :nop, :argument 0} ...],
   :offset 2, :visited [0 1], :result 1} 형식의 상태를 입력 받아
   offset을 1만큼 증가시키고, 현재 offset을 visited에 추가하며,
   result를 argument 만큼 증가"
  [{:keys [instructions offset visited result]} argument]
  {:instructions instructions
   :offset (inc offset)
   :visited (conj visited offset)
   :result (+ argument result)})

(defn execute-jmp
  "{:instructions [{:operation :nop, :argument 0} ...],
   :offset 2, :visited [0 1], :result 1} 형식의 상태를 입력 받아
   offset을 argument 만큼 증가시키며, 현재 offset을 visited에 추가"
  [{:keys [instructions offset visited result]} argument]
  {:instructions instructions
   :offset (+ argument offset)
   :visited (conj visited offset)
   :result result})

(defn execute-nop
  "{:instructions [{:operation :nop, :argument 0} ...],
   :offset 2, :visited [0 1], :result 1} 형식의 상태를 입력 받아
   아무것도 하지 않고 offset을 1 증가시키며, 현재 offset을 visited에 추가"
  [{:keys [instructions offset visited result]}]
  {:instructions instructions
   :offset (inc offset)
   :visited (conj visited offset)
   :result result})

(defn execute
  "{:instructions [{:operation :nop, :argument 0} ...],
   :offset 2, :visited [0 1], :result 1} 형식의 상태를 입력 받아
   :offset 위치의 operation를 수행하고 그 결과를 반화"
  [state]
  (let [{:keys [instructions offset]} state
        {:keys [operation argument]} (nth instructions offset nil)]
    (match [operation]
      [:acc] (execute-acc state argument)
      [:jmp] (execute-jmp state argument)
      [:nop] (execute-nop state))))

(defn solve-8-1
  "https://adventofcode.com/2020/day/8 참조"
  [inputs]
  (let [instructions (map parse-instruction inputs)]
    (->> {:instructions instructions
          :visited []
          :offset 0
          :result 0}
         (iterate execute)
         (take-while #(not (infinity-loop? %)))
         last
         :result)))

(comment
  (solve-8-1 ["nop +0"
              "acc +1"
              "jmp +4"
              "acc +3"
              "jmp -3"
              "acc -99"
              "acc +1"
              "jmp -4"
              "acc +6"])
  (solve-8-1 inputs))

;; state-machine
;; 함수형과 상태 머신은 맞닿아있음

;; composability
;; monad - identity + compose
;; input -> function1 -> function2 -> function3 .....!
;; input -> iterate -> infinite? -> answer

;; initial-state -> iterate + function -> drop-while/take-while (lazy sequence + condition(infinite?)) -> final state -> value
;;

;; ## 파트 2
;; 주어진 지시들 중, 정확히 하나의 지시가 잘못된 것을 알게 되었다.
;; 정확히 하나의 jmp가 nop가 되어야하거나, nop가 jmp가 되면 프로그램은 **종료**된다.

;; ```
;; nop +0  | 1
;; acc +1  | 2
;; jmp +4  | 3
;; acc +3  |
;; jmp -3  |
;; acc -99 |
;; acc +1  | 4
;; nop -4  | 5 ;; 여기!
;; acc +6  | 6
;; ```

;; 위의 예시에서, "여기!" 라고 표기된 곳이 jmp에서 nop로 바뀌면, 지시는 무한히 반복하지 않고 마지막에 6을 반환하며 종료된다.
;; 프로그램이 종료되는 시점의 accumulator의 값을 반환하여라.

;; record
;; spec

(defn fix-instruction-at
  "[{:nop 0} {:acc +6} ...] 형태의 instructions의 offset 위치에
   있는 명령어의 :operation이 nop이면 jmp로 jmp면 nop으로 수정"
  [instructions offset]
  (let [instruction (nth instructions offset)
        operation (:operation instruction)
        fixed-instruction (assoc
                           instruction
                           :operation
                           (cond
                             (= operation :nop) :jmp
                             (= operation :jmp) :nop
                             :else operation))]
    (assoc instructions offset (assoc fixed-instruction :fixed true))))

(defn variations-of-fixed-instructions
  "[{:operation :nop, :argument 0} ...] 형식의 instructions 중
   하나의 :operation이 nop이면 jmp로 jmp면 nop으로 수정한 모든 경우의 명령어들을 반환"
  [instructions]
  (let [fix-instruction-at' (partial fix-instruction-at (vec instructions))]
    (->> (iterate inc 0)
         (take (count instructions))
         (map fix-instruction-at')
         distinct)))

(defn exit-normal?
  "{:instructions [{:operation :nop, :argument 0} ...],
    :offset 2, :visited [0 1], :result 1} 형식의 상태 중
   instructions 크기보다 정수 offset이 크다면 정상 종료로 판단"
  [{:keys [instructions offset]}]
  (>= offset (count instructions)))

(defn infinity-loop-or-exit-normal?
  "{:instructions [{:operation :nop, :argument 0} ...],
    :offset 2, :visited [0 1], :result 1} 형식의 상태가
   무한 루프에 빠졌거나 정상 종료 되었는지 판단"
  [state]
  (or (exit-normal? state) (infinity-loop? state)))

(defn excute-until-infinity-or-exit-normal
  "[{:operation :nop, :argument 0} ...] 형식의 instructions을 실행하고
   무한 루프 정상 종료 혹은 정상 종료되면
   {:instructions [{:operation :nop, :argument 0} ...],
    :offset 2, :visited [0 1], :result 1} 형식으로 반환"
  [instructions]
  (->> {:instructions instructions
        :visited []
        :offset 0
        :result 0}
       (iterate execute)
       (drop-while #(not (infinity-loop-or-exit-normal? %)))
       first))

(defn solve-8-2
  "https://adventofcode.com/2020/day/8#part2 참조"
  [inputs]
  (->>  inputs
        (map parse-instruction)
        variations-of-fixed-instructions
        (map excute-until-infinity-or-exit-normal)
        (filter exit-normal?)
        first
        :result))

(comment
  (solve-8-2 '("nop +0"
               "acc +1"
               "jmp +4"
               "acc +3"
               "jmp -3"
               "acc -99"
               "acc +1"
               "jmp -4"
               "acc +6"))
  (solve-8-2 inputs))
