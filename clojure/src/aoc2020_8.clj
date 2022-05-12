(ns aoc2020_8
  (:require
   [utils :refer [read-resource]]))

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
  "acc -3 형식의 문자열을 {:operation \"acc\" :argument -3} 형식으로 반환"
  [idx input]
  (let [[_ operation argument] (re-find instruction-pattern input)]
    {:ptr idx
     :operation operation
     :argument (Integer/parseInt argument)}))

(defn infinite-loop?
  "기존 호출한 위치 중에 ptr이 있는지 여부를 검사
   만약 이미 있다면 무한 루프"
  [call-stack ptr]
  (boolean (some #(= ptr %) call-stack)))

(defn generate-on-nop
  "nop 연산자일 경우 다음 ptr을 1증가 시킨 뒤 call-stack에 추가
   {:call-stack [0] :result 0} 형식으로 반환"
  [ctx ptr _ _]
  (let [call-stack (:call-stack ctx)
        next-ptr (inc ptr)]
    (assoc ctx :call-stack (conj call-stack next-ptr))))

(defn generate-on-acc
  "acc 연산자일 경우 다음 ptr을 1증가 시킨 뒤
   call-stack에 추가하고 argument를 :result에 누산
   {:call-stack [0] :result 0} 형식으로 반환"
  [ctx ptr argument _]
  (let [call-stack (:call-stack ctx)
        next-ptr (inc ptr)
        result (:result ctx)]
    (-> ctx
        (assoc :call-stack (conj call-stack next-ptr))
        (assoc :result (+ result argument)))))

(defn generate-on-jmp
  "jmp 연산자일 경우 다음 ptr을 argument 만큼 증가 시킨 뒤
   call-stack에 추가, 만약 다음 스탭에서 무한 루프가 발생한다면
   :infinite-loop-at에 다음 ptr과 스탭을 추가하고 reduce를 종료
   {:call-stack [0] :result 0} 형식으로 반환"
  [ctx ptr argument step]
  #_(prn :generate-on-jmp ptr argument)
  (let [call-stack (:call-stack ctx)
        next-ptr (+ ptr argument)]
    (if
     (infinite-loop? call-stack next-ptr)
      (reduced (-> ctx
                   (assoc :call-stack (conj call-stack next-ptr))
                   (assoc :infinite-loop-at {:ptr next-ptr
                                             :step step})))
      (-> ctx
          (assoc :call-stack (conj call-stack next-ptr))))))

(def logic-per-instruction ;; 오퍼레이션과 일치하는 로직
  {"nop" generate-on-nop
   "acc" generate-on-acc
   "jmp" generate-on-jmp})

(defn generate-result
  [instructions ctx step]
  (let [call-stack (get ctx :call-stack)
        ptr (or (peek call-stack) 0)
        instruction (nth instructions ptr nil)
        {:keys [ptr operation argument]} instruction]
    (if (nil? instruction) (reduced ctx)
        ((get logic-per-instruction operation) ctx ptr argument step))))

(defn accumulator
  "instructions을 누산하여 결과를 {:call-stack [0] :result 0} 형식으로 반환
   만약 무한 루프가 발생한다면 발생하는 다음 ptr과 스탭을 :infinite-loop-at에 추가"
  [instructions]
  (let [generate-result' (partial generate-result instructions)]
    (->> (iterate inc 2)
         (reduce generate-result' {:call-stack [0]
                                    :result 0}))))

(defn solve-8-1
  "https://adventofcode.com/2020/day/8 참조"
  [inputs] (->>
            inputs
            (map-indexed parse-instruction)
            accumulator
            (:result)))

;; expect {:call-stack [0 1 2 6 7 3 4 1], :result 5, :infinite-loop-at {:ptr 1, :step 8}}

(comment
  (solve-8-1 '("nop +0"
               "acc +1"
               "jmp +4"
               "acc +3"
               "jmp -3"
               "acc -99"
               "acc +1"
               "jmp -4"
               "acc +6"))
  (solve-8-1 inputs))

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

(defn fix-instruction-at
  "instructions의 ptr 위치에 있는 명령어의 :operation이 nop이면 jmp로 jmp면 nop으로 수정"
  [instructions ptr]
  (let [instruction (nth instructions ptr)
        operation (:operation instruction)
        fixed-instruction (assoc instruction :operation
                                 (cond
                                   (= operation "nop") "jmp"
                                   (= operation "jmp") "nop"
                                   :else operation))]
    (assoc instructions ptr fixed-instruction)))

(defn variations-of-fixed-instructions
  "하나의 :operation이 nop이면 jmp로 jmp면 nop으로 수정한 모든 경우의 명령어들을 반환"
  [instructions]
  (let [fix-instruction-at' (partial fix-instruction-at (vec instructions))]
    (->> (iterate inc 0)
         (take (count instructions))
         (map fix-instruction-at'))))

(defn solve-8-2
  "https://adventofcode.com/2020/day/8#part2 참조"
  [inputs]
  (->>  inputs
        (map-indexed parse-instruction)
        variations-of-fixed-instructions  ;; Parse
        (map accumulator)  ;; Processing
        (filter #(not (contains? % :infinite-loop-at))) ;; Aggregate
        (first)
        (:result)))

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
