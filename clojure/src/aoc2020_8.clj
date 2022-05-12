(ns aoc2020_8
  (:require
   [utils :refer [read-resource]]))

(def inputs (read-resource "2020.day8.input.txt"))

(def instruction-pattern #"([a-z]{3}) ([+-]\d+)")

(defn parse-instruction
  "acc -3 형식의 문자열을 {:operation \"acc\" :argument -3} 형식으로 반환"
  [idx input]
  (let [[_ operation argument] (re-find instruction-pattern input)]
    {:ptr idx
     :operation operation
     :argument (Integer/parseInt argument)}))

(defn infinite-loop? [call-stack ptr]
  (not (nil? (some #(= ptr %) call-stack))))

(defn generate-on-nop [ctx ptr _ _]
  (let [call-stack (:call-stack ctx)
        next-ptr (inc ptr)]
    (assoc ctx :call-stack (conj call-stack next-ptr))))

(defn generate-on-acc [ctx ptr argument _]
  (let [call-stack (:call-stack ctx)
        next-ptr (inc ptr)
        result (:result ctx)]
    (-> ctx
        (assoc :call-stack (conj call-stack next-ptr))
        (assoc :result (+ result argument)))))

(defn generate-on-jmp [ctx ptr argument step]
  (let [call-stack (:call-stack ctx)
        next-ptr (+ ptr argument)]
    (if
     (infinite-loop? call-stack next-ptr)
      (reduced (-> ctx
                   (assoc :call-stack (conj call-stack next-ptr))
                   (assoc :infinite-loop-at {:ptr next-ptr
                                             :step step})))
      (assoc ctx :call-stack (conj call-stack next-ptr)))))

(def logic-per-instruction
  {"nop" generate-on-nop
   "acc" generate-on-acc
   "jmp" generate-on-jmp})

(defn generate-results [instructions ctx step]
  (let [call-stack (get ctx :call-stack)
        ptr (or (peek call-stack) 0)
        {:keys [ptr operation argument]} (nth instructions ptr)]
    ((get logic-per-instruction operation) ctx ptr argument step)))

(defn solve-8-1
  "https://adventofcode.com/2020/day/8 참조"
  [inputs]
  (let [instructions (map-indexed parse-instruction inputs)
        generate-results' (partial generate-results instructions)]
    (->> (iterate inc 2)
         (reduce generate-results' {:call-stack [0]
                                    :result 0
                                    :infinite-loop-at nil}))))

;; expect {:call-stack [0 1 2 6 7 3 4 1], :result 5, :infinite-loop-at {:ptr 1, :step 8}}

(comment
  (solve-8-1 '("nop +0" ;; 0
               "acc +1" ;; 1
               "jmp +4" ;; 2
               "acc +3" ;; 3
               "jmp -3" ;; 4
               "acc -99" ;; 5
               "acc +1" ;; 6
               "jmp -4" ;; 7
               "acc +6") ;; 8
             )
  (solve-8-1 inputs))

(comment
  (contains? [0 1 2 6 7] 3)
  (not (nil? (some #(= 3 %) [0 1 2 6 7]))))