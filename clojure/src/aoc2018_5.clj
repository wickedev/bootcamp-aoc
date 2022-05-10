(ns aoc2018_5
  (:require [utils :refer [read-resource]]))

(def input (read-resource "aoc2018_5_sample.txt"))

(def polymers (first input))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(defn polarity?
  "2개의 문자를 인자로 받아 이 둘이 극성을 가지는지 판단한다
   a a 혹은 a b 는 false, a A 혹은 A a 는 true"
  [c1 c2]
  (let [c1' (int c1)
        c2' (int c2)]
    (or (= c1' (+ c2' 32)) (= c1' (- c2' 32)))))

(defn react-single-cycle
  "한 사이클에 폴리머들이 반응하여 파괴되고 남은 폴리머들을 반환.
   polymers abBA 같은 문자열이 주어지면 '(aA)를 반환"
  [acc curr-unit]
  (let [prev-unit (last acc)]
    (cond
      (nil? prev-unit) (conj acc curr-unit)
      (polarity? prev-unit curr-unit) (pop acc)
      :else (conj acc curr-unit))))

(defn chain-react
  "폴리머들이 연쇄적으로 반응하여 파괴되고 남은 폴리머들은 반환
   polymers abBA 같은 문자열이 주어지면 '()를 반환"
  [polymers]
  (let [polyers' (reduce react-single-cycle [] polymers)]
    (if (= (count polymers) (count polyers'))
      polyers'
      (recur polyers'))))

(defn solve-5-1
  [polymers]
  (count (chain-react polymers)))

(comment
  (solve-5-1 "dabAcCaCBAcCcaDA")
  (solve-5-1 polymers))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.
