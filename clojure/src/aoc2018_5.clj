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
  "2개의 유닛을 인자로 받아 이 둘이 극성을 가지는지 판단한다
   a a 혹은 a b 는 false, a A 혹은 A a 는 true"
  [unit1 unit2]
  (let [unit1' (int unit1)
        unit2' (int unit2)]
    (= 32 (abs (- unit1' unit2')))))

;; abs를 사용할 수 있다 (두 값의 차이와 32를 비교할 때)
;; upperCase, lowerCase 함수를 사용할 수 있을것 같다

(defn react-single-cycle
  "한 사이클에 폴리머들이 반응하여 파괴되고 남은 폴리머들을 반환.
   polymers abBA 같은 문자열이 주어지면 '(aA)를 반환"
  [acc curr-unit]
  (let [prev-unit (peek acc)]
    (cond
      (nil? prev-unit) (conj acc curr-unit)
      (polarity? prev-unit curr-unit) (pop acc)
      :else (conj acc curr-unit))))

;; last O(log32 n), peek O(1), pop O(1)
;; https://stackoverflow.com/questions/17048076/big-o-of-clojure-library-functions

(defn chain-react
  "폴리머들이 연쇄적으로 반응하여 파괴되고 남은 폴리머들은 반환
   polymers abBA 같은 문자열이 주어지면 '()를 반환"
  [polymers]
  (let [polymers' (reduce react-single-cycle [] polymers)]
    (if (= (count polymers) (count polymers'))
      polymers'
      (recur polymers'))))

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

(defn char-range [start end] (range (int start) (int end)))

(defn unit-filter
  "폴리머에서 소문자 c의 소문자 혹은 대문자를 제외한 나머지 폴리머를 반환한다
   입력이 :polymers \"abc\" :c 'c' 라면 \"ab\" 를 출력한다"
  [polymers c]
  (letfn [(case-insensitive-matched? [unit]
            (let [unit' (int unit)
                  exact-match (= c unit')
                  upper-match (= 32 (- c unit'))]
              (not (or exact-match upper-match))))]
    (->> polymers
         (filter case-insensitive-matched?)
         (apply str))))

;; 정규식으로 repalce

(defn solve-5-2
  [polymers]
  (->> (char-range \a \z)
       (map (partial unit-filter polymers))
       distinct
       (map chain-react)
       (map count)
       (apply min)))

(comment
  (solve-5-2 "dabAcCaCBAcCcaDA")
  (solve-5-2 polymers))
