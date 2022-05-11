(ns aoc2018_6
  (:require [clojure.string :as string]
            [utils :refer [read-resource]]))

(def input (read-resource "2018.day6.input.txt"))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(defn parse-coordinate
  "0, 0 형식의 문자열을 {:x 0 :y 0} 형식의 맵으로 반환"
  [input]
  (let [[x y] (map read-string (string/split input #", "))] {:x x :y y}))

(defn max-in
  "({} ...) 형식의 맵 시퀀스 중에서 선택된 키의 가장 높은 값을 반환"
  [maps key]
  (->> maps
       (map key)
       (apply max)))

(defn distance-between
  "두 좌표간에 거리를 https://en.wikipedia.org/wiki/Taxicab_geometry를 사용해서 계산"
  [{x1 :x y1 :y} {id :id x2 :x y2 :y}]
  {id (+ (abs (- x1 x2)) (abs (- y1 y2)))})

(defn with-id
  "인덱스(idx)와 {:x 0 :y 0} 형식의 좌표를 받아
   {:id a :x 0 :y 0} 형식으로 반환
   id는 0 부터 소문자 알파벳 순서로 진행"
  [idx {x :x y :y}]
  {:id (char (+ idx 97)) :x x :y y})

(defn generate-finite-grid
  "좌표들 ({:id a :x 1 :y 1} {:id f :x 8 :y 9})
   중 가장 큰 (x, y)를 끝으로 하는 유한한 그리드를 반환"
  [coords]
  (let [max-x (max-in coords :x)
        max-y (max-in coords :y)]
    (for [x (range 0 max-x), y (range 0 max-y)]
      {:x x
       :y y})))

(defn assoc-distances
  "한 포인트(point) {:x 0 :y 0} 에 
   좌표들(coords) ({:id a :x 1 :y 1} {:id f :x 8 :y 9})
   간에 거리를 추가하여 반환
   예) {:x 0, :y 0, :distances {a 2, b 7}}"
  [coords point]
  (let [distances (->> coords
                       (map (partial distance-between point))
                       (apply conj))]
    (assoc point :distances distances)))

(defn solve-6-1
  "https://adventofcode.com/2018/day/6 참조"
  [inputs]
  (let [coords (->> inputs
                    (map parse-coordinate)
                    (map-indexed with-id))
        with-distances (partial assoc-distances coords)
        grid (->> (generate-finite-grid coords)
                  (map with-distances))]
    grid))

(comment
  (solve-6-1 '("1, 1"
               "1, 6"
               "8, 3"
               "3, 4"
               "5, 5"
               "8, 9")))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.
