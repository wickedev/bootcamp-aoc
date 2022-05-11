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

(defn distance-between
  "두 좌표간에 거리를 https://en.wikipedia.org/wiki/Taxicab_geometry를 사용해서 계산"
  [{x1 :x y1 :y} {id :id x2 :x y2 :y}]
  {id (+ (abs (- x1 x2)) (abs (- y1 y2)))})

(defn with-id
  "인덱스(idx)와 {:x 0 :y 0} 형식의 좌표를 받아
   {:id a :x 0 :y 0} 형식으로 반환
   id는 0 부터 소문자 알파벳 순서로 진행"
  [idx {x :x y :y}]
  {:id (keyword (str (char (+ idx 97)))) :x x :y y})

(defn get-edge-coord
  "({:id a :x 1 :y 1} {:id f :x 8 :y 9}) 형식의
   좌표들(coords) 중 최소 x,y 최대 x,y 를 반환"
  [coords]
  (let [x-coll (map :x coords)
        y-coll (map :x coords)]
    {:min-x (apply min x-coll)
     :min-y (apply min y-coll)
     :max-x (apply max x-coll)
     :max-y (apply max y-coll)}))

(defn generate-finite-grid
  "{:min-x 0 :min-y 0 :max-x 0 :max-y 0}
   형식의 엣지를 끝으로 하는 유한한 그리드를 반환"
  [{:keys [min-x min-y max-x max-y]}]
  (for [y (range min-x (+ max-y 1))
        x (range min-y (+ max-x 1))]
    {:x x :y y}))

(defn assoc-distances
  "{:x 0 :y 0} 형식의 포인트(point)와
   ({:id a :x 1 :y 1} {:id f :x 8 :y 9})형식의 좌표들(coords)
   간에 거리를 추가하여 반환
   예) {:x 0, :y 0, :distances {a 2, b 7}}"
  [coords point]
  (let [distances (->> coords
                       (map (partial distance-between point))
                       (apply conj))]
    (assoc point :distances distances)))

(defn closest-reducer
  "{:a 2, :b 7} 형식의 closests 중에
   가장 가까운 것들을 [[:a 4]] 형식으로 반환"
  [closests curr]
  (let [closest-dist (peek (peek closests))
        curr-dist (val curr)]
    (cond
      (nil? closest-dist) [curr]
      (< closest-dist curr-dist) closests
      (= closest-dist curr-dist) (conj closests curr)
      :else [curr])))

(defn closests->mark
  "[[:a 4] [:e 4]] 형식의 closests를
   :a 와 같은 마크로 변환
   만약 closests가 두개 이상이라면 :.를 반환"
  [closests]
  (if (= (count closests) 1)
    (first (first closests))
    :.))

(defn with-closest
  "{:distances {:a 2, :b 7}} 형식의
   with-distances 중에 가장 가까운 마크를 찾아
   {:distances {:a 2, :b 7} :closest :a} 형식으로 변환"
  [with-distances]
  (let [closest (->> (:distances with-distances)
                     (reduce closest-reducer [])
                     closests->mark)]
    (assoc with-distances :closest closest)))

(defn get-infinate-marks
  "({:x 0 :y 0} ...) 형식의 grid 내에서
   {:min-x 0 :min-y 0 :max-x 0 :max-y 0} 형식의 엣지 영역에 맞닿아 있는
    마크들의 (:a, :b :.) 형식으로 반환"
  [{:keys [min-x min-y max-x max-y]} grid]
  (let [pred-edge (fn [location]
                    (let [x (:x location)
                          y (:y location)]
                      (or (= x min-x)
                          (= x max-x)
                          (= y min-y)
                          (= y max-y))))]
    (->> grid
         (filter pred-edge)
         (map :closest)
         distinct
         set)))

(defn finate-location?
  "#{:a :b} 형식의 무한한 마크에 포함하지 않는 유한한 타일을 검사합니다"
  [infinate-marks location]
  (let [closest (:closest location)]
    (not (contains? infinate-marks closest))))

(defn solve-6-1
  "https://adventofcode.com/2018/day/6 참조"
  [inputs]
  (let [coords (->> inputs
                    (map parse-coordinate)
                    (map-indexed with-id))
        edge-coord (get-edge-coord coords)
        with-distances (partial assoc-distances coords)
        grid (->> (generate-finite-grid edge-coord)
                  (map with-distances)
                  (map with-closest))
        infinate-marks (get-infinate-marks edge-coord grid)
        finate-erea? (partial finate-location? infinate-marks)
        finate-ereas (filter finate-erea? grid)
        largest-finate-erea-size (->> finate-ereas
                                      (group-by :closest)
                                      vals
                                      (map count)
                                      (apply max))]
    largest-finate-erea-size))

(comment
  (solve-6-1 '("1, 1"
               "1, 6"
               "8, 3"
               "3, 4"
               "5, 5"
               "8, 9")) ; it should be equal 17
  (solve-6-1 input))

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

(defn solve-6-2
  "https://adventofcode.com/2018/day/6#part2 참조"
  [inputs distance-max]
  (let [coords (->> inputs
                    (map parse-coordinate)
                    (map-indexed with-id))
        edge-coord (get-edge-coord coords)
        with-distances (partial assoc-distances coords)
        grid (->> (generate-finite-grid edge-coord)
                  (map with-distances)
                  (map with-closest))
        distances-totals (->> grid
                              (map :distances)
                              (map vals)
                              (map #(apply + %)))
        safe-locations-size (->> distances-totals
                                 (filter #(> distance-max %))
                                 count)]
    safe-locations-size))

(comment
  (solve-6-2 '("1, 1"
               "1, 6"
               "8, 3"
               "3, 4"
               "5, 5"
               "8, 9") 32) ; it should be equal 16
  (solve-6-2 input 10000))
