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

(defn idx->id
  "0부터 순차대로 증가하는 idx를 문자로 변환 (a 부터)"
  [idx]
  (keyword (str (char (+ idx 97)))))

(defn parse-coordinate
  "0, 0 형식의 문자열을 {:x 0 :y 0} 형식의 맵으로 반환"
  [idx input]
  (let [[x y] (map read-string (string/split input #", "))]
    {:id (idx->id idx) :x x :y y}))

(defn distance-between-coord
  "첫번째 인자는 location 좌표 두번째 인자는 id가 있는 좌표
   두 좌표간에 거리를 https://en.wikipedia.org/wiki/Taxicab_geometry를 사용해서 계산"
  [{x1 :x y1 :y} {id :id x2 :x y2 :y}]
  {id (+ (abs (- x1 x2)) (abs (- y1 y2)))})

(defn get-edge-coord
  "({:id a :x 1 :y 1} {:id f :x 8 :y 9}) 형식의
   좌표들(coords) 중 최소 x, y 최대 x, y 를 반환"
  [coords]
  (let [x-coll (map :x coords)
        y-coll (map :y coords)]
    {:min-x (apply min x-coll)
     :min-y (apply min y-coll)
     :max-x (apply max x-coll)
     :max-y (apply max y-coll)}))

(defn generate-finite-grid
  "{:min-x 0 :min-y 0 :max-x 0 :max-y 0}
   형식의 엣지를 끝으로 하는 유한한 그리드를 반환"
  [{:keys [min-x min-y max-x max-y]}]
  (for [y (range min-y (inc max-y))
        x (range min-x (inc max-x))]
    {:x x :y y}))

(defn closest-reducer ;generate-closest 
  "{:a 2, :b 7} 형식의 closests 중에
   가장 가까운 것들을 [[:a 4]] 형식으로 반환"
  [closests curr]
  (let [closest-dist (second (peek closests))
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
    (ffirst closests)
    :.))

(defn location->result
  "{:min-x 0 :min-y 0 :max-x 0 :max-y 0} 형식의 엣지,
   ({:id a :x 1 :y 1} {:id f :x 8 :y 9})형식의 좌표들(coords),
   {:x 0 :y 0} 형식의 location을 입력으로 받아
   :distances에 location과 coords 간에 거리
   :closest에 가장 가까운 id
   :edge에 가장 바깥에 있는 로케이션인지 여부를 담은 맵을 반환
   예) {:distances {:a 2, :b 7} :closest :a :edge true}"
  [{:keys [min-x max-x min-y max-y]} coords location]
  (let [distance-between' (partial distance-between-coord location)
        distances (->> coords
                       (map distance-between')
                       (apply conj))
        closest (->>  distances
                      (reduce closest-reducer [])
                      closests->mark)
        {:keys [x y]} location
        edge (or (= x min-x)
                 (= x max-x)
                 (= y min-y)
                 (= y max-y))]
    {:distances distances
     :closest closest
     :edge edge}))

(defn get-infinate-marks
  "({:x 0 :y 0} ...) 형식의 grid 내에서
   {:min-x 0 :min-y 0 :max-x 0 :max-y 0} 형식의 엣지 영역에 맞닿아 있는
    마크들의 형식으로 반환. 예) (:a, :b :.)"
  [grid]
  (->> grid
       (filter :edge)
       (map :closest)
       set))

(defn finate-location?
  "#{:a :b} 형식의 무한한 마크에 포함하지 않는 유한한 로케이션인지 검사"
  [infinate-marks location]
  (let [closest (:closest location)]
    (not (infinate-marks closest))))

(defn generate-results [coords]
  (let [edge-coords (get-edge-coord coords)
        location->result' (partial location->result edge-coords coords)]
    (->> (generate-finite-grid edge-coords)
         (map location->result'))))

(defn calc-size-of-largest-finate-erea
  "({:distances {:a 2, :b 7} :closest :a :edge true} ...) 형식의
  results를 입력으로 받아 유한한 영역 중에 가장 큰 영역의 크기를 반환"
  [results]
  (let [infinate-marks (get-infinate-marks results)
        finate-area? (partial finate-location? infinate-marks)
        finate-areas (filter finate-area? results)]
    (->> finate-areas
         (group-by :closest)
         vals
         (map count)
         (apply max))))

(defn solve-6-1
  "https://adventofcode.com/2018/day/6 참조"
  [inputs]
  (->> inputs
       (map-indexed parse-coordinate) ;; Parsing
       generate-results ;; Processing
       calc-size-of-largest-finate-erea)) ;; Aggregate

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

(defn calc-size-of-safe-locations
  "results 안에서 정수 distance-max 보다 :distance가 작은 결과를 찾아 그 합을 반환"
  [distance-max results]
  (let [distances-total's (->> results
                              (map :distances)
                              (map vals)
                              (map #(apply + %)))]
    (->> distances-total's
         (filter #(> distance-max %))
         count)))

(defn solve-6-2 
  "https://adventofcode.com/2018/day/6#part2 참조"
  [inputs distance-max]
  (->> inputs
       (map-indexed parse-coordinate) ;; Parsing
       generate-results ;; Processing
       ((partial calc-size-of-safe-locations distance-max)))) ;; Aggregate

(comment
  (solve-6-2 '("1, 1"
               "1, 6"
               "8, 3"
               "3, 4"
               "5, 5"
               "8, 9") 32) ; it should be equal 16
  (solve-6-2 input 10000))
