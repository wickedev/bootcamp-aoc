(ns aoc2018_3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.trace :as trace :refer (trace)]))

(def input (-> "day3.sample2.txt"
               (io/resource)
               (slurp)
               (str/split-lines)))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(def input-pattern #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)")

(defn parse-input
  "#1 @ 1,3: 4x4 패턴의 옵션을 파싱"
  [s] (-> (->> s
               (re-find input-pattern)
               rest
               (zipmap [:ids :x :y :w :h]))
          (update :ids #(str/split % #""))
          (update :x read-string)
          (update :y read-string)
          (update :w read-string)
          (update :h read-string)))

(comment
  (parse-input "#1 @ 1,3: 4x4") ; {:ids ["1"], :x 1, :y 3, :w 4, :h 4} 
  (parse-input "#11 @ 11,33: 44x44") ; {:id 11, :x 11, :y 33, :width 44, :height 44}
  )

(defn repeatedly-indexed
  "repeatedly의 indexed 버전"
  [end f]
  (map-indexed (fn [i _] (f i)) (range end)))

(comment
  (repeatedly-indexed 3 (fn [y] (repeatedly-indexed 3 (fn [x] [x y])))))

(defn make-fabric
  "id에 해당하는 패브릭을 만듭니다."
  [{id :id x :x y :y w :w h :h}]
  (let [width (+ x w)
        height (+ y h)]
    (repeatedly-indexed
     height
     (fn [y']
       (repeatedly-indexed
        width
        (fn [x']
          (cond
            (and (<= x x')
                 (<= y y')
                 (<= x' width)
                 (<= y' height)) id
            :else \')))))))

(comment
  (make-fabric {:id "1" :x 1 :y 3 :w 4 :h 4}))
;; ((\' \' \' \' \')
;;  (\' \' \' \' \')
;;  (\' \' \' \' \')
;;  (\' "1" "1" "1" "1")
;;  (\' "1" "1" "1" "1")
;;  (\' "1" "1" "1" "1")
;;  (\' "1" "1" "1" "1"))

(defn make-fabrics
  "{ids :ids x :x y :y w :w h :h} 형식의 옵션들을 입력으로 받아 패브릭을 만듭니다."
  [options]
  (->> options
       (map (fn [{ids :ids x :x y :y w :w h :h}]
              (->> ids
                   (map #(make-fabric {:id % :x x :y y :w w :h h})))))
       (apply concat)))

(comment
  (->> ["#1 @ 1,3: 4x4"
        "#2 @ 3,1: 4x4"
        "#3 @ 5,5: 2x2"]
       (map parse-input)
       make-fabrics))
;; (((\' \' \' \' \')
;;   (\' \' \' \' \')
;;   (\' \' \' \' \')
;;   (\' "1" "1" "1" "1")
;;   (\' "1" "1" "1" "1")
;;   (\' "1" "1" "1" "1")
;;   (\' "1" "1" "1" "1"))
;;  ((\' \' \' \' \' \' \')
;;   (\' \' \' "2" "2" "2" "2")
;;   (\' \' \' "2" "2" "2" "2")
;;   (\' \' \' "2" "2" "2" "2")
;;   (\' \' \' "2" "2" "2" "2"))
;;  ((\' \' \' \' \' \' \')
;;   (\' \' \' \' \' \' \')
;;   (\' \' \' \' \' \' \')
;;   (\' \' \' \' \' \' \')
;;   (\' \' \' \' \' \' \')
;;   (\' \' \' \' \' "3" "3")
;;   (\' \' \' \' \' "3" "3")))



(defn zip
  "순차적으로 두 컬렉션을 묶습니다."
  [coll1 coll2] (map vector coll1 coll2))

(defn zip-2d-array
  "2차원 배열을 묶습니다."
  [coll1 coll2]
  (zip coll1 coll2)
  (map (fn [x] (zip (first x)(last x)))))

(comment
  (zip-2d-array '((\' \' \' \' \')
                  (\' \' \' \' \')
                  (\' \' \' \' \')
                  (\' "1" "1" "1" "1")
                  (\' "1" "1" "1" "1")
                  (\' "1" "1" "1" "1")
                  (\' "1" "1" "1" "1"))

                '((\' \' \' \' \' \' \')
                  (\' \' \' "2" "2" "2" "2")
                  (\' \' \' "2" "2" "2" "2")
                  (\' \' \' "2" "2" "2" "2")
                  (\' \' \' "2" "2" "2" "2"))))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
