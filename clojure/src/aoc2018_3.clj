(ns aoc2018_3
  (:require [clojure.set :as set :refer (difference)]
            [utils :refer [read-resource]]))

(def input (read-resource "aoc2018_3_sample.txt"))


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

(def claim-pattern #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)")

(defn parse-claim
  "#1 @ 1,3: 4x4 형식의 claim을 파싱"
  [input]
  (->> input
       (re-find claim-pattern)
       rest
       (map read-string)
       (zipmap [:id :x :y :w :h])))

(defn parse-claims [claims] (map parse-claim claims))

(defn make-fabric
  "claim으로 부터 fabric을 만듭니다."
  [{:keys [id x y w h]}]
  (let [width (+ x w)
        height (+ y h)]
    (for [x (range x width), y (range y height)]
      {:id id :x x :y y})))

(defn collapse-tiles
  "좌표 중복 없이 연관된 id 리스트를 함께 반환합니다. ex) {:x 3, :y 3} (1 2)"
  [tiles]
  (reduce (fn [s x]
            (let [id (get x :id)
                  xy (select-keys x [:x :y])
                  ids (get s xy '())]
              (assoc s xy (conj ids id))))
          {} tiles))

(defn overlap?
  "ids 카운트가 1 이상이면 겹쳐있다 판단하여 참을 반환"
  [[_ ids]] (< 1 (count ids)))

(defn collapse-fabrics
  "좌표 중복 없이 연관된 id 리스트를 함께 반환합니다. ex) {:x 3, :y 3} (1 2)"
  [fabrics]
  (->> fabrics
       (reduce into)
       collapse-tiles))

(defn solve-3-1
  [inputs]
  (->> inputs
       parse-claims
       (map make-fabric)
       collapse-fabrics
       (filter overlap?)
       count))

(comment
  (solve-3-1 ["#1 @ 1,3: 4x4"
              "#2 @ 3,1: 4x4"
              "#3 @ 5,5: 2x2"])
  (solve-3-1 input))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn ids-from
  "claim들의 :id들을 set으로 반환합니다. #{1}
   claims ({:id 1, :x 1, :y 3, :w 4, :h 4} ...) 형식의 리스트"
  [claims]
  (->> claims
       (map #(get % :id))
       set))

(defn overlaped-ids-from
  "fabirc들에서 겹쳐지는 id들을 반환"
  [fabrics]
  (->> fabrics
       collapse-fabrics
       (filter overlap?)
       (map second)
       (apply concat)
       set))

(defn solve-3-2
  [inputs]
  (let [claims (parse-claims inputs)
        fabrics (map make-fabric claims)
        ids (ids-from claims)
        overlap-ids (overlaped-ids-from fabrics)]
    (first (difference ids overlap-ids))))

(comment
  (solve-3-2 ["#1 @ 1,3: 4x4"
              "#2 @ 3,1: 4x4"
              "#3 @ 5,5: 2x2"])
  (solve-3-2 input))
