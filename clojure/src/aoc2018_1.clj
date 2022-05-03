(ns aoc2018-1
  (:require [clojure.java.io :as io]))

(def input (-> "day1.sample.txt"
               (io/resource)
               (slurp)
               (clojure.string/split-lines)))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

(defn parseInt [str-nums] (map  #(Integer/parseInt %) str-nums))


;; PPAP (parse, process, aggregate, print)
;; parse -> 알고리즘을 적용하기 전에 인풋값을 전처리하는 행위
;; process -> 문제를 풀기위한 알고리즘을 적용하는 행위
(->> input
     (map #(Integer/parseInt %))
     (reduce +))

(reduce + (parseInt input))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(defn first-dupulicate-number
  "numbers 앞에서부터 더하여 2번 이상 동일한 결과값을 내는 첫번째 결과를 반환"
  [numbers ; 더해야 할 숫자의 목록
   acc ; 값을 가산하기 위한 
   prev-results ; 이전 합산 결과를 저장하기 위한
   ]
  (let [curr (first numbers)
        rest-numbers (rest numbers)
        result (if (nil? acc) curr (+ acc curr))]
    (if (contains? prev-results result)
      result
      (first-dupulicate-number rest-numbers result (if (nil? acc) prev-results (conj prev-results result))))))

(first-dupulicate-number (cycle [+1, -1]) nil (set '()))
(first-dupulicate-number (cycle [+3, +3, +4, -2, -4]) nil (set '()))
(first-dupulicate-number (cycle [-6, +3, +8, +5, -6]) nil (set '()))
(first-dupulicate-number (cycle [+7, +7, -2, -7, -4]) nil (set '()))

; FIXME: Evalute 불가
(defn first-dupulicate-number-with-loop
  "numbers 앞에서부터 더하여 2번 이상 동일한 결과값을 내는 첫번째 결과를 반환"
  [numbers ; 더해야 할 숫자의 목록
   sum ; 값을 가산하기 위한  0
   seen? ; 이전 합산 결과를 저장하기 위한 #{}
   ]
   (if (seen? sum)
      sum
      (let [sum' (+ sum (first numbers))
            seen?' (conj seen? sum)]
            (recur sum' seen?' (next numbers)))))


(comment
 (first-dupulicate-number-with-loop (cycle [1, -1]) 0 #{})
;;  (first-dupulicate-number-with-loop (cycle [+3, +3, +4, -2, -4]) 0 #{})
;;  (first-dupulicate-number-with-loop (cycle [-6, +3, +8, +5, -6]) 0 #{})
;;  (first-dupulicate-number-with-loop (cycle [+7, +7, -2, -7, -4]) 0 #{})
;;  (first-dupulicate-number-with-loop (cycle (parseInt input)) nil (set '()))
)


