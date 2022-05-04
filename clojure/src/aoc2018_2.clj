(ns aoc2018-2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (-> "day2.sample.txt"
               (io/resource)
               (slurp)
               (str/split-lines)))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

;; 관용적으로 자주 사용되는 함수들에 익숙하지 않음. 빈도가 높은 함수들?
;; 문법 에러 빈도가 높아서 속도가 잘 안나옴
;; 파이프 중간에 함수를 커맨트 하는 방법
;; juxt
;; docstring 있으면 좋다

(defn twice-n-triple-char-count
  "문자열 box-id를 받아 2, 3번 반복되는 문자의 카운트를 맵으로 반환 (ex {2 1, 3 1} 2번 1개 3번 1개)"
  [box-id; 박스에 적혀진 문자열
   ]
  (->> box-id
       frequencies
       (group-by val)
       (#(update-vals % count))
       (#(select-keys % [2 3]))))

; 1차 솔루션

(defn checksum
  "box-ids를 입력으로 받아 정수 체크섬을 반환"
  [box-ids ; 박스에 적혀진 문자열의 시퀀스
   ]
  (->> box-ids
       (map twice-n-triple-char-count)
       (filter not-empty)
       (reduce (fn [m v]
                 (let [twice (if (v 2) 1 0)
                       tripe (if (v 3) 1 0)]
                   (-> m
                       (update :twice (fn [v] (+ v twice)))
                       (update :triple (fn [v] (+ v tripe))))))
               {:twice 0 :triple 0})
       (reduce (fn [acc x] (* acc (last x))) 1)))

(comment
  (checksum ["abcdef"
             "bababc"
             "abbcde"
             "abcccd"
             "aabcdd"
             "abcdee"
             "ababab"]))

(comment (checksum input))

;; (defn count-duplicate
;;   "((3 2 1) (1 2) (1 3) (2 1) (1 2) (3))
;;    #{2 3}"
;;   [count-seq times]
;;   (->> count-seq
;;        (map (fn [x]
;;               (println x)
;;               (filter (select [2 3])   x)))))

;; (comment
;;   (count-duplicate '('(3 2 1) '(1 2) '(1 3) '(2 1) '(1 2) '(3))  #{2 3}))

;; WIP

(defn counts-in-word
  [box-id]
  (->> box-id
       frequencies
       (group-by val)
       keys))

(defn select [keyseq] (fn [x]
                        (->> keyseq
                             (reduce
                              (fn
                                [acc v]
                                (or acc (some #(= v %) x)))
                              false))))

(comment
  (->> ["abcdef"
        "bababc"
        "abbcde"
        "abcccd"
        "aabcdd"
        "abcdee"
        "ababab"]
       (map counts-in-word)
       (filter (select #{2 3}))))

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
