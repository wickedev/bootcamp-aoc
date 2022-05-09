(ns aoc2018-2
  (:require [utils :refer [read-resource]]))

(def input (read-resource "day2.sample.txt"))

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

;; #################################
;; ###        Refactoring        ###
;; #################################

;; ## Deprecated ##
;; (defn select
;;   [counts] (fn [x]
;;              (->> counts
;;                   (reduce
;;                    (fn
;;                      [prev-cond count]
;;                      (or prev-cond (some #(= count %) x)))
;;                    false))))

;; (comment
;;   (->> [1 3 2 1 1 2 1 3 2 1 1 2 3]
;;        (filter (select #{2 3}))))

(defn counts-in-word
  "문자열 box-id를 받아 반복되는 문자의 카운트를 리스트로 반환 (ex -> (1 2))"
  [box-id; 박스에 적혀진 문자열
   ]
  (->> box-id
       frequencies
       (group-by val)
       keys))

(defn checksum-v2
  "box-ids를 입력으로 받아 정수 체크섬을 반환"
  [box-ids ; 박스에 적혀진 문자열의 콜렉션
   counts-to-check ; 체크해야할 카운트 시퀀스 (ex #{2 3})
   ]
  (->> box-ids
       (map counts-in-word) ; ((1) (3 2 1) (1 2) (1 3) (2 1) (1 2) (3))
       (apply concat) ; (1 3 2 1 1 2 1 3 2 1 1 2 3)
       (filter counts-to-check) ; (3 2 2 3 2 2 3)
       frequencies ; {3 3, 2 4}
       vals ; (3 4)
       (apply *) ; 12
       ))

(comment
  (def sample-data ["abcdef"
                    "bababc"
                    "abbcde"
                    "abcccd"
                    "aabcdd"
                    "abcdee"
                    "ababab"])
  (checksum-v2 sample-data #{2 3})
  (checksum-v2 sample-data #{1})
  (checksum-v2 sample-data #{2})
  (checksum-v2 sample-data #{3}))

(comment
  (checksum-v2 input #{2 3})
  (checksum-v2 input #{1 3})
  (checksum-v2 input #{1 2}))

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

(defn zip
  "순차적으로 두 컬렉션을 묶습니다."
  [coll1 coll2] (map vector coll1 coll2))

(defn intersection
  "문자열 s1 s2 간에 공통 문자열을 반환합니다."
  [s1 s2]
  (let [coll (zip s1 s2)]
    (->> coll
         (map (fn [[a b]]
                (when (= a b)
                  a)))
         (filter some?)
         (apply str))))

(comment (intersection "abc" "acc")) ; "ac"

(defn common-letter-for-box-id
  "box-id를 기준으로 box-ids 안에서 공통 문자를 반환합니다. 공통 문자가 없다면 nil을 반환합니다."
  [box-id; 박스 ID
   box-ids; 박스 ID들
   tolerance ; 허용치; 예를들어 0이라면 완전 일치, 1이라면 하나 만큼 다른 것을 허용
   ]
  (let [curr (first box-ids)
        common-letters (intersection box-id curr)
        matched? (= (count common-letters) (- (count box-id) tolerance))]
    #_(println :common-letter-for-box-id :box-id box-id :box-ids box-ids :next-box-id curr :common common-letters)
    (cond
      matched? common-letters
      (seq (rest box-ids)) (recur box-id (rest box-ids) tolerance)
      :else nil)))

(comment
  (common-letter-for-box-id "fghij" ["klmno"
                                     "pqrst"
                                     "axcye"
                                     "wvxyz"
                                     "fguij"] 1) ; "fgij"
  (common-letter-for-box-id "fghij" ["klmno"
                                     "pqrst"
                                     "axcye"
                                     "wvxyz"] 1) ; nil
  )

(println "h")

(defn common-letter-between-box-ids
  "box-ids 간에 첫번째 공통 문자열를 반환합니다. 공통 문자가 없다면 nil을 반환합니다."
  [box-ids; 박스 ID들
   tolerance ; 허용치; 예) 0이라면 완전 일치, 1이라면 하나 만큼 다른 것을 허용
   ]
  (let [curr (first box-ids)
        rest-box-ids (rest box-ids)
        common-letters (common-letter-for-box-id curr rest-box-ids tolerance)]
    #_(println :common-letter-between-box-ids :curr curr :next-box-id rest-box-ids :common common-letters)
    (cond
      (nil? (next rest-box-ids)) nil
      (nil? common-letters) (recur rest-box-ids tolerance)
      :else common-letters)))

(comment
  (common-letter-between-box-ids  ["abcde"
                                   "fghij"
                                   "klmno"
                                   "pqrst"
                                   "fguij"
                                   "axcye"
                                   "wvxyz"] 1) ; "fgij"
  (common-letter-between-box-ids  ["klmno"
                                   "pqrst"
                                   "axcye"
                                   "wvxyz"] 1) ; nil
  (common-letter-between-box-ids input 1) ; "vtnikorkulbfejvyznqgdxpaw"
  )

;; # 한타 2022.05.06
;; todo
;; 1. 스레딩 매크로 상에서는 trace를 사용해 디버깅, recur에서 trace를 쉽게 하는 방법이 있을까요?
;;     - ex. 디버거, IDEA 라이브 템플릿
;;     - A. tap을 사용해 볼 것
;; 2. 현재 재귀로 풀었지만 2중 반복의 경우 함수 체이닝으로도 풀 수 있는 방법이 있을까요?
;;     - https://github.com/clojure/math.combinatorics

;; #################################
;; ###        Refactoring        ###
;; #################################
