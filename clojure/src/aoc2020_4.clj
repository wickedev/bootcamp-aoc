(ns aoc2020-4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.core.match :refer [match]]))

(def batch-file (-> "aoc.2020.day4.sample.txt"
                    io/resource
                    slurp
                    (#(str/split % #"\n\n+"))))

;; # Day 4

;; [https://adventofcode.com/2020/day/4](https://adventofcode.com/2020/day/4)

;; ## 파트 1
;; 여권이 유효한지 판단하려고 한다. 여권에는 다음과 같은 필드가 있음.
;; - byr (Birth Year)
;; - iyr (Issue Year)
;; - eyr (Expiration Year)
;; - hgt (Height)
;; - hcl (Hair Color)
;; - ecl (Eye Color)
;; - pid (Passport ID)
;; - cid (Country ID)

;; 파트 1에서는 여권의 모든 필드가 존재하는지의 여부를 검사한다. 주어진 입력에서 '유효한' 여권의 숫자를 반환하여라.

;; ```
;; ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
;; byr:1937 iyr:2017 cid:147 hgt:183cm

;; iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
;; hcl:#cfa07d byr:1929

;; hcl:#ae17e1 iyr:2013
;; eyr:2024
;; ecl:brn pid:760753108 byr:1931
;; hgt:179cm

;; hcl:#cfa07d eyr:2025 pid:166559648
;; iyr:2011 ecl:brn hgt:59in
;; ```

;; - 첫번째는 유효한 여권이다. 8개의 필드가 전부 존재한다.
;; - 두번째는 유효하지 않다. hgt가 없기 때문.
;; - 세번째는 cid가 없지만, ** cid는 없어도 되는 ** 것으로 간주한다. 그래서 유효하다.
;; - 네번째는 cid와 byr이 없다. byr은 반드시 있어야하는 필드이므로 유효하지 않다.

(def passport-data-pattern #"([^\s\n]+):([^\s\n]+)")

(defn parse-passport
  ":로 구분된 \"ecl:gry pid:860033327\" 배치 파일을 읽어서
   {:ecl \"gry\", :pid \"860033327\"} 형식으로 반환"
  [batch-file]
  (->> batch-file
       (re-seq passport-data-pattern)
       (map (fn [[_ k v]] {(keyword k) v}))
       (into {})))

(s/def :simple-passport/byr some?)

(s/def :simple-passport/iyr some?)

(s/def :simple-passport/eyr some?)

(s/def :simple-passport/hgt some?)

(s/def :simple-passport/hcl some?)

(s/def :simple-passport/ecl some?)

(s/def :simple-passport/pid some?)

(s/def :simple-passport/cid some?)

(s/def ::questionable-passport (s/keys :req-un [:simple-passport/byr
                                                :simple-passport/iyr
                                                :simple-passport/eyr
                                                :simple-passport/hgt
                                                :simple-passport/hcl
                                                :simple-passport/ecl
                                                :simple-passport/pid]
                                       :opt-un [:simple-passport/cid]))

(defn solve-4-1 [inputs]
  (->> inputs
       (map parse-passport)
       (filter #(s/valid? ::questionable-passport %))
       count))

(comment
  (parse-passport "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm")
  (solve-4-1 ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
              "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
              "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
              "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"])
  (solve-4-1 batch-file))

;; ## 파트 2
;; 파트1에서는 필드의 유무만을 검사했다면, 파트2에서는 구체적인 범위가 주어진다.
;; - byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
;; - iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
;; - eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
;; - hgt (Height) - 마지막에 cm 혹은 in이 오는 숫자:
;; - cm의 경우, 숫자는 최소 150 & 최대 193.
;; - in의 경우, 숫자는 최소 59 & 최대 76.
;; - hcl (Hair Color) - #뒤에 오는 정확히 6개의 캐릭터 0-9 혹은 a-f.
;; - ecl (Eye Color) - 정확히 amb blu brn gry grn hzl oth 중 하나.
;; - pid (Passport ID) - 처음 0을 포함하는 9자리 숫자.
;; - cid (Country ID) - 없어도 됨.

;; 아래는 예시들이다.
;; ```
;; byr valid:   2002
;; byr invalid: 2003

;; hgt valid:   60in
;; hgt valid:   190cm
;; hgt invalid: 190in
;; hgt invalid: 190

;; hcl valid:   #123abc
;; hcl invalid: #123abz
;; hcl invalid: 123abc

;; ecl valid:   brn
;; ecl invalid: wat

;; pid valid:   000000001
;; pid invalid: 0123456789
;; ```
;; 모든 필드의 기준에 맞는 여권의 수를 반환하여라.

(defn valid-height?
  "주어진 문자열이 in 혹은 cm으로 끝나는 정수 문자열일 때
   cm인 경우 150 에서 193 사이거나
   in인 경우 59 에서 76 사이인 경우 참
   그렇지 않은 경우 거짓을 반환"
  [s]
  (let [[_ height unit] (re-find #"^(\d+)(in|cm)?$" s)]
    (if (some? height)
      (let [size (Integer/parseInt height)]
        (match [size unit]
          [(true :<< #(<= 150 % 193)) "cm"] true
          [(true :<< #(<= 59 % 76)) "in"] true
          :else false))
      false)))

(s/def :strict-passport/byr (s/and some? string? #(<= 1920 (Integer/parseInt %) 2002)))

(s/def :strict-passport/iyr (s/and some? string? #(<= 2010 (Integer/parseInt %) 2020)))

(s/def :strict-passport/eyr (s/and some? string? #(<= 2020 (Integer/parseInt %) 2030)))

(s/def :strict-passport/hgt (s/and some? string? valid-height?))

(s/def :strict-passport/hcl (s/and some? string? #(re-matches #"^#[0-9a-f]{6}$" %)))

(s/def :strict-passport/ecl (s/and some? string? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}))

(s/def :strict-passport/pid (s/and some? string? #(re-matches #"^\d{9}$" %)))

(s/def :strict-passport/cid (constantly true))

(s/def ::questionable-strict-passport (s/keys :req-un [:strict-passport/byr
                                                       :strict-passport/iyr
                                                       :strict-passport/eyr
                                                       :strict-passport/hgt
                                                       :strict-passport/hcl
                                                       :strict-passport/ecl
                                                       :strict-passport/pid]
                                              :opt-un [:strict-passport/cid]))

(defn solve-4-2 [inputs]
  (->> inputs
       (map parse-passport)
       (filter #(s/valid? ::questionable-strict-passport %))
       count))

(comment
  (solve-4-2 ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
              "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
              "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
              "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in"])
  (solve-4-2 batch-file))
