(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-resource [s] (-> s
                            (io/resource)
                            (slurp)
                            (string/split-lines)))