(ns om.test-utils
  (:require [clojure.string :as str]))

(defn remove-whitespace [s]
  (str/replace s #"(>)\s+(<)" "$1$2"))
