(ns om.checksums
  (:require [clojure.string :as str]))

;; ===================================================================
;; Checksums (data-react-checksum)

(def MOD 65521)

;; Adapted from https://github.com/tonsky/rum
(defn adler32 [^StringBuilder sb]
  (let [l (.length sb)
        m (bit-and l -4)]
    (loop [a (int 1)
           b (int 0)
           i 0
           n (min (+ i 4096) m)]
      (cond
        (< i n)
        (let [c0 (int (.charAt sb i))
              c1 (int (.charAt sb (+ i 1)))
              c2 (int (.charAt sb (+ i 2)))
              c3 (int (.charAt sb (+ i 3)))
              b  (+ b a c0
                   a c0 c1
                   a c0 c1 c2
                   a c0 c1 c2 c3)
              a  (+ a c0 c1 c2 c3)]
          (recur (rem a MOD) (rem b MOD) (+ i 4) n))

        (< i m)
        (recur a b i (min (+ i 4096) m))

        (< i l)
        (let [c0 (int (.charAt sb i))]
          (recur (+ a c0) (+ b a c0) (+ i 1) n))

        :else
        (let [a (rem a MOD)
              b (rem b MOD)]
          (bit-or (int a) (unchecked-int (bit-shift-left b 16))))))))

(defn assign-react-checksum [^StringBuilder sb]
  (.insert sb (.indexOf sb ">") (str " data-react-checksum=\"" (adler32 sb) "\"")))
