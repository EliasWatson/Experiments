(import ./lru_cache :as lru)

(defmacro string-tail [s]
  ~(string/slice ,s 1))

(lru/memo-fn lev 100 [a b]
  (cond
    (zero? (length a)) (length b)
    (zero? (length b)) (length a)
    (= (first a) (first b)) (lev (string-tail a) (string-tail b))
    (inc (min
      (lev (string-tail a) b)
      (lev a (string-tail b))
      (lev (string-tail a) (string-tail b))))))

(print (lev "kitten" "sitting"))

# (pp lev-lru-cache)
(def hit-percent (lru/hit-percentage lev-lru-cache))
(printf "Hit: %d%%" (* hit-percent 100))
