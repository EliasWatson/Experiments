(defn new [size] @{
  :cache @{}
  :counter 0
  :size size
  :lookups 0
  :hits 0})

(defn lookup [lru key]
  (set (lru :lookups) (inc (lru :lookups)))
  (def entry ((lru :cache) key))
  (if (nil? entry)
    nil
    (do
      (set (lru :hits) (inc (lru :hits)))
      (set (entry :counter) (lru :counter))
      (set (lru :counter) (inc (lru :counter)))
      (entry :val))))

(defn find-lru [lru]
  (first (reduce2
    (fn [accum el]
      (if (< ((get el 1) :counter) ((get accum 1) :counter))
        el
        accum))
    (pairs (lru :cache)))))

(defn shrink [lru]
  (when (> (length (lru :cache)) (lru :size))
    (def key (find-lru lru))
    (set ((lru :cache) key) nil)))

(defn insert [lru key val]
  (put (lru :cache) key @{
    :val val
    :counter (lru :counter)})
  (set (lru :counter) (inc (lru :counter)))
  (shrink lru))

(defn hit-percentage [lru]
  (if (> (lru :lookups) 0)
    (/ (lru :hits) (lru :lookups))
    0))

(defmacro memo-fn [fn-name cache-size args & body]
  (def cache-symbol (symbol (string/format "%s-lru-cache" (string fn-name))))
  ~(upscope
    (def ,cache-symbol (,new ,cache-size))
    (defn ,fn-name ,args
      (def cache-key [,;args])
      (def cache-val (,lookup ,cache-symbol cache-key))
      (if (not (nil? cache-val))
        cache-val
        (do
          (def return-value (do ,;body))
          (,insert ,cache-symbol cache-key return-value)
          return-value)))))


## Testing
# (memo-fn test-memo 3 [a b c]
#   (+ a b c))

# (pp (test-memo 1 2 3))
# (pp (test-memo 1 2 3))
# (pp (test-memo 4 5 6))
# (pp (test-memo 1 2 3))
# (pp (test-memo 4 5 6))
# (pp test-memo-lru-cache)
# (pp (hit-percentage test-memo-lru-cache))
