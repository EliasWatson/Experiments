(defn tuple-shift-up [data index]
	(tuple
		;(tuple/slice data 0 index)
		(get data (+ index 1))
		(get data index)
		;(tuple/slice data (+ index 2))))

(defn sort-pair [data index]
	(if (> (get data index) (get data (+ index 1)))
		(tuple-shift-up data index)
		data))

(defn sort-pass [data index]
	(if (< index (length data))
		(sort-pass (sort-pair data index) (+ index 1))
		data))

(defn bubble-sort [data]
	(def this-pass (sort-pass data 0))
	(if (= this-pass data)
		data
		(bubble-sort this-pass)))

(def my-tuple [8 2 9 1 4 6 7 9 3])
(pp my-tuple)
(pp (bubble-sort my-tuple))