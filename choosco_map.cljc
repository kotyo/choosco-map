(ns choosco-map)

(defn choosco-map-by
  "Creates a new choosco-map with the `cmp` comparator function."
  [cmp]
  (with-meta [] {:cmp cmp}))

(defn choosco-map
  "Cerates a new choosco-map with the default comparator."
  []
  (choosco-map-by compare))

(defn- >key [e]   (nth e 0 nil))
(defn- >cnt [e]   (nth e 1 0))
(defn- >value [e] (nth e 2 nil))
(defn- >left [e]  (nth e 3 nil))
(defn- >right [e] (nth e 4 nil))

(defn cnt
  "Returns the number of elements in the map."
  [tree]
  (>cnt tree))

(defn- incif [c num]
  (if c (inc num) num))

(defn- decif [c num]
  (if c (dec num) num))

(defn retrieve
  "Returns a [key, index, value] triplet for a given key, or nil if the key doesn't exists."
  [tree k]
  ((fn retrieve-core [cmp node k pidx]
     (when-let [key (>key node)]
       (let [idx (+ pidx (- (cnt node) (cnt (>right node))))]
         (cond
           (neg? (cmp k key)) (retrieve-core cmp (>left node) k pidx)
           (pos? (cmp k key)) (retrieve-core cmp (>right node) k idx)
           :else [key (dec idx) (>value node)]))))
   (-> tree meta :cmp) tree k 0))

(defn retrieve-nth
  "Returns a [key, index, value] triplet for a given index, or nil if the index doesn't exists."
  [tree n]
  (let [res
        ((fn retrieve-nth-core [node n]
           (let [k (>key node)
                 nth (- (cnt node) (cnt (>right node)) 1)]
             (cond
               (nil? k) nil
               (< n nth) (retrieve-nth-core (>left node) n)
               (> n nth) (retrieve-nth-core (>right node) (dec (- n nth)))
               :else [(>key node) (>value node)])))
         tree n)]
    (when-let [[k v] res]
      [k n v])))

(defn- rebalance [key offset value left right]
  (let [cnt-diff (- (cnt left) (cnt right))]
    (cond
      (< cnt-diff -1) [(>key right) offset (>value right)
                       [key (+ 1 (cnt left) (cnt (>left right))) value left (>left right)]
                       (>right right)]
      (> cnt-diff 1) [(>key left) offset (>value left)
                      (>left left)
                      [key (+ 1 (cnt (>right left)) (cnt right)) value (>right left) right]]
      :else [key offset value left right])))

(defn- insert-core [cmp node k v pidx]
  (let [[key offset value left right] node
        k-diff (when key (cmp k key))
        idx (+ pidx (- (cnt node) (cnt right)))]
    (cond
      (nil? k-diff) [true [k 1 v nil nil] idx]
      (neg? k-diff) (let [[new? new-left iidx] (insert-core cmp left k v pidx)]
                      [new? (rebalance key (incif new? offset) value new-left right) iidx])
      (pos? k-diff) (let [[new? new-right iidx] (insert-core cmp right k v idx)]
                      [new? (rebalance  key (incif new? offset) value left new-right) iidx])
      :else [false [k offset v left right] idx])))

(defn insert
  "Inserts or updates an element in a choosko-map `m` under the given `k` key with the specified `v` value.
   Returns with the updated map."
  [m k v]
  (-> (insert-core (-> m meta :cmp) m k v 0)
      (nth 1)
      (with-meta (-> m meta))))

(defn insert-ext
  "Inserts or updates an element in a choosko-map `m `under the given `k `key with the specified `v `value.
   Returns with a vector with a content like: [updated-map [k idx v] new?]."
  [m k v]
  (let [m-meta (-> m meta)
        res (insert-core (:cmp m-meta) m k v 0)]
    [(with-meta (nth res 1) m-meta) [k (nth res 2) v] (nth res 0)]))

(defn- min-val [node]
  (if-let [left (>left node)]
    (min-val left)
    node))

(defn- delete-core [cmp node k pidx]
  (let [[key offset value left right] node
        k-diff (when key (cmp k key))
        idx (+ pidx (- (cnt node) (cnt right)))]
    (cond
      (nil? k-diff) [false nil -1 nil]
      (neg? k-diff) (let [[deleted? new-left iidx v] (delete-core cmp left k pidx)]
                      [deleted? (rebalance key (decif deleted? offset) value new-left right) iidx v])
      (pos? k-diff) (let [[deleted? new-right iidx v] (delete-core cmp right k idx)]
                      [deleted? (rebalance key (decif deleted? offset) value left new-right) iidx v])
      (nil? left) [true right idx value]
      (nil? right) [true left idx value]
      :else [true (let [m (min-val right)
                        key (>key m)]
                    (rebalance key (dec offset) (>value m) left
                               (nth (delete-core cmp right key 0) 1))) idx value])))

(defn delete
  "Deletes an existing entry from the `m` choosco-map selected with a `k` key.
   Returns with the new choosco-map."
  [m k]
  (-> (delete-core (-> m meta :cmp) m k 0)
      (nth 1)
      (with-meta (-> m meta))))

(defn delete-ext
  "Deletes an existing entry from the `m` choosco-map selected with a `k` key.
   Returns with a vector with a content like: 
   [updated-map [deleted-key deleted-idx deleted-value] deleted?]"
  [m k]
  (let [m-meta (-> m meta)
        [deleted? res idx v] (delete-core (:cmp m-meta) m k 0)]
    [(with-meta res m-meta) [k (dec idx) v] deleted?]))

(defn walk
  "Returns a lazy seqence of [key offset value] triplets in the `m` choosko-map.
   Optionally you can specify a direction and a starting key. Every returned key should 
   be >= or <= than the specified one depending on the order."
  ([m] (walk m true))
  ([m asc?] (walk m asc? nil))
  ([m asc? start]
   ((fn walk-core [cmp node pidx s]
      (when-let [[key offset value left right] node]
        (let [idx (+ pidx (- offset (cnt right)))
              k-pass? (or (nil? start) ((if asc? >= <=) 0 (cmp start key)))]
          (lazy-seq
           (if asc?
             (cond->> s
               right (walk-core cmp right idx)
               k-pass? (cons [key (dec idx) value])
               (and k-pass? left) (walk-core cmp left pidx))
             (cond->> s
               left (walk-core cmp left pidx)
               k-pass? (cons [key (dec idx) value])
               (and k-pass? right) (walk-core cmp right idx)))))))
    (-> m meta :cmp) m 0 nil)))

(defn walk-nth
  "Returns a lazy seqence of [key offset value] triplets in the `m` choosco-map.
   You have to specify a direction and a starting index. Every returned index should 
   be >= or <= than the specified one depending of the order."
  [m asc? start-idx]
  ((fn walk-nth-core [node pidx s]
     (when-let [[key offset value left right] node]
       (let [idx (+ pidx (- offset (cnt right)))
             k-pass? ((if asc? <= >=) start-idx (dec idx))]
         (lazy-seq
          (if asc?
            (cond->> s
              right (walk-nth-core right idx)
              k-pass? (cons [key (dec idx) value])
              (and k-pass? left) (walk-nth-core left pidx))
            (cond->> s
              left (walk-nth-core left pidx)
              k-pass? (cons [key (dec idx) value])
              (and k-pass? right) (walk-nth-core right idx)))))))
   m 0 nil))
