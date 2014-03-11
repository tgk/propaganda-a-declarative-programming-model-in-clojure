(use 'propaganda.system
     'propaganda.values
     'propaganda.generic-operators)

(require '[clojure.set :refer [intersection union difference]])

;; ---------------------------------------------
;; All the setup for supporting sets and generic
;; mathematical operations

(defn check-intersection
  [s1 s2]
  (let [i (intersection s1 s2)]
    (if (seq i)
      (if (= 1 (count i))
        (first i)
        i)
      (contradiction
       (format "Intersection of %s and %s is empty" s1 s2)))))

(defn check-in-set
  [e s]
  (if (contains? s e)
    e
    (contradiction
     (format "%s is not in %s" e s))))

(defn check-map-intersection
  [m1 m2]
  (let [ks (seq (intersection (set (keys m1))
                              (set (keys m2))))]
    ;; should merge the values, not just check for equality (requires
    ;; reference to merge and contradiction?)
    (if (= (select-keys m1 ks) (select-keys m2 ks))
      (merge m1 m2)
      (contradiction
       (format "Maps %s and %s differ on one or more keys" m1 m2)))))

(defn extend-merge
  [merge]
  (doto merge
    (assign-operation
     (fn [content increment]
       (check-in-set increment content))
     set? any?)
    (assign-operation
     (fn [content increment]
       (check-in-set content increment))
     any? set?)
    (assign-operation
     check-intersection
     set? set?)
    (assign-operation
     check-map-intersection
     map? map?)))

(defn generic-set-operator
  [op]
  (doto (generic-operator op)
    (assign-operation
     (fn [s v] (into #{} (for [elm s] (op elm v))))
     set? any?)
    (assign-operation
     (fn [v s] (into #{} (for [elm s] (op v elm))))
     any? set?)
    (assign-operation
     (fn [s1 s2] (into #{} (for [e1 s1 e2 s2] (op e1 e2))))
     set? set?)))

(def plus (generic-set-operator +))
(def minus (generic-set-operator -))
(def multiply (generic-set-operator *))
(def divide (generic-set-operator /))

(def plusp (function->propagator-constructor plus))
(def minusp (function->propagator-constructor minus))
(def multiplyp (function->propagator-constructor multiply))
(def dividep (function->propagator-constructor divide))

(defn sum
  "a + b = c"
  [system a b c]
  (-> system
      (plusp a b c)
      (minusp c a b)
      (minusp c b a)))

(defn minus-sum
  "a - b = c"
  [system a b c]
  (-> system
      (minusp a b c)
      (minusp a c b)
      (plusp  c b a)))

(defn prod
  "a x b = c"
  [system a b c]
  (-> system
      (multiplyp a b c)
      (dividep c a b)
      (dividep c b a)))

;; Code for setting up relation between cell with map and cell with
;; "simple" value

(defn mkvp
  "Map-key-value propagator will make sure the key k under map in cell m
  is the value under cell v"
  [system m k v]
  (-> system
      ;; probably easier if k is also a cell
      ((function->propagator-constructor (fn [m] (get m k nothing))) m v)
      ((function->propagator-constructor (fn [v] {k v})) v m)))

;; -------------------------------------------------------------
;; Demo

(def base-system
  (let [m (doto (default-merge)
            extend-merge)
        c (default-contradictory?)]
    (make-system m c)))

(-> base-system
    (sum :a :b :c)   ;; install propagators
    (add-value :a 1) ;; put a value in there
    (add-value :b 2) ;; and another one
    (get-value :c))

(-> base-system
    (sum :a :b :c)
    (add-value :a 1)
    (add-value :c 6) ;; let's specify the result
    (get-value :b))

(-> base-system
    (add-value :a 1)
    (sum :a :b :c)   ;; order isn't important
    (add-value :c 6)
    (get-value :b))

;; detects inconsistencies
(try (-> base-system
         (add-value :a 1)
         (sum :a :b :c)
         (add-value :c 6)
         (add-value :b 4))
     (catch Exception e e))




(-> base-system
    (sum :a :b :c) ;; I've extended the definition of sum with sets
    (add-value :a #{1 2 3})
    (add-value :b #{5 6 7})
    (get-value :c))

(-> base-system
    (sum :a :b :c) ;; I've also extended the definition of merge
    (add-value :a #{1 2 3})
    (add-value :b #{5 6 7})
    (add-value :c #{0 2 4 6 8 10})
    (get-value :c))

;; we still have inconsistencies
(try (-> base-system
             (sum :a :b :c) ;; I've also extended the definition of merge
             (add-value :a #{1 2 3})
             (add-value :b #{5 6 7})
             (add-value :c #{0 42})
             (get-value :c))
     (catch Exception e e))




(-> base-system
    (mkvp :m "a" :a) ;; "map-key-value-propagator"
    (mkvp :m "b" :b) ;; allows us to put maps in cells
    (add-value :a 42)
    (add-value :b 43)
    (get-value :m))

(-> base-system
    (mkvp :m "a" :a)
    (mkvp :m "b" :b)
    (mkvp :m "c" :c)
    (sum :a :b :c)
    (add-value :a #{1 2 3})
    (add-value :b 4)
    (get-value :m))

(-> base-system
    (mkvp :m "a" :a)
    (mkvp :m "b" :b)
    (mkvp :m "c" :c)
    (sum :a :b :c)
    (add-value :m {"a" 4 "c" 42})
    (get-value :m))

(-> base-system
    (mkvp :m "a" :a)
    (mkvp :m "b" :b)
    (mkvp :m "c" :c)
    (sum :a :b :c)
    (add-value :m {"a" 4 "c" #{10 42}})
    (get-value :m))

(defn do-what-i-want-with-map
  [m]
  (-> base-system
      (mkvp :m "a" :a)
      (mkvp :m "b" :b)
      (mkvp :m "c" :c)
      (sum :a :b :c)
      (add-value :m m)
      (get-value :m)))

(do-what-i-want-with-map
 {"a" 10})

;; ---------------------------------------------
;; Personal projection

(defn planp
  [system standing-charge unit-rate discounts plan]
  (-> system
      (mkvp plan :standing-charge standing-charge)
      (mkvp plan :unit-rate       unit-rate)
      (mkvp plan :discounts       discounts)))

(defn personal-projection
  ;; "destructing" - ignore this bit
  ([system usage plan spend]
     (let [[standing-charge unit-rate discounts]
           (repeatedly gensym)]
       (-> system
           (planp
            standing-charge unit-rate discounts plan)
           (personal-projection
            usage standing-charge unit-rate discounts spend))))
  ;; calculation
  ([system usage standing-charge unit-rate discounts spend]
     (let [consumption-charges (gensym "consumption-charges")
           costs               (gensym "costs")]
       (-> system
           (prod usage unit-rate consumption-charges)
           (sum standing-charge consumption-charges costs)
           (minus-sum costs discounts spend)))))

;; Let's find your spend
(-> base-system
    (personal-projection :usage :plan :spend)
    (add-value :plan {:standing-charge (/ 70 100)
                      :unit-rate       (/ 34 100)
                      :discounts                0})
    (add-value :usage 3300)
    (get-value :spend)
    float)

;; Let's find your usage
(-> base-system
    (personal-projection :usage :plan :spend)
    (add-value :plan {:standing-charge (/ 70 100)
                      :unit-rate       (/ 34 100)
                      :discounts                0})
    (add-value :spend 1024)
    (get-value :usage)
    float)

;; Alright, we know your spend and usage, but we don't know your discounts
(-> base-system
    (personal-projection :usage :plan :spend)
    (add-value :plan {:standing-charge (/ 70 100)
                      :unit-rate       (/ 34 100)})
    (add-value :usage 3300)
    (add-value :spend 1024)
    (get-value :plan)
    :discounts
    float)

;; We know you're either paying 0.34 or 0.30 per unit - what are your
;; discounts?
(map
 float
 (-> base-system
     (planp :standing-charge :unit-rate :discounts :plan)
     (personal-projection :usage :plan :spend)
     (add-value :standing-charge               (/ 70 100))
     (add-value :unit-rate       #{(/ 34 100) (/ 30 100)})
     (add-value :usage 3300)
     (add-value :spend 1024)
     (get-value :unit-rate)))

;; Hmmm, negative discounts, are you sure your usage is right? Let's try
;; a different number from your bill
(map
 float
 (-> base-system
     (planp :standing-charge :unit-rate :discounts :plan)
     (personal-projection :usage :plan :spend)
     (add-value :standing-charge               (/ 70 100))
     (add-value :unit-rate       #{(/ 34 100) (/ 30 100)})
     (add-value :usage                       #{4000 3300})
     (add-value :spend                               1024)
     (get-value :discounts)))

;; So, there's an evergreen plan you'll be rolled on to - let's find
;; your combined personal projection
(-> base-system
    (personal-projection :base-usage     :base-plan     :base-spend)
    (personal-projection :rollover-usage :rollover-plan :rollover-spend)
    (add-value :ratio (/ 1 2))
    (prod :ratio :usage :base-usage)
    (prod :ratio :usage :rollover-usage)
    (sum :base-spend :rollover-spend :spend)
    (add-value :base-plan
               {:standing-charge 10 :unit-rate 1 :discounts 100})
    (add-value :rollover-plan
               {:standing-charge 20 :unit-rate 2 :discounts 0})
    (add-value :usage 3300)
    (get-value :spend))

;; We can find the ratio, and in that way figure out when your supplier claims you are rolling over (something they might not tell you, or you can't find on your bill)
(-> base-system
    (personal-projection     :base-usage     :base-plan     :base-spend)
    (personal-projection :rollover-usage :rollover-plan :rollover-spend)
    (add-value :ratio #{(/ 1 4) (/ 2 4) (/ 3 4)})
    (prod :ratio :usage     :base-usage)
    (prod :ratio :usage :rollover-usage)
    (sum :base-spend :rollover-spend :spend)
    (add-value :base-plan
               {:standing-charge 10 :unit-rate 1 :discounts 100})
    (add-value :rollover-plan
               {:standing-charge 20 :unit-rate 2 :discounts 0})
    (add-value :spend 7355N)
    (add-value :usage 3300)
    (get-value :ratio))
