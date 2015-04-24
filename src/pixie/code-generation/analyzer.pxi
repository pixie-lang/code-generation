(ns pixie.code-generation.analyzer)

(def *env* nil)
(set-dynamic! (var *env*))

(defmulti analyze-form (fn [x]
                         (cond
                           (nil? x) nil
                           (seq? x) :seq
                           (vector? x) :vector
                           (symbol? x) :symbol
                           (number? x) :number)))

(defmulti analyze-seq (fn [x]
                         (let [f (first x)]
                           (if (symbol? f)
                             f
                             :invoke))))


;; Special Forms

(defmethod analyze-seq 'do
  [x]
  {:op :do
   :children '[:statements :ret]
   :env *env*
   :form x
   :statements (mapv analyze-form (butlast (next x)))
   :ret (analyze-form (last x))})

(defmethod analyze-seq 'if
  [[_ test then else :as form]]
  {:op :if
   :children '[:test :then :else]
   :env *env*
   :form form
   :test (analyze-form test)
   :then (analyze-form then)
   :else (analyze-form else)})

(defmethod analyze-form nil
  [_]
  {:op :const
   :env *env*
   :form nil})

(defmethod analyze-seq :default
  [[sym & args]]
  (let [resolved (resolve-in (the-ns (:ns *env*)) sym)]
    (if (and resolved
             (macro? @resolved))
      (analyze-form (apply @resolved args))
      (throw [:analyzeError
              (str "Don't know how to analyze " sym args)]))))


(defmethod analyze-form :number
  [x]
  {:op :const
   :form x
   :env *env*})

(defmethod analyze-form :seq
  [x]
  (analyze-seq x))

(defn new-env
  "Creates a new (empty) environment"
  []
  {:ns 'user})


(defn analyze [form]
  (binding [*env* (new-env)]
    (analyze-form form)))





(defn walk [post pre selector node]
  (-> (reduce
       (fn [node k]
         (let [v (get node k)
               result (if (or (vector? v)
                              (seq? v))
                        (mapv (partial walk post pre selector) v)
                        (walk post pre selector v))]
           (assoc node k result)))
       (pre node)
       (selector node))
      post))


(defn remove-env [ast]
  (walk #(dissoc % :env)
        identity
        :children
        ast))

(remove-env (analyze '(do 1 1 2)))
