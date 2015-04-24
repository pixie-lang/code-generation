(ns pixie.code-generation.test-analyzer
  (:require [pixie.test :refer :all]
            [pixie.code-generation.analyzer :refer :all]))



(deftest test-const-integer
  (assert= {:op :const
            :form 1
            :env (new-env)}
           (analyze 1)))


(deftest test-do
  (assert= {:op :do
            :env {:ns 'user}
            :form '(do 1)
            :children [:statements :ret]
            :statements []
            :ret {:op :const
                  :form 1
                  :env (new-env)}}
           (analyze '(do 1)))

  (assert= {:op :do
            :env (new-env)
            :form '(do 1 2 3)
            :children [:statements :ret]
            :statements [{:op :const
                          :form 1
                          :env (new-env)}
                         {:op :const
                          :form 2
                          :env (new-env)}]
            :ret {:op :const
                  :form 3
                  :env (new-env)}}
           (analyze '(do 1 2 3))))

(deftest test-if
  (assert= {:op :if
            :children [:test :then :else]
            :env (new-env)
            :form '(if 1 2 3)
            :test {:op :const
                   :form 1
                   :env (new-env)}
            :then {:op :const
                   :form 2
                   :env (new-env)}
            :else {:op :const
                   :form 3
                   :env (new-env)}}
           (analyze '(if 1 2 3))))

(deftest test-macro
  (assert= {:op :if
            :children [:test :then :else]
            :env (new-env)
            :form '(if 1 (do 2))
            :test {:op :const
                   :form 1
                   :env (new-env)}
            :then {:op :do
                   :env {:ns 'user}
                   :form '(do 2)
                   :children [:statements :ret]
                   :statements []
                   :ret {:op :const
                         :form 2
                         :env (new-env)}}
            :else {:op :const
                   :form nil
                   :env (new-env)}}
           (analyze '(when 1 2))))

(deftest test-fn-invoke
  (assert= {:op :invoke
            :children [:fn :args]
            :form '(+ 1 2)
            :env (new-env)
            :fn {:op :global
                 :env (new-env)
                 :form '+}
            :args [{:op :const
                    :form 1
                    :env (new-env)}
                   {:op :const
                    :form 2
                    :env (new-env)}]}
           (analyze '(+ 1 2))))

(deftest test-let*
  (assert= {:op :let
            :form '(let* [x 1]
                         x)
            :children [:bindings :body]
            :bindings [{:op :binding
                        :children [:value]
                        :form 1
                        :name 'x
                        :value {:op :const
                                :form 1}}]
            :body {:op :do
                   :form '(do x)
                   :children [:statements :ret]
                   :statements []
                   :ret {:op :global
                         :form 'x}}}
           (-> (analyze '(let* [x 1]
                               x))
               remove-env)))

