(ns pixie.code-generation.sexpr-emit)


(defmulti to-sexpr :op)

(defmethod to-sexpr :do
  [{:keys [statements ret]}]
  `(do ~@(map to-sexpr statements)
       ~(to-sexpr ret)))

(defmethod to-sexpr :if
  [{:keys [test then else]}]
  `(if ~(to-sexpr test)
     ~(to-sexpr then)
     ~(to-sexpr else)))

(defmethod to-sexpr :const
  [{:keys [form]}]
  form)


(defmethod to-sexpr :let
  [{:keys [bindings body]}]
  `(let* [~@(mapcat
             (fn [{:keys [name value]}]
               [name (to-sexpr value)])
             bindings)]
         ~(to-sexpr body)))

(defmethod to-sexpr :binding
  [{:keys [name]}]
  name)

(defmethod to-sexpr :fn
  [{:keys [arities name]}]
  `(fn* ~name ~@(map to-sexpr arities)))

(defmethod to-sexpr :fn-body
  [{:keys [args body]}]
  `(~args ~(to-sexpr body)))
