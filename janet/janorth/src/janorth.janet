(defn new-context [] @{
  :stack @[]
  :words @{}
  :variables @{}
  :constants @{}
  })

(defmacro stack [] ~(get context :stack))
(defmacro words [] ~(get context :words))
(defmacro variables [] ~(get context :variables))
(defmacro constants [] ~(get context :constants))

(defmacro pop [&opt n]
  (default n 1)
  (if (= n 1)
    ~(array/pop (stack))
    ~(tuple ,;(array/new-filled n ~(array/pop (stack))))))
(defmacro popr [&opt n] ~(reverse (pop ,n)))

(defmacro ff [& body] ~(fn [context] ,;body))
(defmacro ffp [& body] ~(fn [context] (array/push (stack) ,;body)))

(defmacro stack-index [i] ~(- (length (stack)) (inc ,i)))

(def system-funcs {
  :.      (ff  (pp (pop)))
  :.s     (ff  (pp (stack)))
  :+      (ffp (+ ;(pop 2)))
  :-      (ffp (- ;(popr 2)))
  :*      (ffp (* ;(pop 2)))
  :/      (ffp (/ ;(popr 2)))
  :floor  (ffp (math/floor (pop)))
  :ceil   (ffp (math/ceil (pop)))
  :<      (ffp (if (< ;(popr 2)) -1 0))
  :drop   (ff  (pop))
  :roll   (ff
            (def orig-index (pop))
            (def index (stack-index orig-index))
            (array/push (stack) (get (stack) index))
            (array/remove (stack) index))
  :pick   (ff
            (def orig-index (pop))
            (def index (stack-index orig-index))
            (array/push (stack) (get (stack) index)))
  :!      (ff
            (def var-name (pop))
            (def var-value (pop))
            (put (variables) (keyword var-name) var-value))
  :@      (ffp (get (variables) (keyword (pop))))
  })

(defn get-func [context name]
  (get (words) (keyword name)))

(def statements {
  :: (fn [args context _]
    (put (words) (keyword (first args)) (tuple/slice args 1)))
  :if (fn [args context eval-expr]
    (if (not (= 0 (pop)))
      (eval-expr context (first args))
      (when (= 2 (length args))
        (eval-expr context (get args 1)))))
  :do (fn [args context eval-expr]
    (def start-index (pop))
    (def end-index (inc (pop)))
    (each i (range start-index end-index)
      (put (constants) :i i)
      (eval-expr context args)))
  :?do (fn [args context eval-expr]
    (def start-index (pop))
    (def end-index (pop))
    (each i (range start-index end-index)
      (put (constants) :i i)
      (eval-expr context args)))
  :see (fn [args context _]
    (def func-src (get-func context (first args)))
    (print ": " (string/join (map string func-src) " ") " ;"))
  :variable (fn [args context _]
    (put (variables) (keyword (first args)) nil))
  :constant (fn [args context _]
    (put (constants) (keyword (first args)) (pop)))
  })

(defn has [t k]
  (not (= nil (get t (keyword k)))))

(defn eval-expr [context expr]
  (if (= (length expr) 0)
    stack
    (do
      (def command (first expr))
      (cond
        (tuple? command) ((get statements (keyword (first command))) (tuple/slice command 1) context eval-expr)
        (has system-funcs command) ((get system-funcs (keyword command)) context)
        (has (words) command) (eval-expr context (get (words) (keyword command)))
        (has (constants) command) (array/push (stack) (get (constants) (keyword command)))
        (array/push (stack) command))
      (eval-expr context (array/slice expr 1)))))

(defn wrap-stmt [& captures] captures)
(def expression-peg (peg/compile ~{
  :newline (+ "\n" "\r")
  :number {
    :digits (some :d)
    :frac (* "." :digits)
    :main (/
      (<- (* (? "-") :digits (? :frac)))
      ,scan-number)}
  :word (<- (some :S))
  :item (+ :s :number :word)
  :stmt-see (* (<- "see") :s :word)
  :stmt-compile {
    # :desc (* "(" (any (if-not "-" 1)) "--" (any (if-not ")" 1)) ")")
    :if (*
      (<- "if")
      (/ (some (if-not (+ "else" "then") :item)) ,wrap-stmt)
      (? (* "else"
        (/ (some (if-not "then" :item)) ,wrap-stmt)))
      "then")
    :do (*
      (<- (+ "do" "?do"))
      (some (if-not "loop" :item))
      "loop")
    :compile-stmt (/ (+ :if :do) ,wrap-stmt)
    :main (*
      (<- ":")
      (some (if-not ";" (+ :compile-stmt :item)))
      ";")}
  :stmt-variable (* (<- "variable") :s :word)
  :stmt-constant (* (<- "constant") :s :word)
  :stmt (/
    (+ :stmt-compile :stmt-see :stmt-variable :stmt-constant)
    ,wrap-stmt)
  :comment (* "\\" :s (any (if-not :newline 1)))
  :main (any (+ :comment :stmt :item))
  }))

(defn eval-src [context src]
  (eval-expr context (peg/match expression-peg src)))

(defmacro preload [path]
  (def handle (file/open path))
  (def content (file/read handle :all))
  (file/close handle)
  content)

(defn main [&]
  (def context (new-context))
  (eval-src context (preload "src/core.f"))
  (while true
    (eval-src context (file/read stdin :line))))
