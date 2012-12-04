(ns fungp.test.util)

(defn ts [] (.format (new java.text.SimpleDateFormat "YYYYMMdd.HHmmss") (new java.util.Date)))

(def now (ts))