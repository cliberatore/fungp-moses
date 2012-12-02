(ns fungp.test.util)

(defn ts [] (.format (new java.text.SimpleDateFormat "YYYYMMDDHHmmss") (new java.util.Date)))

(def now (ts))