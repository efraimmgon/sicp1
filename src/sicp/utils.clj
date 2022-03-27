(ns sicp.utils)

(defn square [x] (* x x))

(defn cube [x] (* x x x))

(defn average [& args]
  (/ (apply + args) 
     (count args)))
