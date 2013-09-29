(ns contest.foldfile
  (:require [clojure.core.reducers :as r]
            [clojure.java.io :refer [input-stream]])
  (:import [java.nio.channels FileChannel FileChannel$MapMode]
           [java.nio ByteBuffer]
           [java.io FileInputStream]))

;; Bind to core.reducer's private ForkJoin functions
(def fjinvoke #'r/fjinvoke)
(def fjfork #'r/fjfork)
(def fjjoin #'r/fjjoin)


(defn- fold-channel [^FileChannel chn from to buffer-size combinef reducef]

  (defn reduce-slice [^ByteBuffer buffer from to combinef reducef]
    (reduce reducef (combinef) (-> buffer (.array) (String. "UTF-8") (.split "[\n]"))))

  (if (<= (- to from) buffer-size)
    (let [buffer (ByteBuffer/allocate (- to from))]
      (.read chn buffer from)
      (reduce reducef (combinef) (-> buffer (.array) (String. "UTF-8") (.split "[\n]"))))

    (let [a 1]
      (fjinvoke
       #(let [ft (r/fjtask (fold-channel chn from (+ from buffer-size) buffer-size combinef reducef))
              ct (r/fjtask (fold-channel chn from to buffer-size combinef reducef))
              bt (r/fjtask (fold-channel chn from to buffer-size combinef reducef))]
         (fjfork ft)
         (fjfork ct)
         (fjfork bt)
         (combinef (fjjoin ft) (fjjoin ct) (fjjoin bt)))))

    ))

(defn- foldseq
  "Utility function to enable reducers for Iota Seq's"
  [^iota.FileSeq s n combinef reducef]
  (if-let [[v1 v2] (.split s)]
    (let [fc (fn [child] #(foldseq child n combinef reducef))]
      (fjinvoke
       #(let [f1 (fc v1)
              t2 (r/fjtask (fc v2))]
         (fjfork t2)
         (combinef (f1) (fjjoin t2)))))
    (reduce reducef (combinef) (.toArray s))))

(def c (.getChannel (FileInputStream. "resources/test.txt")))

(.size c)

(fold-channel c 0 (.size c) 50 + (fn [r s] (+ r (count s))))


(deftype FoldFile [name]
  r/CollFold
  (coll-fold
   [v n combinef reducef]
   (with-open [f (FileInputStream. name)
               c (.getChannel f)]
     (fold-channel c 0 (.size c) n combinef reducef))))



(r/fold 50 + (fn [r s] (+ r (count s))) (FoldFile. "resources/test.txt"))