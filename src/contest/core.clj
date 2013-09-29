(ns contest.core
  (:require [clojure.core.reducers :as r]
            [clojure.string :as s]
            [iota :as iota]))

(def suffixes #{"E" "W" "N" "S" "NORTH" "SOUTH" "EAST" "WEST"
                "ST" "CT" "RD" "AV" "AVE" "CRT" "STREET" "COURT" "CRES"
                "CRS" "ROAD" "AVENUE" "BLVD" "GARDEN" "GARDENS" "GRDNS" "CRESCENT"})

(defn extract-street-name [^String location1]
  (let [^StringBuilder sb (StringBuilder.)]
    (doseq [^String part (s/split location1 #"\s+")]
      (when-not (or (re-find #"\d" part)
                    (<= (.length part) 2)
                    (and (> (.length sb) 0) (contains? suffixes part)))
        (doto sb (.append part) (.append " "))))
    (.substring sb 0 (if (> (.length sb) 0)
                       (dec (.length sb))
                       (.length sb)))))

(defn parse [line]
  (let [[_ _ _ _ set-fine-amount _ _ street-name] (s/split line #",")]
    [(extract-street-name (s/upper-case street-name))
     (when set-fine-amount (Integer/parseInt set-fine-amount))]))

(defn tree-map [m]
  (doto
    (java.util.TreeMap. #(compare (get m %2) (get m %1)))
    (.putAll m)))

(defn sort-parking-tickets [file]

  (defn reducef
    ([m [street-name set-fine-amount]]
     (update-in m [street-name] (fnil #(+ % set-fine-amount) 0))))

  (defn combinef
    ([] {})
    ([ma mb] (merge-with + ma mb)))

  (->> (rest (iota/seq file))
       (r/map parse)
       (r/filter (fn [[street-name set-fine-amount]] (not (or (empty? street-name) (nil? set-fine-amount)))))
       (r/fold combinef reducef)
       (tree-map)))

(defn -main [& args]
  (time (sort-parking-tickets "./resources/Parking_Tags_Data_2012.csv")))

(-main)
