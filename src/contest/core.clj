(ns contest.core
  (:require [clojure.core.reducers :as r]
            [clojure.string :as s]
            [iota :as iota]))

(defn street-name [street]
    (-> street
        (s/replace #"\s(NORTH|SOUTH|EAST|WEST|E|W|N|S)$" "")
        (s/replace #"\s(ST|STREET|AV|AVE|COURT|CRT|CT|RD )$" "")
        (s/replace #"^\d+\s" "")))

(defn parse [line]
  (let [parts (s/split line #",")]
    {:set-fine-amount (if-let [set-fine-amount (get parts 4)]
                        (Integer/parseInt set-fine-amount)
                        0)
     :street-name (street-name (get parts 7))}))


(parse "***47870,20120101,5,PARK HWY PROHIBED TIME/DAY,40,0001,N/S,10 ST CLAIR AVE N,W/O,20 ELIZABETH ST,ON")

(defn solve []
  (defn solve-reduce
    ([] {})
    ([r line] (let [{:keys [set-fine-amount street-name]} (parse line)]
                (update-in r [street-name] #(if (nil? %) set-fine-amount (+ % set-fine-amount))))))
  (defn solve-combine
    ([] {})
    ([ra rb] (merge-with + ra rb)))
  (->> (rest (iota/seq "./resources/Parking_Tags_Data_2012.csv"))
       (r/fold solve-combine
               solve-reduce)))

(def r (into (array-map) (time (solve))))


(+ 3781095 (* 0.05 3781095))

(- 3781095 (* 0.05 3781095))

(def m (java.util.TreeMap.))

(sort-by #(- (val %)) (time (solve)))
