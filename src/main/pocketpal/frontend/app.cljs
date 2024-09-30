(ns pocketpal.frontend.app
  (:require
   [reagent.core :as r]
   [reagent.dom.client :as rdc]))

(def records (r/atom [{:date (js/Date. "2024-09-25") :amount 1 :type :init :remark "💰初始值"}
                      {:date (js/Date. "2024-09-25") :amount 2 :type :increase :remark "🎉增加啦"}
                      {:date (js/Date. "2024-09-30") :amount -1 :type :decrease :remark "😋“鸡蛋果冻”"}]))

(def show (r/atom false))

(defn get-last-increase-date []
  (let [increases (filter #(= (:type %) :increase) @records)
        sorted-increases (sort-by :date > increases)]
    (if (seq sorted-increases)
      (:date (first sorted-increases))
      nil)))

(defn get-next-increase-date [date]
  (let [future-date (js/Date. (.getTime date))]
    (.setDate future-date (+ (.getDate date) 5))
    future-date))

(defn apply-rule
  []
  (let [today (js/Date.)
        last-increase-date (get-last-increase-date)
        next-increase-date (get-next-increase-date last-increase-date)]
    (loop [nid next-increase-date]
      (when (<= nid today)
        (swap! records conj {:date nid
                             :amount 2
                             :type :increase
                             :remark "🎉增加啦"})
        (recur (get-next-increase-date nid))))))

(defn balance
  []
  (apply-rule)
  (reduce + (map :amount @records)))

(defn view-balance []
  (swap! show not))

(defn format-date [date]
  (let [year (.getFullYear date)
        month (.padStart (str (inc (.getMonth date))) 2 "0")
        day (.padStart (str (.getDate date)) 2 "0")]
    (str year "-" month "-" day)))

;;(defn format-date [date]
;;  (let [formatter (js/Intl.DateTimeFormat. "en-GB" #js {:year "numeric" :month "2-digit" :day "2-digit"})]
;;    (.format formatter date)))

(defn main-view []
  [:div
   [:h1 "蛋堡的零花钱"]
   [:h2 "🐷💬: 嗨,你好啊"]
   [:button {:on-click view-balance} "👀看看我有多少零花钱💰"]
   (when @show
     [:div
      [:p "你的零花钱有 " [:span {:style {:font-size "2em"}} (balance)] " 元."]
      [:ul
       (let [sorted-records (sort-by :date < @records)]
         (for [[idx r] (map-indexed vector sorted-records)]
           ^{:key idx} [:li (str
                             (format-date (:date r))
                             ": "
                             (if (and
                                  (= (:type r) :increase)
                                  (pos? (:amount r)))
                               "+"
                               "")
                             (:amount r)
                             ", "
                             (:remark r))]))]])])

(defn init []
  (let [root (rdc/create-root (js/document.getElementById "root"))]
    (rdc/render root [main-view])))
