(ns pocketpal.frontend.app
  (:require
   [reagent.core :as r]
   [reagent.dom.client :as rdc]))

(def records (r/atom [{:date (js/Date. "2024-09-25") :amount 1 :type :init :remark "初始值😘"}
                      {:date (js/Date. "2024-09-25") :amount 2 :type :increase :remark "增加啦🥳"}
                      {:date (js/Date. "2024-09-30") :amount -1 :type :decrease :remark "“鸡蛋果冻”😋"}]))

(def show-rule (r/atom false))
(def show (r/atom false))
(def show-present (r/atom false))
(def show-past (r/atom false))
(def show-future (r/atom false))
(def current (r/atom ""))
(def rule-interval 5)
(def rule-amount 2)

(defn get-last-increase-date []
  (let [increases (filter #(= (:type %) :increase) @records)
        sorted-increases (sort-by :date > increases)]
    (if (seq sorted-increases)
      (:date (first sorted-increases))
      nil)))

(defn get-next-increase-date [date]
  (let [future-date (js/Date. (.getTime date))]
    (.setDate future-date (+ (.getDate date) rule-interval))
    future-date))

(defn get-all-future-increase-dates
  ([] (get-all-future-increase-dates (get-last-increase-date)))
  ([start-date] (let [next-date (get-next-increase-date start-date)]
                  (when next-date
                    (cons next-date (lazy-seq (get-all-future-increase-dates next-date)))))))

(defn apply-rule
  []
  (let [today (js/Date.)
        last-increase-date (get-last-increase-date)
        next-increase-date (get-next-increase-date last-increase-date)]
    (loop [nid next-increase-date]
      (when (<= nid today)
        (swap! records conj {:date nid
                             :amount rule-amount
                             :type :increase
                             :remark "增加啦🥳"})
        (recur (get-next-increase-date nid))))))

(defn get-balance
  []
  (apply-rule)
  (reduce + (map :amount @records)))

(defn format-date [date]
  (let [year (.getFullYear date)
        month (.padStart (str (inc (.getMonth date))) 2 "0")
        day (.padStart (str (.getDate date)) 2 "0")]
    (str year "-" month "-" day)))

(defn present []
  [:div
   [:h3.text-2xl.font-bold.text-center.mb-4
    "现在"]
   [:p.text-center.mb-4
    "你的零花钱有 " [:span.font-bold (get-balance)] " 元."]])

(defn past []
  [:div
   [:h3.text-2xl.font-bold.text-center.mb-4
    "过去"]
   [:ul.list-disc.list-inside.mb-4.text-sm
    (let [sorted-records (sort-by :date < @records)
          cumulative-amounts (reductions + (map :amount sorted-records))]
      (for [[idx r cumulative] (map vector (range) sorted-records cumulative-amounts)]
        ^{:key idx}
        [:li.font-mono
         (str
          (format-date (:date r))
          ": "
          (if (pos? (:amount r))
            "+"
            "")
          (:amount r)
          ", " cumulative
          ", "
          (:remark r))]))]])

(defn future []
  [:div
   [:h3.text-2xl.font-bold.text-center.mb-4
    "未来"]
   [:ul.list-disc.list-inside.mb-4.text-sm
    (let [future-dates-10 (take 10 (get-all-future-increase-dates))
          balance (get-balance)
          cumulative-amounts-10 (take 10 (reductions + (+ balance rule-amount) (repeat rule-amount)))]
      (for [[idx d cumulative] (map vector (range) future-dates-10 cumulative-amounts-10)]
        ^{:key idx}
        [:li.font-mono
         (str
          (format-date d)
          ": +"
          rule-amount
          ", " cumulative
          ", 增加啦🥳")]))]])

(defn rule []
  [:div
   [:h3.text-xl.font-bold.mb-2
    "规则"]
   [:p "每" rule-interval "天增加" rule-amount "元"]])

(defn handle-show []
  (swap! show not)
  (reset! show-present true)
  (reset! current (if @show-present "present" "")))

(defn handle-show-past []
  (reset! show-past true)
  (reset! current "past"))

(defn handle-show-present []
  (reset! show-present true)
  (reset! current "present"))

(defn handle-show-future []
  (reset! show-future true)
  (reset! current "future"))

(defn main-view []
  [:div.bg-gray-100
   [:div.min-h-screen.bg-gray-100.flex.items-center.justify-center.pb-32
    [:div.bg-white.p-8.rounded-lg.shadow-lg.max-w-md.w-full
     [:h1.text-3xl.font-bold.mb-6.text-center "👶蛋堡的零花钱💰"]
     [:div.flex.justify-center.mb-6
      [:img.w-32.h-32 {:src "img/piggy-bank.png" :alt "Piggy Bank"}]]
     [:div.flex.justify-center.mb-6
      [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded
       {:on-click #(handle-show)}
       (if @show "收起来" "打开看看")]]
     (when @show
       [:div
        [:div
         (when (= @current "past")
           [past])
         (when (= @current "present")
           [present])
         (when (= @current "future")
           [future])]

        [:div.flex.items-center.justify-between.mb-4
         [:button.bg-yellow-500.hover:bg-yellow-700.text-white.font-bold.py-2.px-4.rounded.disabled:opacity-75.disabled:hover:bg-yellow-500
          {:on-click #(handle-show-past)
           :disabled (= @current "past")}
          "< 过去"]
         [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded.disabled:opacity-75.disabled:hover:bg-blue-500
          {:on-click #(handle-show-present)
           :disabled (= @current "present")}
          "现在"]
         [:button.bg-green-500.hover:bg-green-700.text-white.font-bold.py-2.px-4.rounded.disabled:opacity-75.disabled:hover:bg-green-500
          {:on-click #(handle-show-future)
           :disabled (= @current "future")}
          "未来 >"]]

        [:div.flex.justify-center.mb-4
         [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded
          {:on-click #(swap! show-rule not)}
          (if @show-rule "收起来" "看规则")]]
        (when @show-rule
          [:div
           [rule]])])]]])

(defn init []
  (let [root (rdc/create-root (js/document.getElementById "root"))]
    (rdc/render root [main-view])))
