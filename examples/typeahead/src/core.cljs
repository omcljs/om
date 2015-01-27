(ns examples.typeahead.core
  (:refer-clojure :exclude [chars])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]))

(enable-console-print!)

(def chars (into [] "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(defn rand-char []
  (nth chars (rand-int (count chars))))

(defn rand-word []
  (apply str (take (inc (rand-int 10)) (repeatedly rand-char))))

(def app-state
  (atom
    {:words
     (into []
       (map (fn [w i] {:index i :word w :count (count w)})
         (sort (into [] (take 200 (repeatedly rand-word))))
         (range)))}))

(extend-type string
  ICloneable
  (-clone [s] (js/String. s)))

(extend-type js/String
  om/IValue
  (-value [s] (str s))
  ICloneable
  (-clone [s] (js/String. s)))

(extend-type number
  ICloneable
  (-clone [n] (js/Number. n)))

(extend-type js/Number
  om/IValue
  (-value [n] (.valueOf n))
  ICloneable
  (-clone [n] (js/Number. n)))

(defn hidden [^boolean bool]
  (if bool
    #js {:display "none"}
    #js {:display "block"}))

;; we have to use om/value because we've made
;; strings and numbers work as cursors and React doesn't
;; know how to handle these correctly

(defn word-index [index owner]
  (om/component (dom/span nil (om/value index))))

(defn word-count [count owner]
  (om/component (dom/span nil (om/value count))))

(defn word [the-word owner]
  (om/component (dom/span nil (om/value the-word))))

(defn item [the-item owner]
  (om/component
    (dom/li #js {:style (hidden (:hidden the-item))}
      (om/build word-index (:index the-item))
      (dom/span nil " ")
      (om/build word (:word the-item))
      (dom/span nil " ")
      (om/build word-count (:count the-item)))))

(defn change [e owner]
  (om/set-state! owner :text (.. e -target -value)))

(defn typeahead [data owner]
  (reify
    om/IInitState
    (init-state [_] {:text ""})
    om/IRenderState
    (render-state [_ {:keys [text]}]
      (let [words (:words data)]
        (dom/div nil
          (dom/h2 nil "Type ahead example")
          (dom/input
            #js {:type "text"
                 :ref "text-field"
                 :value text
                 :onChange #(change % owner)})
          (apply dom/ul nil
            (om/build-all item words
              {:fn (fn [x]
                     (if-not (string/blank? text)
                       (cond-> x
                         (not (zero? (.indexOf (:word x) text))) (assoc :hidden true))
                       x))})))))))

(om/root typeahead app-state {:target (.getElementById js/document "app")})
