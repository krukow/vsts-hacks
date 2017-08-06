(ns vsts-hacks.popup.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [<! chan]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.protocols :refer [post-message!]]
            [chromex.ext.runtime :as runtime :refer-macros [connect]]
            [chromex.ext.browser-action :as browser]
            [chromex.chrome-event-channel :refer [make-chrome-event-channel]]
            [goog.dom :as gdom]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]))


(defonce local-state (atom {:recent-items []
                            :recent-viewed-items []}))

(defn read [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ value] (find st key)]
      {:value value}
      {:value :not-found})))

;;Not used via David Nolen's sample of om.next
(defn mutate [{:keys [state] :as env} key params]
  (if (= 'increment key)
    {:value {:keys [:count]}
     :action #(swap! state update-in [:count] inc)}
    {:value :not-found}))

(defn work-item-link [work-item]
  (str "https://msmobilecenter.visualstudio.com/Mobile-Center/_workitems?id=" (:id work-item)
       "&_a=edit&fullScreen=true"))

(defn first-name
  [vsts-identity]
  (let [first-name (first (clojure.string/split vsts-identity " "))]
    (if (clojure.string/blank? first-name)
      "Not Assigned"
      first-name)))

(defn click-handler
  [e]
  (let [el (.-target e)]
    (when-not (or (.-shiftKey e) ( .-ctrlKey e) ( .-metaKey e))
      (.create (.-tabs js/chrome) (clj->js {:url (.-href el)})))))

(defui RecentItems
  static om/IQuery
  (query [this]
    [:recent-items])
  Object
  (render
   [this]
   (let [{:keys [recent-items]} (om/props this)]
     (if (or (= :not-found recent-items)
             (empty? recent-items))
       (dom/div nil
                (dom/h1 nil "Welcome")
                (dom/p nil "Did not find recently any created items! Try to open/refresh the browser if you're expecting some."))
       (dom/div
        nil
        (dom/h1
         nil
         (str "Recently edited work items"))
        (apply dom/ul
               nil
               (map
                (fn [item]
                  (dom/li
                   nil
                   (dom/span
                    nil
                    (dom/a (clj->js {:href (work-item-link item)
                                     :onClick click-handler})
                           (get-in item [:fields :System.Title]))
                    (str " [" (get-in item [:fields :System.WorkItemType]) "]")
                    (str " [" (first-name (get-in item [:fields :System.AssignedTo])) "]"))))
                (:value recent-items))))))))

(defui RecentViewedItems
  static om/IQuery
  (query [this]
    [:recent-viewed-items])
  Object
  (render [this]
    (let [{:keys [recent-viewed-items]} (om/props this)]
      (dom/div
       nil
       (dom/h1
        nil
        (str "Recently viewed items"))
       (apply dom/ul nil
          (map
            (fn [item]
              (dom/li
               nil
               (dom/span
                nil
                (dom/a (clj->js {:href (work-item-link item)
                                 :onClick click-handler})
                       (get-in item [:fields :System.Title]))
                (str " [" (get-in item [:fields :System.WorkItemType]) "]")
                (str " [" (first-name (get-in item [:fields :System.AssignedTo])) "]"
                    ))))
            (:value recent-viewed-items)))))))

(def recent-edited-items (om/factory RecentItems))
(def recent-viewed-items (om/factory RecentViewedItems))

(defui VSTSHacks
  static om/IQuery
  (query [this] (into [] (concat (om/get-query RecentItems)
                                 (om/get-query RecentViewedItems))))
  Object
  (render [this]
          (dom/div nil
                   (recent-edited-items (om/props this))
                   (recent-viewed-items (om/props this)))))

(def reconciler
  (om/reconciler
    {:state local-state
     :parser (om/parser {:read read :mutate mutate})}))

(def reconciler2
  (om/reconciler
    {:state local-state
     :parser (om/parser {:read read :mutate mutate})}))


(defn process-message! [message]
  (let [state (js->clj message :keywordize-keys true)]
    (reset! local-state state)))

(defn run-message-loop! [message-channel]
  (go-loop []
    (when-let [message (<! message-channel)]
      (process-message! message)
      (recur))))

(defn connect-to-background-page! []
  (let [background-port (runtime/connect)]
    (post-message! background-port (clj->js {:action "get-state"}))
    (run-message-loop! background-port)))

; -- main entry point -------------------------------------------------------------------------------------------------------

(defn init! []
  (connect-to-background-page!)
  (om/add-root! reconciler
                RecentItems (gdom/getElement "container"))
  (om/add-root! reconciler2
                RecentViewedItems (gdom/getElement "container2")))
