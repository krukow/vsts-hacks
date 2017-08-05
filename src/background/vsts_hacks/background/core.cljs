(ns vsts-hacks.background.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.string :as gstring]
            [goog.string.format]
            [vsts-hacks.background.vsts-api :as api]
            [vsts-hacks.background.storage :as storage]
            [cljs.core.async :refer [<! chan put! onto-chan]]
            [ajax.core :refer [GET POST]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.chrome-event-channel :refer [make-chrome-event-channel]]
            [chromex.protocols :refer [post-message! get-sender]]
            [chromex.ext.tabs :as tabs]
            [chromex.ext.runtime :as runtime]
            [chromex.ext.web-request :as web-request]))

(def clients (atom []))

(defonce state (atom {:recent-items []
                      :recent-viewed-item-ids []
                      :recent-viewed-items []}))


(defn add-client! [client]
  (swap! clients conj client))

(defn remove-client! [client]
  (let [remove-item (fn [coll item] (remove #(identical? item %) coll))]
    (swap! clients remove-item client)))

(defn run-client-message-loop! [client]
  (go-loop []
    (when-let [message-payload (js->clj (<! client) :keywordize-keys true) ]
      (case (:action message-payload)
        "get-state" (post-message! client (clj->js @state))
        "set-user" (swap! state assoc :user (:data message-payload))
        "view-work-item" (swap! state
                                (fn [state]
                                  (update-in
                                   state
                                   [:recent-viewed-item-ids]
                                   (fn [item-ids]
                                     (let [new-work-item (:data message-payload)
                                           item-ids (remove #{new-work-item} item-ids)]
                                       (cons new-work-item item-ids)))))))
      (recur))
    (remove-client! client)))

(defn load-recent-work-items! []
  (go
    (let [recent (<! (api/get-recently-changed "Mobile-Center"
                                               {:limit 10
                                                :changed-by (:user @state)
                                                :days-old 14}))]
      (swap! state assoc :recent-items recent)
      (let [state (clj->js @state)]
        (doseq [client @clients]
          (post-message! client state))))))

(defn load-recent-viewed-work-items! []
  (go
    (when-let [recent-items (:recent-viewed-item-ids @state)]
      (let [recent (<! (api/get-work-items (map :id recent-items)))]
        (swap! state assoc :recent-viewed-items recent)
        (doseq [client @clients]
          (post-message! client (clj->js @state)))))))

(defn add-user-watcher! []
  (add-watch
   state
   :user
   (fn [_key _ref old-value new-value]
     (let [old-user (:user old-value)
           new-user (:user new-value)]
       (when (or (nil? old-user)
               (not (= (:email old-user) (:email new-user))))
         (load-recent-work-items!))))))

(defn add-recent-viewed-watcher! []
  (add-watch
   state
   :recent-viewed-items
   (fn [_key _ref old-value new-value]
     (let [old-items (:recent-viewed-item-ids old-value)
           new-items (:recent-viewed-item-ids new-value)
           new-items-count (count new-items)]
       (when (> new-items-count 10) ; max 10 viewed items
         (swap! state assoc :recent-viewed-item-ids (take 10 new-items)))
       (when-not (= new-items old-items)
         (load-recent-viewed-work-items!))))))


(defn add-storage-watcher! []
  (add-watch
   state
   :storage
   (fn [_key _ref old-value new-value]
     (when-not (= old-value new-value)
       (storage/save-state! new-value)))))

(defn handle-client-connection! [client]
  (add-client! client)
  (run-client-message-loop! client))

(defn process-chrome-event [event-num event]
  (let [[event-id event-args] event]
    (case event-id
      ::runtime/on-connect (apply handle-client-connection! event-args)
      nil)))

(defn run-chrome-web-loop! [chrome-event-channel complete-channel]
  (go-loop []
    (when-let [[[event-id data] event-args] (alts! [chrome-event-channel complete-channel])]
      (load-recent-work-items!)
      (recur))))

(defn run-chrome-event-loop! [chrome-event-channel]
  (go-loop [event-num 1]
    (when-let [event (<! chrome-event-channel)]
      (process-chrome-event event-num event)
      (recur (inc event-num)))))

(defn boot-chrome-event-loop! []
  (let [chrome-event-channel (make-chrome-event-channel (chan))]
    (runtime/tap-all-events chrome-event-channel)
    (run-chrome-event-loop! chrome-event-channel)))

(defn boot-chrome-web-loop! []
  (let [chrome-event-channel (make-chrome-event-channel (chan))
        complete-channel (make-chrome-event-channel (chan))]
    (web-request/tap-on-before-request-events
     chrome-event-channel
     (clj->js
      {:urls ["*://msmobilecenter.visualstudio.com/_api/_wit/updateWorkItems*"]})
     (clj->js ["requestBody"]))
    (web-request/tap-on-completed-events
     complete-channel
     (clj->js
      {:urls ["*://msmobilecenter.visualstudio.com/_api/_wit/updateWorkItems*"]}))
    (run-chrome-web-loop! chrome-event-channel complete-channel)))

(defn init! []
  (go
    (if-let [st (<! (storage/get-state))]
      (reset! state st))
    (add-storage-watcher!)
    (boot-chrome-event-loop!)
    (boot-chrome-web-loop!)
    (add-user-watcher!)
    (add-recent-viewed-watcher!)))
