(ns vsts-hacks.background.vsts-api
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.string :as gstring]
            [goog.string.format]
            [cljs.core.async :refer [<! >! chan put! onto-chan]]
            [ajax.core :refer [GET POST raw-response-format]]
            [chromex.logging :refer-macros [log info warn error group group-end]]))

(def vsts-fields {:title "1"
                  :description "52"
                  :discussion "54"
                  :state "2"
                  :assigned-to "24"
                  :created-by "9"})

(defn get-field [item key]
  (get-in item ["fields" (get vsts-fields key)]))

(defn extract-work-item [event-arg]
  (->> event-arg
       (.-requestBody)
       (.-formData)
       (.-updatePackage)
       (map #(js->clj (.parse js/JSON %)))
       flatten
       (filter #(zero? (get % "id")))
       first))

(def wiql-uri "https://msmobilecenter.visualstudio.com/_apis/wit/wiql?api-version=2.2")

(def wiql-search-query-format "Select [Title] From WorkItems Where [System.Title] = '%s' And [System.TeamProject] = '%s' And [System.CreatedBy] = '%s' And [System.IsDeleted] = false Order By [Changed Date] Desc")

(def wiql-recent-query-format
  "Select [Title] From WorkItems Where [System.TeamProject] = '%s' AND [System.ChangedBy] = '%s'   And [System.IsDeleted] = false AND [System.ChangedDate] >= @Today - %s Order By [Changed Date] Desc")

(defn work-items-url [ids]
  (str "https://" "msmobilecenter.visualstudio.com"
       "/DefaultCollection/_apis/wit/workitems?api-version=3.0-preview&ids="
                                                       (clojure.string/join "," ids)
       "&$expand=relations&ErrorPolicy=omit"))

(defn get-work-items
  [ids]
  (let [out (chan)]
    (GET (work-items-url ids)
         :response-format (raw-response-format)
         :handler
         (fn [raw-response]
           (let [js-response (.parse js/JSON raw-response)]
             (put! out (js->clj js-response :keywordize-keys true)))))
    out))

(defn wiql-recent-query
  [project-name {:keys [changed-by days-old]}]
  (let [name (str (:name changed-by) "<" (:email changed-by) ">")]
    (gstring/format wiql-recent-query-format
                    project-name
                    name
                    days-old)))

(defn query-work-items
  [wiql]
  (let [out (chan)]
    (POST wiql-uri
          :format {:content-type "application/json;charset=utf-8"
                   :write (fn [cljs]
                            (.stringify js/JSON (clj->js cljs)))}
          :response-format (raw-response-format)
          :params {:query wiql}
          :handler #(put! out %))
    out))

(defn query-recently-changed
  [project-name {:keys [limit] :as spec}]
  (let [c (chan)]
    (go
      (let [result (<! (query-work-items (wiql-recent-query project-name spec)))
            result-parsed (.parse js/JSON result)]
        (->> (js->clj result-parsed :keywordize-keys true)
             (:workItems)
             (take limit)
             (>! c))))
    c))

(defn fetch-work-item-id [project-name item]
  (let [query (gstring/format wiql-search-query-format
                              (get-field item :title)
                              project-name
                              (get-field item :created-by))]
    (query-work-items query)))

(defn get-recently-changed
  [project-name {:keys [limit] :as spec}]
  (let [c (chan)]
    (go
      (let [recently-changed (<! (query-recently-changed project-name spec))
            ids (map :id recently-changed)
            full-work-items (<! (get-work-items ids))]
        (>! c full-work-items)))
    c))
