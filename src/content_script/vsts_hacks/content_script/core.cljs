(ns vsts-hacks.content-script.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [cljs.core.async :refer [<! >! timeout chan]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.protocols :refer [post-message!]]
            [chromex.ext.runtime :as runtime :refer-macros [connect]]))


(defn extract-logged-in-user-from-page! [background-port]
  (let [script-data-el (aget (.getElementsByClassName js/document "vss-web-page-data") 0)
        script-data (.parse js/JSON (.-innerHTML script-data-el))
        header-data (aget (.-data script-data) "ms.vss-tfs-web.header-data")
        usersettings-data (aget header-data "usersettingsMenuItems")
        user (first (filter #(= "user" (.-id %))
                            (array-seq usersettings-data 0)))]
    (if user
      (post-message! background-port (clj->js
                                      {:action "set-user"
                                       :data
                                       {:name (aget user "text")
                                        :email (aget user "title")}}))
      (post-message! background-port (clj->js {:user nil})))))


(def work-item-dialog-query ".new-workitem-dialog span[aria-label='ID Field']")
(def work-item-form-query ".work-item-form-id span[aria-label='ID Field']")

(defn wait-for-work-item-pop-up-satisfies
  [pred]
  (let [out (chan)]
    (letfn [(inner-loop []
              (go
                (let [elements-popup (array-seq (.querySelectorAll
                                                 js/document
                                                 work-item-dialog-query))
                      elements-form (array-seq (.querySelectorAll
                                                js/document
                                                work-item-form-query))
                      elements (concat elements-popup elements-form)]
                  (if (pred elements)
                    (>! out elements)
                    (.requestAnimationFrame js/window inner-loop)))))]
      (inner-loop))
    out))

(defn track-work-items! [background-port]
  (let [wait-for-work-item-pop-up!
        #(wait-for-work-item-pop-up-satisfies
          (fn [elements] (> (count elements) 0)))

        wait-for-work-item-gone!
        #(wait-for-work-item-pop-up-satisfies
          (fn [elements]
            (zero? (count elements))))

        parser (fn [el]
                 (if-let [text (.-innerHTML el)]
                   (let [id (.parseInt js/window text 10)]
                     (if (js/isNaN id) nil id))))]

    (go-loop [elements (<! (wait-for-work-item-pop-up!))]
      (let [id-element (first (filter parser elements)) ]
        (when id-element
          (post-message! background-port (clj->js
                                          {:action "view-work-item"
                                           :data
                                           {:id (parser id-element)}}))
          (extract-logged-in-user-from-page! background-port))
        (<! (wait-for-work-item-gone!))
        (recur (<! (wait-for-work-item-pop-up!)))))))

(defn connect-to-background-page! []
  (let [background-port (runtime/connect)]
    (extract-logged-in-user-from-page! background-port)
    (track-work-items! background-port)))

(defn init! []
  (connect-to-background-page!))
