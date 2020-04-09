(ns scherz.server
  (:gen-class)
  (:require [compojure.core :refer [defroutes GET ANY]]
            [compojure.handler :refer [site]]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :refer [response]]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [environ.core :refer [env]]
            [scherz.scale :refer [scales]]
            [scherz.generate :refer [initial-chords generate-chords
                                     generate-progression]]))

(defn handle-errors [handler]
  (fn [request]
    (try
      (handler request)
      (catch clojure.lang.ExceptionInfo e
        {:status 400 :body (str e)})
      (finally
        {:status 500 :body "Something went wrong on our end :("}))))

(defroutes routes
  (GET "/scales" []
       (response scales))
  (GET "/generate-progression" {body :body}
       (response (generate-progression (:scales body)
                                       (:tensions body)
                                       (:options body))))
  (GET "/initial-chords" {body :body}
       (response (initial-chords (:scales body)
                                (:tonic body))))
  (GET "/generate-chords" {body :body}
       (response (generate-chords (:scales body)
                                  (:prev body)
                                  (:tension body))))
  (ANY "*" []
       {:status 404 :body "Not found :("}))

(def app
  (-> routes
      (wrap-json-body {:keywords? true})
      wrap-json-response
      handle-errors))

(defn -main [& [port]]
  (let [port (Integer. (or port (env :port) 5000))]
    (jetty/run-jetty (site #'app) {:port port :join? false})))

; (.stop server)
; (def server (-main))
