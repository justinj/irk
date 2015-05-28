(ns irc-bot.core
  (:require [pixie.io.tcp :as tcp]
            [pixie.io :as io]
            [pixie.string :as string]
            [pixie.csp :as csp]
            [pixie.async :as async]))

; https://tools.ietf.org/html/rfc2812

(defn connect [server]
  (tcp/tcp-client server 6667))

(defn send-message [client message]
  (println "Sending: " (pr-str message))
  (io/spit client message))

(defn parse-message [s]
  (let [words (string/split s " ")]
    (if (string/starts-with? s ":")
      {:prefix (nth words 0)
       :command (nth words 1)
       :params (drop 2 words)})))

(defn command->sym [command]
  (keyword (string/lower-case command)))

(defn sym->command [sym]
  (string/upper-case (name command)))

(defn user-message [user mode real-name]
  (str "USER " user " " mode " * :" real-name "\n"))

(defn nick-message [n]
  (str "NICK " n "\n"))

(defn join-message [channel]
  (str "JOIN " channel "\n"))

(defn privmsg-message [target message]
  (str "PRIVMSG " target " :" message "\n"))

(defn next-message [{:keys [client]}]
  (when-let [msg (io/read-line client)]
    (when (not (empty? msg))
      (let [msg-map (parse-message msg)]
        #_(prn (:command msg-map))
        (assoc msg-map :command (command->sym (:command msg-map)))))))

(defn join-room [{:keys [client]} room]
  (send-message client (join-message room)))

(defn register [{:keys [client]} nick]
  (set-user client nick)
  (set-nick client nick))

(defn set-user [client nick]
  (send-message client (user-message nick 0 nick)))

(defn set-nick [client nick]
  (send-message client (nick-message nick)))

(defn start [{:keys [server nick rooms] :as context}]
  (let [client (connect server)
        context (assoc context :client client)]
    (register context nick)
    (doseq [room rooms]
      (join-room context room))
    (let [ch (csp/chan)]
      (async/future
        (loop []
          (when-let [msg (next-message context)]
            (csp/>! ch msg))
          (recur)))
      ch
      )))

(defmulti handle-message #(:command %2))

(defmethod handle-message :privmsg [{:keys [client]} parsed]
  (when (string/starts-with? (:prefix parsed) ":justinjaffray")
    (send-message client (privmsg-message "#352udc" "Justin has gotten the milk."))
    (send-message client (privmsg-message "#352udc" "It is now Spencer's turn."))))

(defmethod handle-message :default [_ _] nil)

(def ch
  (start
    {:server "174.143.119.91"
     :nick "milkbot"
     :rooms ["#352udc"]
     :handler handle-message}))
