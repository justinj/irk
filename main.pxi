(ns irc-bot.core
  (:require [pixie.io.tcp :as tcp]
            [pixie.io :as io]
            [pixie.string :as string]))

(def client
  (tcp/tcp-client
    "174.143.119.91" ; freenode
    6667))


(defn send-message [client message]
  (println "Sending: " (pr-str message))
  (io/spit client message))

(defn parse-message [s]
  (let [words (string/split s " ")]
    (if (string/starts-with? s ":")
      {:prefix (nth words 0)
       :command (nth words 1)
       :params (drop 2 words)})))

(def nick "milkbot")

(defn user-message [user mode real-name]
  (str "USER " user " " mode " * :" real-name "\n"))

(defn nick-message [n]
  (str "NICK " n "\n"))

(defn join-message [channel]
  (str "JOIN " channel "\n"))

(defn privmsg-message [target message]
  (str ":milkbot PRIVMSG " target " :" message "\n"))

(send-message client (user-message nick 0 nick))
(send-message client (nick-message nick))
(send-message client (join-message "#352udc"))

(defmulti handle-message #(:command %2))

(defmethod handle-message "PRIVMSG" [client parsed]
  (when (string/starts-with? (:prefix parsed) ":justinjaffray")
    (send-message client (privmsg-message "#352udc" "Justin has gotten the milk."))
    (send-message client (privmsg-message "#352udc" "It is now Spencer's turn."))))

(defmethod handle-message :default [_ _] nil)

(loop []
  (when-let [msg (io/read-line client)]
    (handle-message client (parse-message msg)))
  (recur))
