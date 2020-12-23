(ns advent2020.core
  (:require [clojure.string :as string]))

(defn listify-txt [path]
  (string/split-lines (slurp path)))

(defn split [regex s]
  (string/split s regex))