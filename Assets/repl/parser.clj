(ns repl.parser)

(def rainbows [
  "#ffa500ff"
  "#add8e6ff"
  "#ff00ffff"
  "#008000ff"
  "#00ffffff"
  "#c0c0c0ff"])

(def scheme {
  :comment "#808080ff"
  :symbol  "#bbbbbbff"
  :ns      "#00ffffff"
  :kw      "#ab00ffff"
  :qkw     "#ff0babff"
  :int     "#ffff00ff"
  :reader  "#00ffffff"
  :illegal "#ff0000ff"})

(defn colorize [t]
  (str "<color=" (scheme (:token t) "#ffa500ff") ">" (:value t) "</color>"))

(defn highlite [col]
  (let [depth (volatile! 0)]
    (reduce 
      (fn [s t]
        (if (or (:open t) (:close t))
            (do 
              (if (:close t) (vswap! depth dec))
              (let [res (str s "<color=" (get rainbows @depth (:illegal scheme)) ">" 
                        (:value t) "</color>")] 
                (if (:open t) (vswap! depth inc)) 
                res))
            (str s (colorize t))))
      "" col)))

(def token:rx
  #"[ \n\t\r]+|
    |\"(\\\"|[^\"])*\"|
    |;[^;\n\r]+|
    |[\(\)\[\]\{\}\^\~\`\'\@]|
    |[^ \n\t\r\(\)\[\]\{\}]+")

(defn tokenize [s]
  (cond 
    (re-find #"^[ \n\t\r]" s) {:token :ws   :value s}
    (re-find #"^\"" s)        {:token :str  :value s}
    (re-find #"^;" s)         {:token :comment  :value s}
    (re-find #"^::" s)        {:token :qkw  :value s}
    (re-find #"^:" s)         {:token :kw  :value s}
    (re-find #"^[\~\`\'\@]" s) {:token :reader  :value s}
    (re-find #"^[1-9][0-9]*" s)         {:token :int  :value s}
    (re-find #"^\(" s)        {:token :po   :value s :open true}
    (re-find #"^\[" s)        {:token :vo   :value s :open true}
    (re-find #"^\{" s)        {:token :mo   :value s :open true}
    (re-find #"^\)" s)        {:token :pc   :value s :close true}
    (re-find #"^\]" s)        {:token :vc   :value s :close true}
    (re-find #"^\}" s)        {:token :mc   :value s :close true}
    :else {:token :symbol :value s}))

(defn parse [s]
  (->> s
    (re-seq token:rx)
    (map first)
    (map tokenize)
    (highlite)))