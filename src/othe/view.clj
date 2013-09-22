;;;MVCのV。見た目と、ユーザアクション検出を担当。
(ns othe.view
  (:use
    othe.model                        ;Mに依存。
    [clojure.string :only (join)]))

;;;ユーザからのコマンドのハンドラ。
;;ユーザのコマンド入力を検出したときにコールバックする。
;;引数はベクタで、最初の要素は、:moveか:exit。
;;:moveの場合、第2要素にposを指定する。
(def command-handler)

;;;桁見出しのシーケンス。アルファベット小文字で。
;;;b-sizeが26を超えると対応できない。
(def code-a 97)              ;\aの文字コード。
(def code-curly 123)         ;\{の文字コード。\{は、\zの次の文字。
(def col-headers
  (take b-size (map (comp str char) (range code-a code-curly))))

;;;桁見出し文字列。
(def col-header-str (str "  " (join " " col-headers)))

;;;区切り線。
(def separator (join (repeat 50 \-)))

(defn- st-str
  "マスの状態を表す文字列。"
  [st]
  (cond
    (= st :b) "x"
    (= st :w) "o"
    :else " "))

(defn- score-str
  "スコア文字列。"
  [bs ws]
  (let [s (str "BLACK(x):" bs ", WHITE(o):" ws)]
    (format "%50s" s)))

(defn- winner-str
  "勝者文字列。"
  [bs ws]
  (cond
    (> bs ws) "Winner is BLACK. Congratulations!"
    (> ws bs) "Yeah, WHITE won!!!"
    :else "It's a draw game."))

(defn- board-strs
  "文字列シーケンス。ボードの各行をレンダリングしたもの。"
  [brd]
  (for [row (partition b-size brd)]
    (join " " (map st-str row))))

(defn- board-strs-with-row
  "board-strsに行番号を付与したもの。
行番号は1から開始。2桁には対応できない。"
  [brd]
  (map str
    (range (inc first-row) (inc last-row))
    (repeat b-size " ")
    (board-strs brd)))

(defn- redraw-board
  "盤面を表示。"
  []
  (println col-header-str)
  (dorun (map println (board-strs-with-row (retrieve-board)))))

(defn- col-from-line
  "ユーザ入力から桁を解読。"
  [line]
  (.indexOf col-headers (subs line 0 1)))

(defn- row-from-line
  "ユーザ入力から行を解読。"
  [line]
  (dec (read-string (subs line 1))))

(defn- pos-from-line
  "ユーザ入力からposを解読。不正な入力値ならnil。"
  [line]
  (when (re-find #"^[a-h][1-8]$" line)            ;この正規表現は8x8の盤専用。
    (let [r (row-from-line line)
          c (col-from-line line)]
      (pos-from-rowcol r c))))

(defn- read-cmd
  "stdinから、コマンドを読む。"
  []
  (print (if (is-black-turn?) "It's BLACK's turn: " "Hey WHITE, your turn: "))
  (flush)
  (read-line))

(defn- wait-for-cmd
  "ユーザ入力を待ち、nilかposを返す。
まともな入力でなければ、警告して繰り返す。"
  []
  (loop [line (read-cmd)]
    (if (empty? line)
      (println "Exiting...")
      (if-let [pos (pos-from-line line)] pos
        (do
          (print "Input should be like a1 or b2. Or Enter to exit: ")
          (flush)
          (recur (read-cmd)))))))

(defn- view-thread
  "ユーザ入力を監視するスレッド。入力が空だったら終了。"
  []
  (loop [pos (wait-for-cmd)]
    (when pos
      (command-handler [:move pos])
      (recur (wait-for-cmd))))
  (command-handler [:exit]))

(defn on-state-changed
  "Model変化時のハンドラ。"
  [& e]
  (if e
    (print "You can't move there. Input again: ")
    (let [bs (count-blacks) ws (count-whites)]
      (println separator)
      (println (score-str bs ws))
      (redraw-board)
      (when (is-game-over?)
        (println (str "GAME OVER: " (winner-str bs ws)))
        (command-handler [:exit])))))

(defn init-view
  "Viewを初期化。handlerは、ユーザコマンドのハンドラ。"
  [handler]
  (println "Welcome to the Battle Zone!")
  (println "'x' is Black and 'o' is White.")
  (println "Input the column name first, like 'a1' or 'b2'")
  (println "Just hit Enter to exit.")
  (def command-handler handler))

(defn start-ui
  "ユーザとのインタラクションを開始。"
  []
  (.start (Thread. view-thread)))
