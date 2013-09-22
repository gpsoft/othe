;;;メイン。MVCの"C"に相当。
;;;ユーザのコマンドをViewから受け取り、それを処理する。
(ns othe.core
  (:use
    othe.view                  ;VとMに依存。
    othe.model))

(defn on-command
  "Viewからのコマンド通知を処理するハンドラ。
引数はベクタで、最初の要素は、:moveか:exit。
:moveの場合、第2要素にposを指定する。"
  [cmdline]
  (let [cmd (first cmdline)
        pos (second cmdline)]
    (cond
      (= cmd :move) (play-move pos)   ;1手打つ。
      (= cmd :exit) (System/exit 0)   ;アプリ終了。
      :else nil)))

(defn -main
  "エントリポイント。"
  [& args]
  (init-view on-command)       ;Viewを初期化しつつハンドラを登録。
  (init-game on-state-changed) ;Modelを初期化しつつ、Viewのハンドラを登録。
                               ;ゲームの状態が変わったらViewへ通知が行く。
  (start-ui))                  ;UIを開始(非同期)。
