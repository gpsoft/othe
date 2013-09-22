;;;モデル。MVCのM。
;;;VにもCにも依存しない。
(ns othe.model)

;;;用語について。
;;;  pos      盤面の位置。左上が0で、右へ増えていく。右端で左へ折り返す。
;;;            0  1  2  3  4  5  6  7
;;;            8  9 10 11 12 13 14 15
;;;           16 17 18 19 ...
;;;                       ...61 62 63
;;;           
;;;  brd      盤面の状態をベクタで管理する。
;;;           posをインデックスにして、各マスの状態にアクセスする。
;;;  st       ひとマスの状態。:bか:wか:freeのどれか。
;;;  col      桁。0から開始。
;;;  row      行。0から開始。
;;;  opr      マスの状態を変更するときの、posとstのペア。
;;;  oprs     posをキーとし、stを値としたマップ。
;;;  dir      方角。あるマスに対して、その上とか右斜め下とかを表現する。
;;;  posline  ある位置からある方角を眺めたときに見えるposのベクタ。
;;;           例えば、2から南西を眺めると、[9 16]が見える。
;;;  bw       Black or White。:bか:wを取る引数の名前とかに使う。


;;;ボードのメトリクス。
(def b-size 8)                    ;一辺のサイズ。
(def first-pos 0)
(def last-pos (* b-size b-size))  ;exclusive.
(def all-pos (range first-pos last-pos))
(def first-col 0)
(def last-col b-size)             ;exclusive.
(def first-row 0)
(def last-row b-size)             ;exclusive.

(def dirs #{:n :ne :e :se :s :sw :w :nw})    ;方角。

;;;mutableなデータ。
(def board (ref []))              ;各マスの状態。
(def player (ref nil))            ;次の番手。:bか:w。

;;Viewのハンドラ。
;;Modelが変化したときにコールバックする。
;;通常は引数なし。
;;打てない場所に打とうとしたとき、引数に :errを指定。
(def observer)

;;;row/colとposの変換。
(defn col-from-pos [pos] (mod pos b-size))
(defn row-from-pos [pos] (quot pos b-size))
(defn pos-from-rowcol [r c] (+ (* r b-size) c))

;;;基本的な述語など。
(defn- in-board? [pos] (and (>= pos first-pos) (< pos last-pos)))
(defn- free? [brd pos] (= (brd pos) :free))
(defn- self? [brd pos bw] (and (not (free? brd pos)) (= (brd pos) bw)))
(defn- opponent? [brd pos bw] (and (not (free? brd pos)) (not= (brd pos) bw)))
(defn- opponent [bw] (if (= bw :b) :w :b))

;;;引数posから、ある方角へ1マス移動した後のposを得るラムダ。
;;;方角をキーとしたマップで管理。
(def successor
  (let [north (fn [pos] (- pos b-size))
        east inc
        south (fn [pos] (+ pos b-size))
        west dec]
    {:n north
     :ne (comp north east)
     :e east
     :se (comp south east)
     :s south
     :sw (comp south west)
     :w west
     :nw (comp north west)}))

;;;引数posからある方向へ連続して移動するときに、
;;;左右の端で折り返してないことを確認するラムダ。
;;;方角をキーとしたマップで管理。
;;;北と南へ移動するときは、絶対に折り返さないね。
(def not-wrapped?
  (let [east? (fn [pos] (> (col-from-pos pos) first-col))
        west? (fn [pos] (< (col-from-pos pos) (dec last-col)))]
    {:n identity          ;never wrap.
     :ne east?
     :e east?
     :se east?
     :s identity         ;never wrap.
     :sw west?
     :w west?
     :nw west?}))

(defn- posline-for-dir
  "posにおけるdir方向へのposline。"
  [pos dir]
  (let [suc (successor dir)
        nwrap? (not-wrapped? dir)]
    (take-while (fn [pos] (and (nwrap? pos) (in-board? pos)))
      (iterate suc (suc pos)))))

(defn- all-poslines
  "posにおける、各方角へのposlineを集めたシーケンス。"
  [pos]
  (filter not-empty (for [dir dirs] (posline-for-dir pos dir))))

(def initial-oprs
  "ゲームの初期状態(:bと:wが2個ずつ)を表すoprのマップ。"
  (let [cntr (dec (quot b-size 2))
        pos (pos-from-rowcol cntr cntr)]
    {pos :b
     ((successor :se) pos) :b
     ((successor :e) pos) :w
     ((successor :s) pos) :w}))

(defn- board-manipulator
  "oprのマップに基づいて、盤面を変更するラムダ。
このラムダをboardへmap-indexedすると、変更後の盤面が得られる。"
  [oprs]
  (fn [pos st] (if-let [s (oprs pos)] s st)))

(defn- manipulated-board
  "manipulatorを盤面に対して呼んだあとの、新しい盤面。"
  [brd manip]
  (vec (map-indexed manip brd)))

(defn- clamping?
  "bwにとって、poslineは、挟めるか?"
  [brd posline bw]
  (and
    (opponent? brd (first posline) bw) ;poslineの先頭が敵で、かつ
    (if-let [fst (first (filter (fn [pos] (not (opponent? brd pos bw))) (rest posline)))]
      (self? brd fst bw) nil)))        ;poslineの残りから敵地以外のマスを探し
                                       ;最初に見つけたのが自陣ならOK。

(defn- playable?
  "bwにとって、posは打てる場所か?"
  [brd pos bw]
  (and (free? brd pos)                 ;posが空きマスで、かつ
    (some (fn [pl] (clamping? brd pl bw)) (all-poslines pos))))
                                       ;どっかの方角で挟めるならOK。

(defn- has-pos-to-play?
  "bwにとって、打てる場所はあるか?"
  [brd bw]
  (not-empty (filter (fn [pos] (playable? brd pos bw)) all-pos)))

(defn- make-oprs
  "poslineに関して、bwにとってのoprを計算する。
引数には、clamping?がtrueなposlineを指定すること。"
  [brd posline bw]
  (reduce (fn [m pos] (assoc m pos bw)) {}
    (take-while (fn [pos] (opponent? brd pos bw)) posline)))

(defn- make-all-oprs
  "posにおける全poslineに関して、bwにとってのoprを計算する。"
  [brd pos bw]
  (apply merge
    (cons {pos bw}
      (for [posline (filter (fn [pos] (clamping? brd pos bw)) (all-poslines pos))]
        (make-oprs brd posline bw)))))

(defn- next-player
  "bwの次は誰の番か決める。"
  [bw brd]
  (let [nbw (opponent bw)]
    (if (has-pos-to-play? brd nbw) nbw bw)))

;;;ゲーム状態の取得。
(defn- retrieve-game-state
  "ゲーム状態(黒番なら:b、白番なら:w、ゲーム終了なら:over)。"
  []
  (let [brd @board
        bw @player]
    (if (empty? (filter (fn [pos] (free? brd pos)) all-pos)) :over bw)))
(defn- occupancy
  "bwの陣地の広さ。"
  [brd bw]
  (count (filter (fn [pos] (= (brd pos) bw)) all-pos)))
(defn is-game-over? [] (= (retrieve-game-state) :over))
(defn is-black-turn? [] (= (retrieve-game-state) :b))
(defn count-blacks [] (occupancy @board :b))
(defn count-whites [] (occupancy @board :w))
(defn retrieve-board [] @board)

(defn init-game
  "新しいゲームを始める。
引数obはViewのハンドラ。Modelが変化したときにコールバックする。"
  [ob]
  (dosync
    (let [blank (vec (repeat (- last-pos first-pos) :free))
          manip (board-manipulator initial-oprs)]
      (ref-set board (manipulated-board blank manip)))
    (ref-set player :b)           ;最初は黒番。
    (def observer ob))
  (observer))

(defn play-move
  "posへ打つ。"
  [pos]
  (dosync
    (if (not (playable? @board pos @player))
      (observer :err)
      (do
        (let [manip (board-manipulator (make-all-oprs @board pos @player))]
          (alter board manipulated-board manip))
        (alter player next-player @board)
        (observer)))))
