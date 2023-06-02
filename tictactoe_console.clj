;; @project: Tic tac toe (console) - 2023
;; @authors: Adrien Villalonga, Baptiste Goni

;; DEPENDENCIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[clojure.string :as str])

;; GLOBALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def LOGO [" ▄█        ▄█        ▄██████▄   ▄█    █▄     ▄████████       ▄████████  ▄█        ▄██████▄       ▄█ ███    █▄     ▄████████    ▄████████ "
           "███       ███       ███    ███ ███    ███   ███    ███      ███    ███ ███       ███    ███     ███ ███    ███   ███    ███   ███    ███ "
           "███▌      ███       ███    ███ ███    ███   ███    █▀       ███    █▀  ███       ███    ███     ███ ███    ███   ███    ███   ███    █▀  "
           "███▌      ███       ███    ███ ███    ███  ▄███▄▄▄          ███        ███       ███    ███     ███ ███    ███  ▄███▄▄▄▄██▀  ▄███▄▄▄     "
           "███▌      ███       ███    ███ ███    ███ ▀▀███▀▀▀          ███        ███       ███    ███     ███ ███    ███ ▀▀███▀▀▀▀▀   ▀▀███▀▀▀     "
           "███       ███       ███    ███ ███    ███   ███    █▄       ███    █▄  ███       ███    ███     ███ ███    ███ ▀███████████   ███    █▄  "
           "███       ███▌    ▄ ███    ███ ███    ███   ███    ███      ███    ███ ███▌    ▄ ███    ███     ███ ███    ███   ███    ███   ███    ███ "
           "█▀        █████▄▄██  ▀██████▀   ▀██████▀    ██████████      ████████▀  █████▄▄██  ▀██████▀  █▄ ▄███ ████████▀    ███    ███   ██████████ "
           "          ▀                                                            ▀                    ▀▀▀▀▀▀               ███    ███              "
           "                                                                                                                                         "
           "                                                                                                                                         "
           "                                                                                                                                         "
           "    ███      ▄█   ▄████████          ███        ▄████████  ▄████████          ███      ▄██████▄     ▄████████                            "
           "▀█████████▄ ███  ███    ███      ▀█████████▄   ███    ███ ███    ███      ▀█████████▄ ███    ███   ███    ███                            "
           "   ▀███▀▀██ ███▌ ███    █▀          ▀███▀▀██   ███    ███ ███    █▀          ▀███▀▀██ ███    ███   ███    █▀                             "
           "    ███   ▀ ███▌ ███                 ███   ▀   ███    ███ ███                 ███   ▀ ███    ███  ▄███▄▄▄                                "
           "    ███     ███▌ ███                 ███     ▀███████████ ███                 ███     ███    ███ ▀▀███▀▀▀                                "
           "    ███     ███  ███    █▄           ███       ███    ███ ███    █▄           ███     ███    ███   ███    █▄                             "
           "    ███     ███  ███    ███          ███       ███    ███ ███    ███          ███     ███    ███   ███    ███                            "
           "   ▄████▀   █▀   ████████▀          ▄████▀     ███    █▀  ████████▀          ▄████▀    ▀██████▀    ██████████                            "
           "                                                                                                                                         "
           "                                                                                                                                         "
           "                                                                                                                                         "])

(def BOARD ["   A B C" "  ┏━┳━┳━┓" "1 ┃§┃§┃§┃ 1" "  ┣━╋━╋━┫" "2 ┃§┃§┃§┃ 2" "  ┣━╋━╋━┫" "3 ┃§┃§┃§┃ 3" "  ┗━┻━┻━┛" "   A B C"])
(def GAME [0 0 0 0 0 0 0 0 0])
(def GAME_WINS [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [6 4 2]])
(def BINDS {"A1" 0 "B1" 1 "C1" 2 "A2" 3 "B2" 4 "C2" 5 "A3" 6 "B3" 7 "C3" 8})

;; DISPLAY FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn display-logo []
  (doseq [row LOGO]
    (println row)))

(defn val-sym [vx]
  (if (= vx 0) " "
      (if (= vx 1) "O"
          (if (= vx 2) "X" " "))))

(defn replace-row-values [row val-a val-b val-c]
  (str/replace-first
   (str/replace-first
    (str/replace-first row "§" (val-sym val-a))
    "§" (val-sym val-b))
   "§" (val-sym val-c)))

(defn display-board [game]
  (doseq [[index element] (map-indexed vector BOARD)]
    (when (= index 2)
      (println (replace-row-values element (nth game 0) (nth game 1) (nth game 2))))
    (when (= index 4)
      (println (replace-row-values element (nth game 3) (nth game 4) (nth game 5))))
    (when (= index 6)
      (println (replace-row-values element (nth game 6) (nth game 7) (nth game 8))))
    (when (and (not= index 2) (not= index 4) (not= index 6))
      (println element))))

(defn clr []
  (doseq [i (range 0 32)]
    (println "")))

;; GAME FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-tour [game player-id pos]
  (if (= (nth game pos) 0)
    (update-in game [pos] (constantly player-id))
    false))

(defn play-tour [game, player-id]
  (println "select:")
  (let [user-input (clojure.string/upper-case (read-line))]
    (if (contains? BINDS user-input)
      (let [pos (nth (find BINDS user-input) 1)
            ar (apply-tour game player-id pos)]
        (do (if (= ar false)
              (play-tour game player-id)
              ar)))
      (play-tour game player-id))))

(defn is-win [win game]
  (if (and (= (nth game (nth win 0)) (nth game (nth win 1)))
           (= (nth game (nth win 1)) (nth game (nth win 2)))
           (= (nth game (nth win 2)) (nth game (nth win 0)))
           (not= (nth game (nth win 0)) 0))
    (if (= (nth game (nth win 0)) 1)
      1
      2)
    0))

(defn is-win-recursive [game-wins game]
  (if (empty? game-wins)
    0
    (if (not= (is-win (first game-wins) game) 0)
      (is-win (first game-wins) game)
      (recur (rest game-wins) game))))

;; GAME LOOP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn game-loop [game player-id tour skip-clr]
  (when (= skip-clr false)
    (clr))
  (if (not= (is-win-recursive GAME_WINS game) 0)
    (do (display-board game)
     (println "\nPlayer n°" (is-win-recursive GAME_WINS game) "win the game"))
    (do (display-board game)
        (println "\n=== PLAYER n°" player-id " ===")
        (let [ngame (play-tour game player-id)]
          (if (= player-id 1)
            (game-loop ngame 2 (inc tour) false)
            (game-loop ngame 1 (inc tour) false))))))

(display-logo)
(game-loop GAME 1 0 true)
