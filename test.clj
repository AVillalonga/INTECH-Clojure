;; (defn compter-mots-frequents [mots]
;;   (reduce (fn [resultat mot]
;;             (update resultat mot (fnil inc 0)))
;;           {} mots))

(defn compter-mots-frequents [mots]
  (reduce (fn [acc key]
            (if (contains? acc key) 
              (update acc key inc) 
              (apply acc key))) {} mots))

(println (compter-mots-frequents ["apple" "banana" "apple" "orange" "banana" "apple"])); Renvoie {"apple" 3, "banana" 2, "orange" 1}