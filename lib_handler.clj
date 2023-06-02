;; Programmation Fonctionnelle
;; @project: TP N°2 - Lib handler (console) - 2023
;; @authors: Adrien Villalonga, Baptiste Goni
;; ================================================================ 
;; Gestion d’une bibliothèque
;; Note : Les fonctions demandées doivent être purement fonctionnelles (sans affectation), ce qui
;;        ne veut pas dire qu’on ne s’autorise pas à les utiliser avec des références (atom) :
;;        Supposons que vous deviez développer une application pour gérer une bibliothèque. La
;;        bibliothèque possède une collection de livres, chaque livre a un titre, un auteur, un éditeur, une
;;        date de publication, un nombre d’exemplaires disponibles et un numéro ISBN (qui peut servir
;;        de clef primaire). La bibliothèque a également des lecteurs, dont on connaît le nom, le prénom,
;;        l’adresse et la liste des emprunts, et un identifiant qui est le hash de la concaténation du nom et
;;        du prénom). Un lecteur peut emprunter un livre pour une période limitée, seulement si il est
;;        disponible (nombre d’exemplaires disponibles supérieur strictement à 0).
;;        Pour représenter ces données dans votre application, vous décidez d'utiliser des
;;        enregistrements Clojure pour les livres et les lecteurs. La collection des livres et la liste des
;;        lecteurs seront intégrées dans des atoms clojure.
;; ================================================================

;; (def date (java.util.Date.))

(defn now []
  (quot (System/currentTimeMillis) 1000))

;; Debug functions

(defn display-book [book]
  (println "................................................")
  (println "Title\t:" (:title book) "\nAuthor\t:" (:author book) "\nEditor\t:" (:editor book) "\nSince\t:" (:since book) "\nISBN\t:" (:isbn book) "\nCount\t:" (:count book)))

(defn display-books [books]
  (doseq [book books]
    (display-book book)))

(defn display-borrower [borrower]
  (println "————————————————————————————————————————————————————————————")
  (println "Lastname\t:" (:lastname borrower) "\nFirstname\t:" (:firstname borrower) "\nAddress\t\t:" (:address borrower) "\nHash\t\t:" (:hash borrower))
  (println "Borrowed Books\t:" (:books borrower)))

(defn display-borrowers [borrowsers]
  (doseq [borrower borrowsers]
    (display-borrower borrower)))

;; 1. Définissez l'enregistrement Book avec les champs suivants: titre, auteur, éditeur, date-de-publication, isbn et exemplaires-disponibles.

(defrecord Book [title author editor since isbn count])

;; 2. Créez une fonction appelée create-book qui prend cinq arguments représentant les champs d'un enregistrement Book, et renvoie un nouvel enregistrement Book avec exemplaires-disponibles initialisé à 1.

(defn create-book
  ([title author editor since isbn]
   (->Book title
           author
           editor
           since
           isbn
           1))
  ([title author editor since isbn count]
   (->Book title
           author
           editor
           since
           isbn
           count)))

(def library (atom (list (create-book "Le monde s'effondre" "Chinua Achebe" "Nigeria" 1958 "123456789")
                         (create-book "Contes" "Hans Christian Andersen" "Danemark" 1837 "23456789")
                         (create-book "Orgueil et Préjugés" "Jane Austen" "Royaume-Uni" 1813 "3456789")
                         (create-book "Le Père Goriot" "Honoré de Balzac" "France" 1835 "456789")
                         (create-book "Décaméron" "Boccace" "Italie" 1353 "56789")
                         (create-book "Décaméron2" "Boccace" "Italie" 1353 "56789")
                         (create-book "Fictions" "Jorge Luis Borges" "Argentine" 1944 "6789"))))

;; 3. Créez une fonction appelée list-books-by-author qui prend une liste d'enregistrements Book et un nom d'auteur en argument, et renvoie une nouvelle liste d'enregistrements Book qui ont l'auteur spécifié.

(defn list-book-by-author [author books]
  (filter (fn [book] (= (:author book) author)) books))

;; (display-books (list-book-by-author "Boccace" @library)) ;; <<< TEST

;; 4. Créez une fonction appelée sort-books-by-date qui prend une liste d'enregistrements Book en argument et renvoie une nouvelle liste d'enregistrements Book triés par date de publication, du plus ancien au plus récent.

(defn sort-books-by-date [books]
  (sort-by :since books))

;; (display-books (sort-books-by-date @library)) ;; <<< TEST

;; 5. Créez une fonction appelée get-book-isbn qui prend un enregistrement Book en argument et renvoie le numéro ISBN du livre.

(defn get-book-isbn [book]
  (:isbn book))

;; (println (get-book-isbn (first @library))) ;; <<< TEST

;; 6. Créez une fonction appelée set-book-isbn qui prend un enregistrement Book et un nouveau numéro ISBN en argument, et renvoie un nouvel enregistrement Book avec le numéro ISBN mis à jour.

(defn set-book-isbn [isbn book]
  (assoc book :isbn isbn))

;; (display-book (first @library)) (display-book (set-book-isbn 321 (first @library)))  ;; <<< TEST

;; 7. Créez une fonction appelée print-book qui prend un enregistrement Book en argument et imprime le titre, l'auteur, l'éditeur, la date de publication et le numéro ISBN du livre sous une forme lisible.

(defn print-book [book] (display-book book)) ;; Voir la fonction permettant les testes (defn display-book)

;; (print-book (first @library))  ;; <<< TEST

;; 8. Définissez l'enregistrement Borrower avec les champs suivants: nom, prénom, adresse et livres-empruntés.

(defrecord Borrower [lastname firstname address books hash])

;; Use to handle last mounth book (borrowed)

(defrecord Borrow [hash isbn date returned])

(defn create-borrow
  ([hash isbn]
   (->Borrow hash isbn (now) false))
  ([hash isbn date returned]
   (->Borrow hash isbn date returned)))

(def borrows (atom (list)))

;; 9. Créez une fonction appelée create-borrower qui prend quatre arguments représentant les champs d'un enregistrement Borrower, et renvoie un nouvel enregistrement Borrower avec une liste vide de livres-empruntés (pour le hashage, voir https://clojuredocs.org/clojure.core/hash)

(defn create-borrower
  ([lastname firstname address]
   (->Borrower lastname
               firstname
               address
               (list)
               (hash (str lastname firstname))))
  ([lastname firstname address books hash]
   (->Borrower lastname
               firstname
               address
               books
               hash)))

(def borrowers (atom (list (create-borrower "Villalonga" "Adrien" "51 rue du père lachaise")
                           (create-borrower "Goni" "Baptiste" "71 rue des pyramides")
                           (create-borrower "Super" "Man" "Porte des étoile"))))

;; (display-borrowers @borrowers) ;; <<< TEST

;; 10. Créez une fonction appelée borrow-book qui prend une clef d’emprunteur, l’ISBN d’un livre et une date de prêt en argument. La fonction doit ajouter le livre à la liste des livres-empruntés du lecteur et décrémenter le champ exemplaires-disponibles du livre, seulement si le nombre d’exemplaires disponibles est supérieur à 0.

(defn borrow-book [borrower-hash isbn]
  (let [targets (atom (list))]
    (doseq [book @library]
      (when (and (> (:count book) 0) (= (:isbn book) isbn))
        (doseq [borrower @borrowers]
          (when (= (:hash borrower) borrower-hash)
            (reset! targets (list (create-book (:title book) (:author book) (:editor book) (:since book) (:isbn book) (dec (:count book)))
                                  (create-borrower (:lastname borrower) (:firstname borrower) (:address borrower) (conj (:books borrower) isbn) (:hash borrower))))))))
    (when (not= (first @targets) nil)
      (let [new-lib (filter #(not= (:isbn %) (:isbn (first @targets))) @library),
            new-borrowers (filter #(not= (:hash %) borrower-hash) @borrowers)]
        (swap! borrows conj (create-borrow (:hash (last @targets)) (:isbn (first @targets))))
        (reset! library (conj new-lib (first @targets)))
        (reset! borrowers (conj new-borrowers (last @targets)))))))

(borrow-book (:hash (first @borrowers)) (:isbn (first @library))) ;; <<< TEST (1/4)
(borrow-book (:hash (first @borrowers)) (:isbn (nth @library 1))) ;; <<< TEST (1/4)
;; (display-borrowers @borrowers) ;; <<< TEST (2/4)
;; (display-books @library) ;; <<< TEST (3/4)
;; (println @borrows) ;; TEST (4/4)

;; 11. Créez une fonction appelée return-book qui prend une clef d’emprunteur et l’ISBN d’un livre en argument. La fonction doit retirer le livre de la liste des livres-empruntés du lecteur, incrémenter le champ exemplaires-disponibles du livre.

(defn return-book [borrower-hash isbn]
  (let [targets (atom (list))]
    (doseq [book @library]
      (when (= (:isbn book) isbn)
        (doseq [borrower @borrowers]
          (when (and (= (:hash borrower) borrower-hash) (> (count (filter #(= isbn %) (:books borrower))) 0))
            (reset! targets (list (create-book (:title book) (:author book) (:editor book) (:since book) (:isbn book) (inc (:count book)))
                                  (create-borrower (:lastname borrower) (:firstname borrower) (:address borrower) (filter #(not= isbn %) (:books borrower)) (:hash borrower))))))))
    (when (not= (first @targets) nil)
      (let [new-lib (filter #(not= (:isbn %) (:isbn (first @targets))) @library),
            new-borrowers (filter #(not= (:hash %) borrower-hash) @borrowers),
            new-borrows (filter #(and (not= borrower-hash (:hash %)) (not= isbn (:isbn %) (not= false (:returned %)))) @borrows)]
        (let [borrow-row (first (filter #(and (= borrower-hash (:hash %)) (= isbn (:isbn %)) (= false (:returned %))) @borrows))]
          (when (not= borrow-row nil)
            (reset! borrows (conj new-borrows (create-borrow (:hash borrow-row) (:isbn borrow-row) (:date borrow-row) true)))))
        (reset! library (conj new-lib (first @targets)))
        (reset! borrowers (conj new-borrowers (last @targets)))))))

(return-book (:hash (first @borrowers)) (:isbn (first @library))) ;; <<< TEST (1/4)
;; (display-borrowers @borrowers) ;; <<< TEST (2/4)
;; (display-books @library) ;; <<< TEST (3/4)
;; (println "\n\n" @borrows) ;; <<< TEST (3/4)

;; 12. Créer une fonction appelée late-return qui renvoie la liste des emprunts (livre et emprunteur) datant de plus de 1 mois.

(defn late-return []
  (let [sinceOneMounth (- (now) 2419200)]
    (filter #(> (:date %) sinceOneMounth) @borrows)))

;; (println (late-return)) ;; <<< TEST

;; 13. Ecrire une fonction appelée save-lib qui sauvegarde la liste des livres et la liste des emprunteurs dans des fichiers (https://clojuredocs.org/clojure.java.io/writer)

(defn save-lib []
  (spit "./lib.latest.uq" @library)
  (spit "./bor.latest.uq" @borrows)
  (spit "./customers.latest.uq" @borrowers))

(save-lib) ;; <<< TEST

;; 14. Ecrire une fonction appelée load-lib qui charge la liste des livres et la liste des emprunteurs depuis des fichiers (https://clojuredocs.org/clojure.java.io/reader)

(defn load-lib []
  (reset! library (read-string (slurp "./lib.latest.uq")))
  (reset! borrows (read-string (slurp "./bor.latest.uq")))
  (reset! borrowers (read-string (slurp "./customers.latest.uq"))))

(load-lib) ;; <<< TEST (1/4)
;; (display-books @library) ;; <<< TEST (2/4)
;; (display-borrowers @borrowers) ;; <<< TEST (3/4)
;; (println @borrows) ;; <<< TEST (4/4)

;; Et pour finir, une petite interface utilisateur (en mode texte) pour accéder aux fonctionnalités de la bibliothèque.

(defn CLI-MENU-DISPLAY []
  (println "\n\nWhat you whant ?")
  (println "\t1. Load")
  (println "\t2. Save")
  (println "\t3. Late return")
  (println "\t4. Display Books")
  (println "\t5. Display Borrowers")
  (println "\t6. Display History")
  (println "\t7. Exit"))

(defn CLI []
  (CLI-MENU-DISPLAY)
  (println "?>")
  (let [input (Integer/parseInt (read-line))]
    (println "\n\n\n")
    (when (= input 1)
      (load-lib))
    (when (= input 2)
      (save-lib))
    (when (= input 3)
      (println (late-return)))
    (when (= input 4)
      (display-books @library))
    (when (= input 5)
      (display-borrowers @borrowers))
    (when (= input 6)
      (println @borrows))
    (when (not= input 7)
      (CLI))))

(CLI)

;; EOF