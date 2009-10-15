;the consonant-transcription-table
(def cons-rel
     '(("t" "1") ("p" "q")           ("k" "z")
       ("d" "2") ("b" "w")           ("g" "x")
       ("f" "e") ("c" "d") ("x" "c")
       ("v" "r") ("j" "f")
       ("n" "5") ("m" "t")
       ("r" "6")
       ("l" "j")
       ("s" "K")           ("z" "k")
       ("" "`")
       ("." "`")
       ("'" "~")))

;the same for vowels, but here we have four different positions for each
(def vowel-rel
     '(("a" "#" "E" "D" "C")
       ("e" "$" "R" "F" "V")
       ("i" "%" "T" "G" "B")
       ("o" "^" "Y" "H" "N")
       ("u" "&" "U" "J" "M")
       ("y" "È" "É" "Ê" "Ë")))

;takes a list of lists and returns a
;list of the first elements of the nested lists
(defn get-firsts [coll]
  (map #(first %) coll))

;returns a list of all consonants, lowercase
(defn lowercase-cons []
  (remove #(or (= % "")
	       (= % ".")) (get-firsts cons-rel)))

;returns a list of all consonants, uppercase
(defn uppercase-cons []
  (map #(.toUpperCase %) (lowercase-cons)))

;works similar to assoc from common lisp:
;takes a list of lists and a 'key',
;returns the first list that has 'key' as the first element
(defn lget [coll key]
  (first (filter #(= key (first %)) coll)))

;returns true if the string contains any uppercase letters
(defn containsUpperCase [s]
  (if (= s (.toLowerCase s))
    false
    true))

;transcribes a consonant using the consonant transcription table
;uppercase consonants being interpreted as lowercase
(defn transcribe-consonant [jbo]
  (if (containsUpperCase jbo)
    (format "%s%s" (second (lget cons-rel (.toLowerCase jbo))) \")
    (second (lget cons-rel jbo))))

;returns the correct position of a tengwar vowel above a consonant c
(defn get-upper-vowel-position [c]
  (cond (or
	 (= c "d")
	 (= c "b")
	 (= c "g")
	 (= c "v")
	 (= c "j")
	 (= c "n")
	 (= c "m")
	 (= c "l")
	 (= c "z")) 1
	(or
	 (= c "t")
	 (= c "p")
	 (= c "k")
	 (= c "c")
	 (= c "x")
	 (= c "r")) 2
	(= c "f") 3
	(or
	 (= c "s")
	 (= c "")
	 (= c ".")
	 (= c "'")) 4))

;transcribes a vowel, uses the vowel position according
;to prec(eding)-jbo-consonant
(defn transcribe-vowel [jbo-vowel prec-jbo-consonant]
  (nth
   (lget vowel-rel jbo-vowel)
   (get-upper-vowel-position prec-jbo-consonant)))

;takes a consonant and a vowel and returns a full transcription of the pair
(defn combine-cons-vowel [cons vowel]
  (list
   (format "%s%s" cons vowel)
   (format "%s%s"
	   (transcribe-consonant cons)
	   (transcribe-vowel (.toLowerCase vowel) (.toLowerCase cons)))))

;generates the final transcription table
(defn generate-table []
  (concat
   ;;Uppercase solitary consonants
   (map #(list % (transcribe-consonant %)) (uppercase-cons))
   ;;Uppercase consonant-vowel pairs
   (for [consonant (uppercase-cons)
	 vowel (get-firsts vowel-rel)]
     (combine-cons-vowel consonant vowel))
   ;;Lowercase solitary consonants
   (map #(list % (transcribe-consonant %)) (lowercase-cons))
   ;;Lowercase consonant-vowel pairs
   (for [consonant (get-firsts cons-rel)
	 vowel (get-firsts vowel-rel)]
     (combine-cons-vowel consonant vowel))))
