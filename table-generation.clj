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

(def vowel-rel
     '(("a" "#" "E" "D" "C")
       ("e" "$" "R" "F" "V")
       ("i" "%" "T" "G" "B")
       ("o" "^" "Y" "H" "N")
       ("u" "&" "U" "J" "M")
       ("y" "È" "É" "Ê" "Ë")))

(defn get-firsts [coll]
  (map #(first %) coll))

(defn lowercase-cons []
  (remove #(or (= % "")
	       (= % ".")) (get-firsts cons-rel)))

(defn uppercase-cons []
  (map #(.toUpperCase %) (lowercase-cons)))

(defn lget [coll key]
  (first (filter #(= key (first %)) coll)))

(defn containsUpperCase [s]
  (if (= s (.toLowerCase s))
    false
    true))

(defn transcribe-consonant [jbo]
  (if (containsUpperCase jbo)
    (format "%s%s" (second (lget cons-rel (.toLowerCase jbo))) \")
    (second (lget cons-rel jbo))))

;1:	2:	3:	4:
;d	t	f	s
;b	p		""
;g	k	
;v	c	
;j	x	
;n	r
;m	
;l	
;z	


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

(defn transcribe-vowel [jbo-vowel prec-jbo-consonant]
  (nth
   (lget vowel-rel jbo-vowel)
   (get-upper-vowel-position prec-jbo-consonant)))

;Note: there MUST be some library function defined like that already!
;Note: it's concat...
;(defn append [coll1 coll2]
;  (if (= coll1 '())
;    coll2
;    (cons (first coll1) (append (rest coll1) coll2))))

(defn combine-cons-vowel [cons vowel]
  (list
   (format "%s%s" cons vowel)
   (format "%s%s"
	   (transcribe-consonant cons)
	   (transcribe-vowel (.toLowerCase vowel) (.toLowerCase cons)))))

(defn generate-table []
  ;;Uppercase solitary consonants
  (concat (map #(list % (transcribe-consonant %)) (uppercase-cons))
	  ;;Uppercase consonant-vowel pairs
	  (for [consonant (uppercase-cons)
		vowel (get-firsts vowel-rel)]
	    (combine-cons-vowel consonant vowel))
	  ;;Lowercase solitary consonants
	  (map #(list % (transcribe-consonant %)) (lowercase-cons))
	  ;;Lowercase consonant-vowel pairs
	  (for [consonant (get-firsts cons-rel)
		vowel (get-firsts vowel-rel)]
	    (combine-cons-vowel consonant vowel))
	  '()))
