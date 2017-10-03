(defrule startup 
  =>
  (printout t "Welcome to our system. Where do you want to eat today?" crlf)
  (printout t "What is your name?" crlf)
    (bind ?uname (read))
  (printout t "Do you smoke? (yes or no)" crlf)
    (bind ?smoke (readline))
    (if (= (str-compare ?smoke "") 0)
    then
    (bind ?smoke "no"))
  (printout t "What is your minimum budget? (0-9999)" crlf)
    (bind ?minbud (read))
    (if (or (stringp ?minbud) (symbolp ?minbud))
    then
    (bind ?minbud 400))
  (printout t "What is your maximum budget? (0-9999)" crlf)
    (bind ?maxbud (read))
    (if (or (stringp ?maxbud) (symbolp ?maxbud))
    then
    (bind ?maxbud 1000))
  (printout t "What clothes are you wearing? (casual, informal, formal)" crlf)
    (bind ?cloth (readline))
    (if (= (str-compare ?cloth "") 0)
    then
    (bind ?cloth "casual"))
  (printout t "Do you want restaurant with Wi-Fi? (yes or no)" crlf)
    (bind ?wifi (readline))
    (if (= (str-compare ?wifi "") 0)
    then
    (bind ?wifi "yes"))
  (printout t "What are your latitude coordinate?" crlf)
    (bind ?lat (read))
    (if (or (stringp ?lat) (symbolp ?lat))
    then
    (bind ?lat 0.0))
  (printout t "What are your longitude coordinate?" crlf)  
    (bind ?long (read))
    (if (or (stringp ?long) (symbolp ?long))
    then
    (bind ?long 0.0))
  (assert (user-preference (user-name ?uname) (isSmoker (lowcase ?smoke)) (budgetMin ?minbud) 
  (budgetMaks ?maxbud) (dresscode (lowcase ?cloth)) (hasWifi (lowcase ?wifi)) (latitude ?lat) (longitude ?long)))
)

;Matching smoker
(defrule smoking-match
  (user-preference (isSmoker ?smoker1))
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1) (checked 0))
  (restoran (nama ?nama2) (isSmoker ?smoker2))
  =>
  (if (= (str-compare ?nama1 ?nama2) 0)
  then
    (modify ?rek (checked 1))
    (if (= (str-compare ?smoker1 ?smoker2) 0)
    then
      (assert (add-smoker-poin "add"))
    )
  )
)

;Add smoker poin
(defrule add-smoker-poin
  ?smokerpoin <- (add-smoker-poin "add")
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1))
  =>
  (modify ?rek (count (+ ?c1 1)))
  (retract ?smokerpoin)
)


;Matching budget
(defrule budget-match
  (user-preference (budgetMin ?min1) (budgetMaks ?maks1))
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1) (checked 1))
  (restoran (nama ?nama2) (budgetMin ?min2) (budgetMaks ?maks2))
  =>
  (if (= (str-compare ?nama1 ?nama2) 0)
  then
    (modify ?rek (checked 2))
    (if (and (<= ?min1 ?maks2) (>= ?maks1 ?min2))
    then
      (assert (add-budget-poin "add"))
    )
  )
)

;Add budget poin
(defrule add-budget-poin
  ?budgetpoin <- (add-budget-poin "add")
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1))
  =>
  (modify ?rek (count (+ ?c1 1)))
  (retract ?budgetpoin)
)


;Matching Dresscode
(defrule dresscode-match
  (user-preference (dresscode ?dc1))
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1) (checked 2))
  (restoran (nama ?nama2) (dresscode ?dc2))
  =>
  (if (= (str-compare ?nama1 ?nama2) 0)
  then
    (modify ?rek (checked 3))
    (if (= (str-compare ?dc1 ?dc2) 0)
    then
      (assert (add-dresscode-poin "add"))
    )
  )
)

;Add dresscode poin
(defrule add-dresscode-poin
  ?dcpoin <- (add-dresscode-poin "add")
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1))
  =>
  (modify ?rek (count (+ ?c1 1)))
  (retract ?dcpoin)
)

;Matching Wifi
(defrule wifi-match
 (user-preference (hasWifi ?wifi1))
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1) (checked 3))
  (restoran (nama ?nama2) (hasWifi ?wifi2))
  =>
  (if (= (str-compare ?nama1 ?nama2) 0)
  then
    (modify ?rek (checked 4))
   (if (= (str-compare ?wifi1 ?wifi2) 0)
    then
      (assert (add-wifi-poin "add"))
    )
  )
)

;Add wifi poin
(defrule add-wifi-poin
  ?wifipoin <- (add-wifi-poin "add")
  ?rek <- (rekomendasi (nama ?nama1) (count ?c1))
  =>
 (modify ?rek (count (+ ?c1 1)))
  (retract ?wifipoin)
)

;Calculating euclidean distance
(defrule calc-distance
  (restoran (nama ?nama) (longitude ?x2) (latitude ?y2))
  ?rek <- (rekomendasi (nama ?nama) (checked 4))
  (user-preference (longitude ?x1) (latitude ?y1))
  =>
  (assert (calculate-distance "done"))
  (assert (jarak (sqrt(+ (** (- ?x1 ?x2) 2) (** (- ?y1 ?y2) 2)))))
  (assert (modify-distance ?nama))
  (modify ?rek (checked 5))
  (assert (last-check "done"))
)

;modify distance
(defrule mdfy-distance
  ?calc <- (calculate-distance "done")
  ?cek <- (last-check "done")
  ?rek <- (rekomendasi (nama ?nama))
  ?mdf <- (modify-distance ?nama)
  ?dist <- (jarak ?d)
  =>
  (modify ?rek (distance ?d)(checked 6))
  (retract ?mdf)
  (retract ?cek)
  (retract ?calc)
  (retract ?dist)
)

;;;;;;;;;;;;;;;;;;;;;;;kategori

(defrule add-category
  ?rek <- (rekomendasi (nama ?nama) (count ?c) (kategori "") (distance ?d) (printed ?p) (checked 6))
  =>
  (retract ?rek)
  (if (eq ?c 4) then
    (assert (rekomendasi (nama ?nama) (count ?c) (kategori "very recommendable") (distance ?d) (printed ?p) (checked 7))))
  (if (or (eq ?c 3) (eq ?c 2)) then
    (assert (rekomendasi (nama ?nama) (count ?c) (kategori "recommendable") (distance ?d) (printed ?p) (checked 7))))
  (if (or (eq ?c 1) (eq ?c 0)) then
    (assert (rekomendasi (nama ?nama) (count ?c) (kategori "not recommendable") (distance ?d) (printed ?p) (checked 7)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;print output
(defrule find-very-recommendable
   (declare(salience 100))
   (not (rekomendasi (checked ?cek&:(< ?cek 7))))
   (rekomendasi (nama ?nama1)(kategori "very recommendable")(distance ?dist1)(checked 7))
   (not (rekomendasi (kategori "very recommendable")(distance ?dist2&:(< ?dist2 ?dist1))))
   ?f1 <- (rekomendasi (nama ?nama1) (count ?count1)(kategori "very recommendable")(distance ?dist1)(printed ?printed1))
   ?var <- (total-print ?printx&:(< ?printx 3))
   =>
   (printout t ?nama1 " ("?dist1 ") very recommendable" crlf)
   (retract ?f1)
   (retract ?var)
   (assert (total-print (+ ?printx 1)))
)
   
(defrule find-recommendable
   (declare(salience 50))
   (not (rekomendasi (checked ?cek&:(< ?cek 7))))
   (rekomendasi (nama ?nama1)(kategori "recommendable")(distance ?dist1)(checked 7))
   (not (rekomendasi (kategori "recommendable")(distance ?dist2&:(< ?dist2 ?dist1))))
   ?f1 <- (rekomendasi (nama ?nama1) (count ?count1)(kategori "recommendable")(distance ?dist1)(printed ?printed1))
   ?var <- (total-print ?printx&:(< ?printx 3))
   =>
   (printout t ?nama1 " ("?dist1 ") recommendable" crlf)
   (retract ?f1)
   (retract ?var)
   (assert (total-print (+ ?printx 1)))
)
   
(defrule find-not-recommendable
   (declare(salience 25))
   (not (rekomendasi (checked ?cek&:(< ?cek 7))))
   (rekomendasi (nama ?nama1)(kategori "not recommendable")(distance ?dist1)(checked 7))
   (not (rekomendasi (kategori "not recommendable")(distance ?dist2&:(< ?dist2 ?dist1))))
   ?f1 <- (rekomendasi (nama ?nama1) (count ?count1)(kategori "not recommendable")(distance ?dist1)(printed ?printed1))
   ?var <- (total-print ?printx&:(< ?printx 3))
   =>
   (printout t ?nama1 " ("?dist1 ") not recommendable" crlf)
   (retract ?f1)
   (retract ?var)
   (assert (total-print (+ ?printx 1)))
)
