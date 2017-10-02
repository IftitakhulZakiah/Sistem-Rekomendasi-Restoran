(defrule startup 
  =>
  (printout t "Welcome to our system. Where do you want to eat today?" crlf)
  (printout t "What is your name?" crlf)
    (bind ?uname (read))
  (printout t "Do you smoke? (yes or no)" crlf)
    (bind ?smoke (read))
  (printout t "What is your minimum budget? (0-9999)" crlf)
    (bind ?minbud (read))
  (printout t "What is your maximum budget? (0-9999)" crlf)
    (bind ?maxbud (read))
  (printout t "What clothes are you wearing? (casual, informal, formal)" crlf)
    (bind ?cloth (read))
  (printout t "Do you want restaurant with Wi-Fi? (yes or no)" crlf)
    (bind ?wifi (read))
  (printout t "What are your latitude coordinate?" crlf)
    (bind ?lat (read))
  (printout t "What are your longitude coordinate?" crlf)  
    (bind ?long (read))
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
  (rekomendasi (nama ?nama) (distance 0.0))
  (user-preference (longitude ?x1) (latitude ?y1))
  =>
  (printout t ?nama crlf)
  (assert (calculate-distance "done"))
  (assert (jarak (sqrt(+ (** (- ?x1 ?x2) 2) (** (- ?y1 ?y2) 2)))))
  (assert (modify-distance ?nama))
)

:modify distance
(defrule modify-distance
  ?calc <- (calculate-distance "done")
  ?rek <- (rekomendasi (nama ?nama) (distance 0.0))
  ?mdf <- (modify-distance ?nama)
  ?dist <- (jarak ?d)
  =>
  (modify ?rek (distance ?d))
  (printout t ?nama " " ?d crlf)
  (retract ?mdf)
  (retract ?calc)
  (retract ?dist)
)

;;;;;;;;;;;;;;;;;;;;;;;kategori

(defrule add-category
  ?rek <- (rekomendasi (nama ?nama) (count ?c) (kategori "") (distance ?d) (printed ?p) (checked 4))
  =>
  (retract ?rek)
  (if (eq ?c 4) then
    (assert (rekomendasi (nama ?nama) (count ?c) (kategori "very recommendable") (distance ?d) (printed ?p) (checked 4))))
  (if (or (eq ?c 3) (eq ?c 2)) then
    (assert (rekomendasi (nama ?nama) (count ?c) (kategori "recommendable") (distance ?d) (printed ?p) (checked 4))))
  (if (or (eq ?c 1) (eq ?c 0)) then
    (assert (rekomendasi (nama ?nama) (count ?c) (kategori "not recommendable") (distance ?d) (printed ?p) (checked 4)))
  )
)

;;;;;;;;;;;;;;;;;;;;;;print output
(defrule find-very-recommendable
   (rekomendasi (nama ?nama1)(kategori "very recommendable")(distance ?dist1))
   (not (rekomendasi (distance ?dist2&:(< ?dist2 ?dist1))))
   ?f1 <- (rekomendasi (nama ?nama1) (count ?count1)(kategori ?kategori1)(distance ?dist1)(printed ?printed1))
   ?var <- (total-print ?printx&:(< ?printx 3))
   =>
   (printout t "Restoran " ?nama1 " "?dist1 " "?kategori1 " crlf)
   (retract ?f1)
   (retract ?var)
   (assert (total-print (+ ?printx 1)))
)
   
(defrule find-recommendable
   (rekomendasi (nama ?nama1)(kategori "recommendable")(distance ?dist1))
   (not (rekomendasi (distance ?dist2&:(< ?dist2 ?dist1))))
   ?f1 <- (rekomendasi (nama ?nama1) (count ?count1)(kategori ?kategori1)(distance ?dist1)(printed ?printed1))
   ?var <- (total-print ?printx&:(< ?printx 3))
   =>
   (printout t "Restoran " ?nama1 " "?dist1 " "?kategori1 " crlf)
   (retract ?f1)
   (retract ?var)
   (assert (total-print (+ ?printx 1)))
)
   
(defrule find-not-recommendable
   (rekomendasi (nama ?nama1)(kategori "not recommendable")(distance ?dist1))
   (not (rekomendasi (distance ?dist2&:(< ?dist2 ?dist1))))
   ?f1 <- (rekomendasi (nama ?nama1) (count ?count1)(kategori ?kategori1)(distance ?dist1)(printed ?printed1))
   ?var <- (total-print ?printx&:(< ?printx 3))
   =>
   (printout t "Restoran " ?nama1 " "?dist1 " "?kategori1 " crlf)
   (retract ?f1)
   (retract ?var)
   (assert (total-print (+ ?printx 1)))
)
