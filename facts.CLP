(deffacts restoran
  (restoran (nama "Restoran A") (isSmoker "yes") (budgetMin 1000) (budgetMaks 2000) (dresscode "casual") (hasWifi "yes") (latitude -6.8922186) (longitude 107.5886173))
  (restoran (nama "Restoran B") (isSmoker "no") (budgetMin 1200) (budgetMaks 2500) (dresscode "informal") (hasWifi "yes") (latitude -6.224085) (longitude 106.7859815))
  (restoran (nama "Restoran C") (isSmoker "yes") (budgetMin 2000) (budgetMaks 4000) (dresscode "formal") (hasWifi "no") (latitude -6.2145285) (longitude 106.8642591))
  (restoran (nama "Restoran D") (isSmoker "no") (budgetMin 500) (budgetMaks 1400) (dresscode "formal") (hasWifi "no") (latitude -6.9005363) (longitude 107.6222191))
  (restoran (nama "Restoran E") (isSmoker "yes") (budgetMin 1000) (budgetMaks 2000) (dresscode "informal" "casual") (hasWifi "yes") (latitude -6.2055617) (longitude 106.8001597))
  (restoran (nama "Restoran F") (isSmoker "no") (budgetMin 2500) (budgetMaks 5000) (dresscode "informal") (hasWifi "yes") (latitude -6.9045679) (longitude 107.6399745))
  (restoran (nama "Restoran G") (isSmoker "yes") (budgetMin 1300) (budgetMaks 3000) (dresscode "casual") (hasWifi "yes") (latitude -6.1881082) (longitude 106.7844409))
  (restoran (nama "Restoran H") (isSmoker "no") (budgetMin 400) (budgetMaks 1000) (dresscode "informal") (hasWifi "no") (latitude -6.9525133) (longitude 107.6052906))
  (restoran (nama "Restoran I") (isSmoker "no") (budgetMin 750) (budgetMaks 2200) (dresscode "informal" "casual") (hasWifi "yes") (latitude -6.9586985) (longitude 107.7092281))
  (restoran (nama "Restoran J") (isSmoker "no") (budgetMin 1500) (budgetMaks 2000) (dresscode "casual") (hasWifi "yes") (latitude -6.2769732) (longitude 106.775133))
) 

(deffacts rekomendasi
  (rekomendasi (nama "Restoran A") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran B") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran C") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran D") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran E") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran F") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran G") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran H") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran I") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
  (rekomendasi (nama "Restoran J") (count 0) (kategori "") (distance 0.0) (printed "no") (checked 0))
)

(deffacts var-global
  (total-print 0)
 )
