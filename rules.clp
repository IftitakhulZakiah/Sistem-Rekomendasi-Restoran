(defrule find-kategori
    =>
    (switch ?count
        (case 4 then (assert (rekomendasi (kategori "very recommendable")))
        (case 3 then (assert (rekomendasi (kategori "Recommendable")))
        (case 2 then (assert (rekomendasi (kategori "Recommendable")))
        (case 1 then (assert (rekomendasi (kategori "Not Recommendable")))
    )
