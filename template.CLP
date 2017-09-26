(deftemplate restoran
  (slot nama (type STRING))
  (slot isSmoker (type STRING))
  (slot budgetMin (type INTEGER))
  (slot budgetMaks (type INTEGER))
  (slot dresscode (type STRING))
  (slot hasWifi (type STRING))
  (slot latitude (type FLOAT))
  (slot longtitude (type FLOAT))
)

(deftemplate rekomendasi
	(slot nama (type STRING))
	(slot count (type INTEGER) (default 0))
	(slot rank (type INTEGER) (default 0))
	(slot kategori (type STRING))
)

(deftemplate user-preference
  (slot user-name (type STRING) (default "fulan"))
  (slot isSmoker (type STRING) (default "no"))
  (slot budgetMin (type INTEGER) (default 400))
  (slot budgetMaks (type INTEGER) (default 1000))
  (slot dresscode (type STRING) (default "casual"))
  (slot hasWifi (type STRING) (default "yes"))
  (slot latitude (type FLOAT) (default 0.0))
  (slot longtitude (type FLOAT) (default 0.0))
)
