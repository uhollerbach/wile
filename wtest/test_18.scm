;;; test SQLite interface

(define (dn v)
  (display v)
  (newline))

(dn (sqlite-version))

(define db (sqlite-open "cars.db" 'read-only))

(dn (sqlite-meta-tables db))

(dn (sqlite-meta-schema db "cars"))

(sqlite-dump-table db "cars" stdout)
(sqlite-dump-table db "; bobby tables" stdout)

(dn (sqlite-run db "SELECT * FROM cars WHERE price = 4"))

(dn (sqlite-run db "SELECT SUM(price) FROM cars WHERE id <= 4"))

(dn (sqlite-run db "SELECT name FROM cars WHERE (price >= 10000 AND price <= 40000)"))

(define stmt (sqlite-statement-prepare db
 "SELECT name FROM cars WHERE (price >= ?1 AND price <= ?2)"))

(dn (sqlite-statement-info stmt))

(sqlite-statement-bind stmt 15000 30000)
(dn (sqlite-statement-run stmt))

(sqlite-statement-bind stmt 0 100)
(dn (sqlite-statement-run stmt))

(sqlite-statement-bind stmt 50000 1000000)
(dn (sqlite-statement-run stmt))

(sqlite-statement-cleanup stmt)

(sqlite-close db)
