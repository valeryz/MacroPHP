
(in-package :php)

(phpize '(progn
  (setq $db (mysql-pconnect ""))
  (mysql-select-db "mydb")
  (setq $res (mysql-query "select * from items"))
  (while (setq $row (mysql-fetch-array $res *mysql-num*))
    (printf "%s" (aref $row 0)))))
