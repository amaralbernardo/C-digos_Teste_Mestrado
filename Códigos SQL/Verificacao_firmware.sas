PROC SQL;
   CREATE TABLE Tabela_Leituras_DTC AS 
   SELECT *
   FROM Tabela_Leituras_DTC t1
   GROUP BY t1.firmware
   HAVING count(t1.firmware)>=70
;QUIT;
