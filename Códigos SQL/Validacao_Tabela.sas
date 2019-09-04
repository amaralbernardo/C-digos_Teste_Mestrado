PROC SQL;
   CREATE TABLE Verificar_DTC AS
   SELECT *, COUNT(t1.ref_ext_EB) as num_EBS
   FROM Tabela_EBS_DTCS_leituras t1
   GROUP BY t1.ref_ext_DTC, t1.data_leit
   HAVING num_EBS <> t1.qtd_EBs_regi
;QUIT;
