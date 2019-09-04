PROC SQL;
   CREATE TABLE Tabela_OS_todas AS 
   SELECT t1.id_OS,
          t1.data_OS,
          t1.tempo_OS,
          t1.nome_OS,
          t1.marca_EB,
          t1.modelo_EB,
          t1.sucesso,
          t2.ref_ext_DTC
   FROM WORK.OS t1 INNER JOIN WORK.Todas_EBS t2
   ON (t1.ref_ext_EB = t2.ref_ext_EB)
   WHERE t1.data_OS >= t2.data_regi_DTC_I 
         AND t1.data_OS < t2.data_regi_DTC_F
         AND t1.data_OS >= t2.data_PT_I 
         AND t1.data_OS < t2.data_PT_F
         AND t1.data_OS >= t2.data_CPE_I
         AND t1.data_OS < t2.data_CPE_F
         AND t1.data_OS >= [1a data] AND t1.data_OS<=[2a data]
   GROUP BY t1.id_OS
   HAVING count(t1.id_OS)=1
   ORDER BY t1.id_OS
;QUIT;
