PROC SQL;
   CREATE TABLE Tabela_OS_EB_DTC AS 
   SELECT t1.data_OS,
          t1.nome_OS,
          t1.marca_EB,
          t1.modelo_EB,
          t2.config,
          t2.marca_DTC,
          t2.firmware,
          t2.qtd_EBs_regi,
          t1.sucesso,
          t1.tempo_OS
   FROM Tabela_OS_todas t1 INNER JOIN WORK.Todos_DTC t2
   ON (t1.ref_ext_DTC = t2.ref_ext_DTC)
   WHERE t2.config is not null 
         AND t2.modelo_DTC LIKE 'DTC'
         AND t2.firmware is not null 
         AND t2.qtd_EBs_regi>=10
;QUIT;
