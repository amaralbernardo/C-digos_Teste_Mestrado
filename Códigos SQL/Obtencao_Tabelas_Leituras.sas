PROC SQL;
   CREATE TABLE Tabela_Leituras_DTC AS 
   SELECT UNIQUE t1.ref_ext_DTC,
                 t1.data_leit,
                 t1.config,
                 t1.marca_DTC,
                 t1.firmware,
                 t1.qtd_EBs_regi,
                 sum(t1.fim_1dia) as fim_1dia,
                 sum(t1.leitura_obtida) as leitura_obtida,
                 avg(t1.tempo_leit_EB) as tempo_medio
   FROM Tabela_EBS_DTCS_leituras t1
   GROUP BY t1.ref_ext_DTC, t1.data_leit
;QUIT;

PROC SQL;
   ALTER TABLE Tabela_Leituras_DTC
   DROP ref_ext_DTC
;QUIT;

PROC SQL;
   ALTER TABLE Tabela_EBS_DTCS_leituras
   DROP ref_ext_DTC, ref_ext_EB
;QUIT;
