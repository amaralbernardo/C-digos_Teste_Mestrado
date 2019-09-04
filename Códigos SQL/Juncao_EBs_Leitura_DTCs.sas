PROC SQL;
   CREATE TABLE Tabela_EBS_mais_DTCS AS
   SELECT DISTINCT t1.ref_ext_DTC,
                   t1.ref_ext_EB,
                   t1.marca_EB,
                   t1.modelo_EB,
                   t1.estado_EB,
                   t1.estado_LAN_EB,
                   t2.data_leit,
                   t2.config,
                   t2.marca_DTC, 
                   t2.firmware,
                   t2.qtd_EBs_regi,
                   t2.num_leit_DTC_dia1
   FROM WORK.Todas_EBS t1 INNER JOIN Tabela_Leituras_DTC t2
   ON (t1.ref_ext_DTC=t2.ref_ext_DTC) 
   WHERE t1.data_regi_DTC_F>t2.data_leit
         AND t1.data_regi_DTC_I<=t2.data_leit
   GROUP BY  t1.ref_ext_EB, t2.data_leit, t1.ref_ext_DTC
   HAVING t1.data_CPE_F = max(t1.data_CPE_F) 
          AND t1.data_PT_F = max(t1.data_PT_F) 
;QUIT;
