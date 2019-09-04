PROC SQL;
   CREATE TABLE Tabela_Leituras_DTC AS 
   SELECT t1.ref_ext_DTC,
	  t1.data_leit,
	  t1.config,
	  t1.marca_DTC, 
	  t1.firmware,
          t1.qtd_EBs_regi,
	  t1.num_leit_DTC_dia1
   FROM WORK.Leituras_DTC t1
   WHERE t1.estado_DTC LIKE 'Telecontagem' 
	 AND t1.modelo_DTC LIKE 'DTC'
	 AND t1.data_leit < today()-6 
	 AND t1.data_leit > today()-14
	 AND t1.qtd_EBs_regi>=10 
	 AND t1.config is NOT NULL AND t1.firmware is NOT NULL
   GROUP BY t1.ref_ext_DTC
   HAVING count(t1.ref_ext_DTC)=7
   ORDER BY t1.ref_ext_DTC, t1.data_leit
;QUIT;