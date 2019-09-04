PROC SQL;
   CREATE TABLE Tabela_EBS_DTCS_leituras as
   SELECT t1.ref_ext_DTC,
          t1.ref_ext_EB,
          t1.data_leit,
          t1.marca_EB,
          t1.modelo_EB,
          t1.estado_EB,
          t1.estado_LAN_EB,
          t1.config,
          t1.marca_DTC,
          t1.firmware,
          t1.qtd_EBs_regi,
          case
          WHEN (t2.tempo_leit_EB is not null
                AND t2.tempo_leit_EB<24) then 1
          else 0
          end as fim_1dia,
          case
          WHEN t2.tempo_leit_EB is not null then 1
          else 0
          end as leitura_obtida,
          t2.tempo_leit_EB
   FROM Tabela_EBS_mais_DTCS t1 LEFT JOIN WORK.Leituras_EBS t2
   ON (t1.ref_ext_EB=t2.ref_ext_EB 
       AND t1.data_leit=t2.data_leit)
;QUIT;
