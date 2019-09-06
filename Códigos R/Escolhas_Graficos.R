#Cria frame dentro do frame "Teste" com as opções para os gráficos que necessitam de escolhas do utilizador
tmpTestOpcGra <- gframe("Gráficos Escolhas", container=tmpTest, horizontal = FALSE)

#Se o ficheiro for das leituras, cria um combobox com duas opções: receção de leituras ao primeiro dia/receção de leituras
if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
{
  cb_dias_teste<-gcombobox(c("Leituras Recebidas","Leituras 1º dia"))
  add(tmpTestOpcGra,cb_dias_teste)
}

#Combobox com opções para mostrar nos gráficos todas as opções, ou apenas a opção selecionada
cb_opcao_graficos <- gcombobox(c("Todas Opções","Apenas Opção Selecionada"))
add(tmpTestOpcGra,cb_opcao_graficos)
