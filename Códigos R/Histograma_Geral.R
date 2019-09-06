#Cria frame para a opção "Histograma da tabela atual"
tmpHis<-gframe("Histograma da tabela atual", container=group, horizontal = FALSE)

#Se o ficheiro for das leituras, cria um combobox com duas opções: receção de leituras ao primeiro dia/receção de leituras
if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
{
  cb_dias <- gcombobox(c("Leituras Recebidas","Leituras 1º dia"))
  add(tmpHis,cb_dias)
}

#Função que cria um histograma da tabela atual (Se a tabela for de leituras, então cria o histograma consoante a escolha no combobox anterior)
plot_geral<-function(h,...)
{
  #Atualiza-se o valor da janela e coloca-se um gráfico na janela (que posteriormente será um histograma)
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,ggraphics())
  add(BigGroup,valor_janela$value)
  if (analise_nome=="Análise DTCs")
  {
    #Histograma da percentagem de EBs recebidas nos DTCs
    if (svalue(cb_dias)=="Leituras Recebidas")
    {
      depend<-tabela_a_trabalhar$value[["leitura_obtida"]]/tabela_a_trabalhar$value[["qtd_EBs_regi"]]*100
      hist(depend,breaks=20, main="Percentagem De Leituras DTCs", xlab = "Percentagem", ylab="Número de DTCs")
    }
    #Histograma da percentagem de EBs recebidas nos DTCs ao fim de um dia
    else
    {
      depend<-tabela_a_trabalhar$value[["fim_1dia"]]/tabela_a_trabalhar$value[["qtd_EBs_regi"]]*100
      hist(depend,breaks=20, main="Percentagem De Leituras DTCs 1º dia", xlab = "Percentagem", ylab="Número de DTCs")
    }
  }
  else if (analise_nome=="Análise EBs")
  {
    #Histograma que representa o sucesso (valor=1) ou insucesso (valor=0) das EBs estudadas
    if (svalue(cb_dias)=="Leituras Recebidas")
    {
      depend<-tabela_a_trabalhar$value[["leitura_obtida"]]
      hist(depend,breaks=20, main="Leituras EBs", xlab = "Leitura Realizada", ylab="Número de EBs")
    }
    #Histograma que representa o sucesso (valor=1) ou insucesso (valor=0) ao fim de um dia das EBs estudadas
    else
    {
      depend<-tabela_a_trabalhar$value[["fim_1dia"]]
      hist(depend,breaks=20, main="Leituras EBs 1º Dia", xlab = "Leitura Realizada", ylab="Número de EBs")
    }
  }
  #Histograma que representa o sucesso (valor=1) ou insucesso (valor=0) das OSs estudadas
  else
  {
    depend<-tabela_a_trabalhar$value[["sucesso"]]
    hist(depend,breaks=20, main="OS Realizadas", xlab = "OS realizada", ylab="Número de OS")
  }
}

#Criação do botão "Histograma"
botao_hist <- gbutton("Histograma", "hist_tot", handler=plot_geral)
add(tmpHis,botao_hist)
