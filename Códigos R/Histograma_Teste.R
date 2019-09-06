#Cria frame dentro do frame "Teste" para os gráficos que necessitam da opção para se desenhar
tmpTestGraOpc <- gframe("Gráficos (com escolha)", container=tmpTest, horizontal = FALSE)

#Função que permite criar o histograma do sucesso/insucesso (OS/EBs) ou percentagens (DTC) para cada variável na coluna associada
realizar_plot<-function(h,...)
{
  #Atualiza-se o valor da janela e coloca-se um gráfico na janela
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,ggraphics())
  add(BigGroup,valor_janela$value)
  #Obtém-se o valor da coluna escolhida
  coluna_escolhida<-svalue(teste_coluna$value)
  #Verifica se é utilizada a tabela toda, ou apenas a opção selecionada
  if (svalue(cb_opcao_graficos)=="Apenas Opção Selecionada")
  {
    coluna_opcao_escolhida<-svalue(teste_opcao$value)
    tabela_aux<-droplevels(tabela_a_trabalhar$value[tabela_a_trabalhar$value[[coluna_escolhida]]==coluna_opcao_escolhida,])
  }
  else
  {
    tabela_aux<-tabela_a_trabalhar$value
  }
  #Se for análise das leituras, o programa verifica qual a opção escolhida no combobox existente no frame "teste" antes de criar os histogramas 
  if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
  {
    leituras_todas<-svalue(cb_dias_teste)
    if (leituras_todas=="Leituras 1º dia")
    {
      nome_depend<-"fim_1dia"
    }
    else
    {
      nome_depend<-"leitura_obtida"
    }
    #coluna correspondente ao número de leituras que foram enviadas da tabela utilizada
    depend<-tabela_aux[[nome_depend]]
    #Se for DTCs, calcula-se a percentagem e cria-se o histograma
    if (analise_nome=="Análise DTCs")
    {
      depend<-depend/tabela_aux[["qtd_EBs_regi"]]*100
      hist(depend ~ tabela_aux[[coluna_escolhida]],breaks=20,xlab = paste(paste("Percentagem de Leituras (",leituras_todas, sep=""), ")", sep=""),ylab="Número de DTCs")
    }
    #Se for EBs, cria-se o histograma
    else
    {
      hist(depend ~ tabela_aux[[coluna_escolhida]],breaks=20,xlab = paste(paste("Leitura realizada (",leituras_todas, sep=""), ")", sep=""),ylab="Número de EBs")
    }
  }
  #Se for OS, obtém-se o sucesso/insucesso das OSs realizadas e cria-se o histograma
  else
  {
    depend<-tabela_aux[["sucesso"]]
    hist(depend ~ tabela_aux[[coluna_escolhida]],breaks=20,xlab = "OS realizada",ylab="Número de OS")
  }
}

#Criação do botão "Histograma"
botao_plot <- gbutton("Histograma", "hist", handler=realizar_plot)
add(tmpTestGraOpc,botao_plot)
