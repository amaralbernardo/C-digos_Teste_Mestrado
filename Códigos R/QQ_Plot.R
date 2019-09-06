#Cria frame dentro do frame "Teste" para os gráficos que só funcionam com a opção escolhida
tmpTestGraObriOpc <- gframe("Gráficos (opção escolhida)", container=tmpTest, horizontal = FALSE)

#Função que cria o QQPlot da variável escolhida do combobox acima
criar_qqplot<-function(h,...)
{
  #Atualiza-se o valor da janela e coloca-se um gráfico na janela
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,ggraphics())
  add(BigGroup,valor_janela$value)

  #Obtém-se o valor da coluna escolhida
  coluna_escolhida<-svalue(teste_coluna$value)
  coluna_opcao_escolhida<-svalue(teste_opcao$value)
  titulo<-paste(paste(paste(coluna_escolhida, " (", sep=""), coluna_opcao_escolhida, sep=""), ")", sep="")
  tabela_aux<-droplevels(tabela_a_trabalhar$value[tabela_a_trabalhar$value[[coluna_escolhida]]==coluna_opcao_escolhida,])

  #Verifica qual a opção escolhida no combobox existente no frame "teste" antes de criar as caixas de bigodes 
  leituras_todas<-svalue(cb_dias_teste)
  if (leituras_todas=="Leituras 1º dia")
  {
    nome_depend<-"fim_1dia"
  }
  else
  {
    nome_depend<-"leitura_obtida"
  }
  #Calcula-se a percentagem e cria-se a caixa de bigodes
  depend<-tabela_aux[[nome_depend]]/tabela_aux[["qtd_EBs_regi"]]*100
  qqnorm(depend, main=titulo)
  qqline(depend, col = "steelblue")
}

#Criação do botão "QQPlot"
botao_qq <- gbutton("QQPlot", "QQ", handler=criar_qqplot)
add(tmpTestGraObriOpc,botao_qq)
