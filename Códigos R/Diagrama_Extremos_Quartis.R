#Função que cria uma caixa de bigodes para cada variável na coluna associada
realizar_big<-function(h,...)
{
  #Atualiza-se o valor da janela e coloca-se um gráfico na janela
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,ggraphics())
  add(BigGroup,valor_janela$value)
  #Obtém-se o valor da coluna escolhida
  coluna_escolhida<-svalue(teste_coluna$value)
  titulo<-coluna_escolhida
  titulo_x<-""
  #Verifica se é utilizada a tabela toda, ou apenas a opção selecionada
  if (svalue(cb_opcao_graficos)=="Apenas Opção Selecionada")
  {
    coluna_opcao_escolhida<-svalue(teste_opcao$value)
    tabela_aux<-droplevels(tabela_a_trabalhar$value[tabela_a_trabalhar$value[[coluna_escolhida]]==coluna_opcao_escolhida,])
    titulo_x<-coluna_opcao_escolhida
  }
  else
  {
    tabela_aux<-tabela_a_trabalhar$value
  }
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
  plot(depend ~ tabela_aux[[coluna_escolhida]], xlab=titulo_x, ylab=paste(paste("Percentagem de Leituras (",leituras_todas, sep=""), ")", sep=""), main=titulo)
}

#Criação do botão "Caixa de Bigodes"
botao_big <- gbutton("Caixa de Bigodes", "cb", handler=realizar_big)
add(tmpTestGraOpc,botao_big)
