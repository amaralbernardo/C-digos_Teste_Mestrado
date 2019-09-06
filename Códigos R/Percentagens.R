#Função que cria uma text box com as percentagens de leitura para a opção do combobox escolhida para cada um dos elementos da coluna a testar
perc_leit<-function(h,...)
{
  #Atualiza-se o valor da janela e coloca-se um grupo na janela
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,ggroup(horizontal=FALSE, container=BigGroup))
  add(BigGroup,valor_janela$value)
  #Valor da coluna a testar
  valor_testar<-svalue(teste_coluna$value)
  #Retira os valores que existem na coluna a testar
  variaveis<-unique((tabela_a_trabalhar$value[[valor_testar]]))
  leit_suc_fim<-c()
  total<-c()
  leit_suc_um<-c()
  #Para cada uma das variáveis da coluna a testar
  for (i in 1:length(variaveis))
  {
    #Cria a tabela só com a variável selecionada
    tabela_aux<-tabela_a_trabalhar$value[tabela_a_trabalhar$value[[valor_testar]]==variaveis[i],]
    #sum(tabela_aux[["leitura_obtida"]]) -> Faz a soma de todos os EBs que enviaram a informação da leitura para os Sistemas Centrais
    leit_suc_fim<-c(leit_suc_fim,sum(tabela_aux[["leitura_obtida"]]))
    #Se for a análise de leituras nos EBs
    if (analise_nome=="Análise EBs")
    {
      #nrow(tabela_aux) -> Número de linhas 
      total<-c(total,nrow(tabela_aux))
    }
    #Se for análise de leituras nos DTCs
    else
    {
      #sum(tabela_aux[["qtd_EBs_regi"]]) -> Soma de todas as EBs em analise
      total<-c(total,sum(tabela_aux[["qtd_EBs_regi"]]))
    }
    #Obtém a percentagem de EBs que enviaram a sua informação da leitura para os Sistemas Centrais
    percentagem<-as.character(round(as.double(leit_suc_fim[i])/total[i]*100,4))
    #Coloca no programa os resultados
    glabel(paste(paste(paste(paste(paste(paste(paste(paste("Número de leituras (",variaveis[i],sep=""),"): ",sep=""),leit_suc_fim[i],sep=""), " em ",sep=""), total[i], sep=""), " (", sep=""), percentagem, sep=""), "%)", sep=""), container=valor_janela$value)
  }
  #Separador
  glabel("",container=valor_janela$value)
  #Para cada uma das variáveis da coluna a testar
  for (i in 1:length(variaveis))
  {
    #É criada a tabela com valores tais que contém a tal variável da coluna a testar
    tabela_aux<-tabela_a_trabalhar$value[tabela_a_trabalhar$value[[valor_testar]]==variaveis[i],]

    #Faz a soma de todas as EBs que enviaram a sua leitura ao fim de um dia
    leit_suc_um<-c(leit_suc_um,sum(tabela_aux[["fim_1dia"]]))
    #Obtém a percentagem de EBs que enviaram a sua informação da leitura para os Sistemas Centrais no próprio dia
    percentagem<-as.character(round(as.double(leit_suc_um[i])/total[i]*100,4))
    glabel(paste(paste(paste(paste(paste(paste(paste(paste("Número de leituras primeiro dia (",variaveis[i],sep=""),"): ",sep=""),leit_suc_um[i], sep=""), " em ", sep=""),total[i],sep=""), " (",sep=""), percentagem,sep=""), "%)",sep=""),container=valor_janela$value)
  }
  #Separador
  glabel("",container=valor_janela$value)
  for (i in 1:length(variaveis))
  {
    #Obtém a percentagem, das EBs que enviaram a informação da sua leitura para os Sistemas Centrais, daquelas que consigaram mandar no próprio dia
    percentagem<-as.character(round(as.double(leit_suc_um[i])/leit_suc_fim[i]*100,4))
    glabel(paste(paste(paste(paste("Percentagem primeiro dia (",variaveis[i],sep=""),"): ",sep=""),percentagem, sep=""), "%", sep=""),container=valor_janela$value)
  }
}

#Botão que permite mostrar as percentagens
perc_func<-gbutton("Percentagem de leituras", "PL", handler=perc_leit)
add(tmpTestTempPercT,perc_func)
