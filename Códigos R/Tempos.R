#Função que cria uma text box com a média dos tempos que as EBs demoraram a enviar a sua informação de leitura para os Sistemas Centrais, desde o seu pedido (daqueles que conseguiram enviar)
#Além disso, é mostrada o número de tempos negativos que existem na tabela
tempo_medio<-function(h,...)
{
  #Atualiza-se o valor da janela e coloca-se um grupo na janela
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,ggroup(horizontal=FALSE, container=BigGroup))
  add(BigGroup,valor_janela$value)
  #Valor da coluna a testar
  valor_testar<-svalue(teste_coluna$value)
  #Retira os valores que existem na coluna a testar
  variaveis<-unique((tabela_a_trabalhar$value[[valor_testar]]))
  #Para cada uma das variáveis da coluna a testar
  for (i in 1:length(variaveis))
  {
    #É criada a tabela com valores tais que contém a tal variável da coluna a testar e cujo o valor da coluna "tempo_leit_EB" não seja nulo
    tabela_aux<-tabela_a_trabalhar$value[tabela_a_trabalhar$value[[valor_testar]]==variaveis[i] & !is.na(tabela_a_trabalhar$value[["tempo_leit_EB"]]),]
    #Obtém a média dos tempos
    media<-mean(tabela_aux[["tempo_leit_EB"]])
    aux_label<-paste(paste(paste(paste("Média tempos (",variaveis[i],sep=""),"): ",sep=""),as.character(round(as.double(media),4)),sep=""), " horas", sep="")
    #Caso a tabela não tenha sido obtida com o simulador, verifica-se quantos valores negativos foram encontrados na tabela a analisar
    if (!simulador)
    {
      valores_negativos<-length(tabela_aux[tabela_aux$tempo_leit_EB<0,][["tempo_leit_EB"]])
      aux_label<-paste(paste(paste(paste(paste(paste(aux_label,"          ",sep=""),"Tempos negativos (",variaveis[i],sep=""),"): ",sep=""),valores_negativos,sep="")," em ",sep=""),length(tabela_aux[,1]),sep="")
    }
    glabel(aux_label, container=valor_janela$value)
  }
}

#Botão que permite mostrar os tempos médios 
botao_tempo<-gbutton("Tempo médio", "PL", handler=tempo_medio)
add(tmpTestTempPercT,botao_tempo)
