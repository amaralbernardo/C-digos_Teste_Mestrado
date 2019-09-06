#Cria frame para a opção "Sumario"
tmpSum <- gframe("Sumario", container=group)

#Função que cria um sumário da tabela utilizada após clicar no botão "Sumario da tabela atual"
mostrarSumario<-function(h,...)
{
  #Nomes de cada uma das colunas da tabela
  nomes_colunas<-gsub(" ", "", colnames(summary(tabela_a_trabalhar$value)), fixed = TRUE)
  #Número de colunas na tabela
  num_colunas<-length(tabela_a_trabalhar$value)
  data_usar<-c()
  #Verifica qual é o máximo número de linhas que são utilizadas de todas as colunas
  max_valor<-0
  for (i in 1:num_colunas)
  {
    if (length(summary(tabela_a_trabalhar$value[,i]))>max_valor)
    {
      max_valor<-length(summary(tabela_a_trabalhar$value[,i]))
    }
  }
  #Adiciona-se o sumário de cada coluna à variável "data_usar" (da última coluna para a primeira)
  for (i in num_colunas:1)
  {
    #Nota: Aquando criado o sumário da coluna a adicionar, o nome de cada variável existente nessa coluna é considerado como sendo o nome da linha
    aux<-summary(tabela_a_trabalhar$value[,i])
    valor<-length(aux) #Guarda o valor do número de linhas que se obteve depois do sumário da coluna
    length(aux)<-max_valor
    data_usar<-cbind(aux,data_usar)
    #Para cada linha da coluna adicionada
    for (j in 1:valor)
    {
      #Para contornar o problema das variáveis dito anteriormente, junta-se o valor da variável à coluna, juntamente com o seu valor, e apagam-se os nomes das linhas
      if (is.na(as.double(data_usar[j,1]))) #Caso factor/string
      {
        data_usar[j,1]<-paste(paste(row.names(data_usar)[j],": ",sep=""),data_usar[j,1],sep="")
      }
      else #Caso double
      {
        data_usar[j,1]<-paste(paste(row.names(data_usar)[j],": ",sep=""),as.character(round(as.double(data_usar[j,1]),4)),sep="")
      }
      row.names(data_usar)[j]<-""
    }
  }
  #Altera-se o nome das colunas
  colnames(data_usar) <- nomes_colunas
  for (i in 1:length(data_usar[1,]))
  {
    for (j in 1:length(data_usar[,i]))
    {
      #Verifica em todas as posições da tabela criada, se há valores NA. Se existirem, considera-se o valor "" para esses valores
      if (is.na(data_usar[j,i]))
      {
        data_usar[j,i]<-""
      }
    }
  }
  #Atualiza-se o valor da janela e coloca-se a tabela criada a ser mostrada no programa
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,gtable(data_usar, filter.FUN="manual"))
  size(valor_janela$value)<-c(1000,50)
  add(BigGroup,valor_janela$value)
}

#Criação do botão "Sumario da Tabela Atual"
botao_sumario <- gbutton("Sumario da Tabela Atual","sumario",handler = mostrarSumario)
add(tmpSum,botao_sumario)
