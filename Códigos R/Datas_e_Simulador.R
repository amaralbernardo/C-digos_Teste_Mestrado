if (analise_nome!="Análise OS") #Leituras
{
  #Passagem do mês de inglês para português
  tabela_original[["data_leit"]]<-gsub("FEB","FEV",tabela_original[["data_leit"]])
  tabela_original[["data_leit"]]<-gsub("APR","ABR",tabela_original[["data_leit"]])
  tabela_original[["data_leit"]]<-gsub("MAY","MAI",tabela_original[["data_leit"]])
  tabela_original[["data_leit"]]<-gsub("AUG","AGO",tabela_original[["data_leit"]])
  tabela_original[["data_leit"]]<-gsub("SEP","SET",tabela_original[["data_leit"]])
  tabela_original[["data_leit"]]<-gsub("OCT","OUT",tabela_original[["data_leit"]])
  tabela_original[["data_leit"]]<-gsub("DEC","DEZ",tabela_original[["data_leit"]])
  #Ex: antes do "as.date": 09FEV2019; depois do "as.date": 2019-02-21
  tabela_original[["data_leit"]]<-as.Date(tabela_original[["data_leit"]],"%d%b%Y")
  #Nome da coluna que contém a data
  nome_coluna<-"data_leit"
}
else #OS
{
  #Ex: antes do "as.date": 09/02/2019
  car<-substr(as.character(tabela_original[["data_OS"]][1]), 3, 3)
  if (car=="-")
  {
    tabela_original[["data_OS"]]<-as.Date(tabela_original[["data_OS"]], "%d-%m-%Y")
  }
  else
  {
    tabela_original[["data_OS"]]<-as.Date(tabela_original[["data_OS"]], "%d/%m/%Y")
  }
  #Nome da coluna que contém a data
  nome_coluna<-"data_OS"
}

#Obtém a data mais antiga
dia_inicio<-min(as.character(tabela_original[[nome_coluna]]))
#Obtém a data mais recente
dia_final<-max(as.character(tabela_original[[nome_coluna]]))
#Altera a coluna que contém a data para "dia_semana"
colnames(tabela_original)[colnames(tabela_original)==nome_coluna] <- "dia_semana"
#Altera as datas para o dia da semana correspondente à data e a variável "dia_semana" passa a ser do tipo fator
tabela_original[["dia_semana"]]<-as.factor(weekdays(tabela_original[["dia_semana"]]))
#Se a tabela introduzida foi obrida a partir do simulador
if (simulador)
{
  #elimina as variáveis ID existentes na tabela
  tabela_original<-tabela_original[c(-(length(tabela_original)-1):-length(tabela_original))]
  #Titulo do programa contém as datas em estudo e uma frase a indicar que se trata de resultados simulados
  titulo<-paste(paste(paste(dia_inicio, " até ", sep=""), dia_final, sep=""), " (valores simulados)", sep="")
}
#No caso de a tabela introduzida ser da que foi obtida a partir da base de dados da empresa
else
{
  #Titulo do programa contém apenas as datas da tabela
  titulo<-paste(paste(dia_inicio, " até ", sep=""), dia_final, sep="")
}
