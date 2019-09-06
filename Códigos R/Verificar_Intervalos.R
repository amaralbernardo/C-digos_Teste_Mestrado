#Função que verifica se todas as divisões realizadas têm pelo menos um elemento
funcao_verificar_divisoes<-function(tabela,div)
{
  #O primeiro e o último intervalo têm sempre, pelo menos um elemento.
  i<-2
  tamanho_div<-length(div)
  while (i < (tamanho_div-1))
  {
    #Se a tabela não contiver nenhum DTC com o número de EBs do intervalo selecionado, o utilizador tem hipótese de, ou cancelar as divisões, ou juntar o intervalo sem dados com o intervalo seguinte
    if(empty(tabela[tabela[["qtd_EBs_regi"]]<=div[i+1] & tabela[["qtd_EBs_regi"]]>div[i],]))
    {
      string<-paste("Não existem DTCs com o número de EBs ", div[i], sep="")
      string<-paste(string, " a ", sep="")
      string<-paste(string, div[i+1], sep="")
      string<-paste(string, ". Ao clicar no 'ok', este intervalo irá-se juntar com o intervalo ", sep="")
      string<-paste(string, div[i+1], sep="")
      string<-paste(string, " a ", sep="")
      string<-paste(string, div[i+2], sep="")
      string<-paste(string, ". No caso de clicar em 'cancel', não se criará a variável que permite verificar os DTCs por EBs.", sep="")
      ok<-gconfirm(string)
      if (ok)
      {
        for (j in (i+1):(tamanho_div-1))
        {
          div[j]<-div[j+1]
        }
        div<-div[-tamanho_div]
        tamanho_div<-tamanho_div-1
      }
      else
      {
        return(c())
      }
    }
    else
    {
      i<-i+1
    }
  }
  return(div)
}
