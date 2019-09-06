#Função que devolve um array que contém as divisões que se pretende fazer na qtd_EBs_regi
funcao_divisao<-function(min_valor, max_valor)
{
  num_grupos<-1
  while (num_grupos<=1 | num_grupos>=6)
  {
    div<-c()
    valor<-ginput("Número de divisões (2 a 5)",text="",title="Divisões", icon="question")
    if (is.na(valor))
    {
      return(c(-1))
    }
    num_grupos<-strtoi(valor)
    #Se cancelar a opção não se fez nenhuma alteração
    if (is.na(num_grupos))
    {
      gmessage("Valor inválido!", title="Erro", icon="error")
      num_grupos<-0
    }
    #Valor introduzido tem de ser entre 2 e 5
    else if (num_grupos>=6 | num_grupos<=1)
    {
      gmessage("Valor inválido!", title="Erro", icon="error")
    }
    else #Constução dos intervalos
    {
      div<-c(min_valor)
      i<-1
      aux_div<-div[length(div)]
      while (i<num_grupos)
      {
        string<-paste("Introduze o extremo superior do ", i, sep="")
        string<-paste(string, "º intervalo (valor entre ", sep="")
        string<-paste(string, aux_div, sep="")
        string<-paste(string, " e ", sep="")
        string<-paste(string, max_valor-(num_grupos-i), sep="")
        string<-paste(string, ")", sep="")
        valor<-ginput(string, text="", title=paste(i, "º intervalo",sep=""), icon="question")
        if (is.na(valor))
        {
          return(c(-1))
        }
        divisao<-strtoi(valor)
        if (is.na(divisao))
        {
          gmessage("Valor inválido!", title="Erro",
                   icon="error")
        }
        else if (divisao>=aux_div & divisao<(max_valor-(num_grupos-i)))
        {
          div<-c(div,divisao)
          aux_div<-div[length(div)]+1
          i<-i+1
        }
        else if (divisao==(max_valor-(num_grupos-i)))
        {
          div<-c(div,divisao)
          i<-i+1
          for (j in i:(num_grupos-1))
          {
            divisao<-divisao+1
            div<-c(div,divisao)
          }
          i<-num_grupos
        }
        else
        {
          gmessage("Valor inválido!", title="Erro",
                   icon="error")
        }
      }
      div<-c(div,max_valor)
    }
  }
  return (div)
}
