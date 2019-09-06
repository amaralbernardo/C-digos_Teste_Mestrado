#Função que verifica se o teste a utilizar é paramêtrico ou não-paramêtrico.
funcao_parametrico<-function(tabela_uti,valor_testar,variaveis,valor_cb)
{
  nao_parametrico<-FALSE
  i<-1
  #Enquanto não se concluir que é não paramêtrico, ou enquanto houver variáveis a testar
  while (!nao_parametrico & i<=length(variaveis))
  {
    #Obtém uma tabela cuja coluna a testar só contém a variável representada no ciclo
    tabela_aux<-tabela_uti[tabela_uti[[valor_testar]]==variaveis[i],]
    #Obtém as percentagens obtidas para essa variável
    coluna_testar<-tabela_aux[[valor_cb]]/tabela_aux[["qtd_EBs_regi"]]*100
    #Obtém o número de observações contendo essa variável
    num_obs<-length(coluna_testar)
    #Se o número de observações for igual ou inferior a 5000, utilizar o teste de shapiro, caso contrário, usar o teste anderson-darling
    if (num_obs<=5000)
    {
      pvalor<-shapiro.test(coluna_testar)$p.value
    }
    else
    {
      pvalor<-ad.test(coluna_testar)$p.value
    }
    #Se o p-valor for <0.05, rejeita-se a hipótese nula, logo a distribuição não é normal aos níveis de significância usuais, e portanto deve-se utilizar o teste não-paramétrico
    if (pvalor<0.05)
    {
      nao_parametrico<-TRUE
    }
    else
    {
      #Verificar existência de outliers
      ter_quartil<-quantile(coluna_testar)[4]
      pri_quartil<-quantile(coluna_testar)[2]
      AIQ<-ter_quartil-pri_quartil
      num_outliers<-length(coluna_testar[coluna_testar>ter_quartil+1.5*AIQ | coluna_testar<pri_quartil-1.5*AIQ])
      
      if (num_outliers/num_obs>=0.025)
      {
        nao_parametrico<-TRUE
      }
      else
      {
        i<-i+1
      }
    }
  }
  return (nao_parametrico)
}
