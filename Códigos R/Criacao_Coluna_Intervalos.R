#Função que permite criar coluna que indica em que grupo, que foi posteriormente definido na funcao_divisão, é que se encontra o número de EBs do DTC
funcao_criar_coluna_num_EBs<-function(tabela, div)
{
  tabela[["qtd_EBs_regi_divisoes"]]<-""
  for (i in 1:(length(div)-1))
  {
    if (i==1)
    {
      aux_string<-paste(toString(div[i]),"-",sep="")
    }
    else
    {
      aux_string<-paste(toString(div[i]+1),"-",sep="")
    }
    aux_string<-paste(aux_string,toString(div[i+1]),sep="")
    if (i==1)
    {
      tabela[tabela[["qtd_EBs_regi"]]<=div[i+1] & tabela[["qtd_EBs_regi"]]>=div[i],][["qtd_EBs_regi_divisoes"]]<-aux_string
    }
    else
    {
      tabela[tabela[["qtd_EBs_regi"]]<=div[i+1] & tabela[["qtd_EBs_regi"]]>div[i],][["qtd_EBs_regi_divisoes"]]<-aux_string
    }
  }
  tabela[["qtd_EBs_regi_divisoes"]]<-as.factor(tabela[["qtd_EBs_regi_divisoes"]])
  return(tabela)
}
