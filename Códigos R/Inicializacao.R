nome<-gfile(text = "Selecione o ficheiro a utilizar",
            filter = list("Ficheiros csv" = list(patterns = c("*.csv"))), #Ficheiro a utilizar: *.csv
            quote = FALSE, #Não acrescenta aspas ao nome do ficheiro utilizado
            type = "open", #Abrir ficheiro
            initialfilename = "TABELA_LEITURAS_DTC.csv")
if (!is.na(nchar(nome))) #Se utilizador selecionou um ficheiro
{
  tabela<-read.csv(file=nome,head=TRUE,sep=";") #Lê tabela selecionada
  #attach -> Os objetos da tabela podem ser acedidos pelos seus nomes
  #detach -> Os objetos da tabela deixam de ser acedidos pelos nomes
  attach(tabela)
  # Se o "ID" existe, significa que a tabela a analisar foi criada pelo simulador
  if (exists("ID"))
  {
    simulador<-TRUE
  }
  else
  {
    simulador<-FALSE
  }
  # Se a tabela contiver a variável "tempo_medio", a tabela é de análise das leituras nos DTCs
  if (exists("tempo_medio"))
  {
    detach(tabela)
    opcao_esc<-"Análise DTCs"
  }
  # Se a tabela contiver a variável "tempo_leit_EB", a tabela é de análise das leituras nas EBs
  else if (exists("tempo_leit_EB"))
  {
    detach(tabela)
    opcao_esc<-"Análise EBs"
  }
  # Se a tabela contiver a variável "tempo_OS", a tabela é de análise das ordens de serviço
  else if (exists("tempo_OS"))
  {
    detach(tabela)
    opcao_esc<-"Análise OS"
  }
  # Se nenhuma das opções existir, a tabela não é válida
  else
  {
    detach(tabela)
    existe<-FALSE
    gmessage("Tabela inválida!", title="Erro",
             icon="error")
    return(FALSE)
  }
  #criação do interface
  interface(tabela, opcao_esc, simulador)
}
