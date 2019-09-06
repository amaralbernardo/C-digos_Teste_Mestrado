#Cria frame para gravar informação num ficheiro
tmpGra <- gframe("Gravar Em Ficheiro", container=group, horizontal = FALSE)

#Função que permite gravar a informação selecionada pelo utilizador para um ficheiro .csv
gravar_ficheiro<-function(h,...)
{
  visible(window) <- FALSE
  #Nome a dar ao ficheiro
  nome_ficheiro<-ginput("Nome a dar ao ficheiro",text="",title="Nome do Ficheiro", icon="question")
  #Verifica se operação foi cancelada
  if (!is.na(nome_ficheiro))
  {
    realizar_ope<-TRUE
    nome_ficheiro<-paste(nome_ficheiro, ".csv", sep="")
    #Se não foi introduzido nenhum valor
    if (nome_ficheiro==".csv")
    {
      gmessage("É necessário introduzir um nome para o ficheiro!", title="Erro", icon="error")
      realizar_ope<-FALSE
    }
    #Se o ficheiro existir, o programa pergunta se o utilizador quer substituir o ficheiro existente e continuar com a operação
    else if (file.exists(nome_ficheiro))
    {
      if (!gconfirm("Já existe um ficheiro com esse nome. Se continuar o ficheiro será substituído. Pretende continuar?"))
      {
        realizar_ope<-FALSE
      }
      else
      {
        file.remove(nome_ficheiro)
      }
    }
    if (realizar_ope)
    {
      #As características a gravar serão guardadas nos vetores seguintes
      carac_dtc<-c()
      carac_eb<-c()
      carac_dtc_eb<-c()
      #Tabela sem as alterações feitas
      tabela_aux<-tabela_sem_alteracoes$value
      i<-1
      #Vetor que contém dados que não dependem do DTC ou EB (Ex: dias da semana, nome da OS)
      dados_est<-c()
      #Para cada coluna da coluna
      while (i<=length(tabela_aux))
      {
        #Se a coluna não for a primeira (dia da semana), "qtd_EBs_regi_divisoes" ou "nome_OS" (no caso da tabela for de análise às OS)
        if (i!=1 & colnames(tabela_aux)[i]!="qtd_EBs_regi_divisoes" & colnames(tabela_aux)[i]!="nome_OS")
        {
          #Se a coluna não for do tipo fator
          if (sapply(tabela_aux, class)[i]!="factor")
          {
            #Retiram-se essas colunas para a análise
            tabela_aux<-tabela_aux[-i]
          }
          else
          {
            #Obtém-se o nome dessa coluna
            nome_carac<-colnames(tabela_aux)[i]
            #O programa pergunta ao utilizador se pretende considerar a característica na gravação do ficheiro
            if (gconfirm(paste(paste("Considerar a característica ", nome_carac, sep=""), "?", sep="")))
            {
              #Se sim, o utilizador indica se é uma característica do DTC, EB ou DTC/EB
              if (gconfirm("Característica DTC?", title = nome_carac, icon = "question"))
              {
                carac_dtc<-cbind(carac_dtc, i)
              }
              else if (gconfirm("Característica EB?", title = nome_carac, icon = "question"))
              {
                carac_eb<-cbind(carac_eb,i)
              }
              else
              {
                carac_dtc_eb<-cbind(carac_dtc_eb,i)
              }
              i<-i+1
            }
            #Caso contrário, retira-se essa coluna da tabela
            else
            {
              tabela_aux<-tabela_aux[-i]
            }
          }
        }
        else
        {
          dados_est<-cbind(dados_est,i)
          i<-i+1
        }
      }
      #Obter o nome da coluna com os valores do sucesso ou insucesso de envio de leitura/realização da OS dependendo da tabela a utilizar
      if (analise_nome=="Análise EBs")
      {
        depend<-"fim_1dia"
      }
      else
      {
        depend<-"sucesso"
      }
      #A tabela_unica será uma tabela com todas as combinações existentes na tabela considerada anteriormente
      tabela_unica<-uniquecombs(tabela_aux,ordered=FALSE)
      #Acrescenta-se ao nome das colunas as colunas "num_vezes","IC_inf","IC_sup","media" e essas colunas, inicialmente, terão o valor 0
      n_colunas<-c(colnames(tabela_unica),"num_vezes","IC_inf","IC_sup","media")
      tabela_a_gravar<-cbind(tabela_unica, 0, 0, 0, 0)
      #As posições das colunas serão adicionados ao vetor com os dados estatísticos 
      for (j in i:(i+3))
      {
        dados_est<-cbind(dados_est,j)
      }
      valor_col_adi<-i+4
      #Nome das colunas do ficheiro a gravar
      colnames(tabela_a_gravar)<-n_colunas
      #Cria-se uma janela que indica a percentagem realizada da operação
      mens_jan<-gwindow()
      mens<-ggroup(container=mens_jan, horizontal = FALSE)
      glabel("Gravar Dados Ficheiros:", container=mens)
      mens_label<-glabel("0% concluído")
      add(mens,mens_label)
      #Realização da operação gravar em ficheiro
      l <- unlist(apply(tabela_aux, 2, list), recursive=F)
      for (i in 1:length(tabela_unica[,1]))
      {
        #Procedimento feito para obter cada combinação da tabela anteriormente obtida
        logic <- mapply(function(x,y)x==y, l, tabela_unica[i,])
        linhas_cons<-which(.rowSums(logic, m=nrow(logic), n=ncol(logic)) == ncol(logic))
        #Da tabela original, obter apenas as linhas que coincidem com a combinação a testar
        tabela_teste<-tabela_sem_alteracoes$value[linhas_cons,]
        #Valores que obtiveram sucesso
        num_suc<-length(tabela_teste[tabela_teste[[depend]]==1,][[depend]])
        #valores que não obtiveram sucesso
        num_insuc<-length(tabela_teste[tabela_teste[[depend]]==0,][[depend]])
        #Na coluna "num_vezes", gravar o número de EBs com a combinação a estudar
        tabela_a_gravar[["num_vezes"]][i]<-length(tabela_teste[,1])
        #Realiza o teste binomial para os sucessos e insucessos da combinação
        teste<-binom.test(c(num_suc,num_insuc), p = 0.8)
        #Obtém-se, respetivamente, o intervalo de confiança inferior, superior e a média estimada e grava na tabela a gravar
        tabela_a_gravar[["IC_inf"]][i]<-teste$conf.int[1]
        tabela_a_gravar[["IC_sup"]][i]<-teste$conf.int[2]
        tabela_a_gravar[["media"]][i]<-teste$estimate
        #Atualiza-se a janela da percentagem realizada da operação
        delete(mens,mens_label)
        mens_label<-glabel(paste(round((i-1)/length(tabela_unica[,1])*100,2),"% concluído",sep=""))
        add(mens,mens_label)
      }
      #Depois de obter os dados todos, organiza-se a informação de forma a ficar
      #1º -> Características do DTC
      #2º -> Características das EBs
      #3º -> Características dos DTCs/EBs
      #4º -> Dados Estatísticos
      #Nota: Em cada um destes pontos há uma coluna em branco a separá-los
      tabela_a_gravar<-cbind(tabela_a_gravar,"","","")
      colnames(tabela_a_gravar)[valor_col_adi]<-""
      carac_dtc<-cbind(carac_dtc,valor_col_adi)
      valor_col_adi<-valor_col_adi+1
      colnames(tabela_a_gravar)[valor_col_adi]<-" "
      carac_eb<-cbind(carac_eb,valor_col_adi)
      valor_col_adi<-valor_col_adi+1
      colnames(tabela_a_gravar)[valor_col_adi]<-"  "
      carac_dtc_eb<-cbind(carac_dtc_eb,valor_col_adi)
      carac_ordem<-cbind(carac_dtc, carac_eb, carac_dtc_eb,dados_est)
      tabela_a_gravar<-tabela_a_gravar[,carac_ordem]
      #Gravar a tabela no ficheiro
      write.table(tabela_a_gravar, file = nome_ficheiro, quote = FALSE, row.names=FALSE,col.names=TRUE, sep=";")
      #Apagar a caixa das percentagens de realização
      dispose(mens_jan)
    }
  }
  visible(window) <- TRUE
}

#Botão que permite gravar os dados da tabela num ficheiro
botao_gravar <-gbutton("Gravar", "gravar", handler=gravar_ficheiro)
add(tmpGra,botao_gravar)
