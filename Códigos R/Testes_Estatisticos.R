#Função que cria o teste consoante o caso que se está a tratar
realizar_teste<-function(h,...)
{
  #Atualiza-se o valor da janela e coloca-se um gráfico na janela
  delete(BigGroup,valor_janela$value)
  atualizar_ponteiro(valor_janela,ggroup(horizontal=FALSE, container=BigGroup))
  add(BigGroup,valor_janela$value)
  #Obtém-se o valor da coluna escolhida
  valor_testar<-svalue(teste_coluna$value)
  tabela_aux<-droplevels(tabela_a_trabalhar$value)
  #Coluna escolhida
  col_escolhida<-tabela_aux[[valor_testar]]
  #Obtém-se as variáveis existentes na coluna escolhida
  variaveis<-unique(col_escolhida)
  #O programa vai retirar todas as opções cujo número de observações for abaixo de 3 (No caso dos DTCs, o teste shapiro-wilk só é utilizado se o número de observações for superior a 2)
  texto_aviso<-"Devido ao número de observações ser abaixo de 3, as seguintes opções da variável escolhida não foram consideradas:"
  if (analise_nome=="Análise DTCs")
  {
    i<-1
    while (i<=length(variaveis))
    {
      if (sum(tabela_aux[[valor_testar]]==variaveis[i])<3)
      {
        texto_aviso<-paste(texto_aviso,variaveis[i],sep="\n")
        tabela_aux<-droplevels(tabela_aux[tabela_aux[[valor_testar]]!=variaveis[i],])
        variaveis<-droplevels(variaveis[-i])
      }
      else
      {
        i<-i+1
      }
    }
  }
  i<-length(variaveis)
  #Se no final o número de opções com mais de 2 observações for inferior a 2, então o teste não será realizado
  if (i<=1)
  {
    gmessage("A variável escolhida não tem pelo menos duas opções com número de observações superior a 2")
  }
  else
  {
    #Caso tenha sido encontrado alguma opção com menos de 3 observações, é deixado um aviso
    if (texto_aviso!="Devido ao número de observações ser abaixo de 3, as seguintes opções da variável escolhida não foram consideradas:")
    {
      gmessage(texto_aviso)
    }
    #Os testes realizados variam consoante a tabela a analisar
    #Se for a análise de DTCs
    if (analise_nome=="Análise DTCs")
    {
      #Obtém-se se pretende realizar o teste para as leituras obtidas ou as leituras obtidas ao fim de 1 dia
      if (svalue(cb_dias_teste)=="Leituras Recebidas")
      {
        depen<-"leitura_obtida"
      }
      else
      {
        depen<-"fim_1dia"
      }
      #Obtém uma coluna com as percentagens de cada DTC, consoante as escolhas feitas
      percentagem<-tabela_aux[[depen]]/tabela_aux[["qtd_EBs_regi"]]*100
      #O programa verifica primeiro se é para utilizar o teste paramêtrico ou não paramêtrico
      nao_parametrico<-funcao_parametrico(tabela_aux,valor_testar,variaveis,depen)
      #Se o teste a utilizar for paramêtrico
      if (!nao_parametrico)
      {
        #Teste de Barlett para estudar a homogeneidade de variâncias entre as opções da variável
        homogeneidade<-bartlett.test(percentagem ~ col_escolhida)
        #Se houver mais do que duas opções para comparar
        if (i!=2)
        {
          #Se o p-valor for inferior a 0.05, rejeita-se a homogeneidade de variâncias, ao nível de significância usual, e é utilizado o teste de welch com o teste post-hoc Games-Howell
          if (homogeneidade$'Pr(>F)'[1]<0.05)
          {
            str_teste<-"Teste Welch"
            pvalor <- oneway.test(percentagem ~ col_escolhida)$p.value
            #Devido á não homogeneidade 
            poolsd <- FALSE
            #O teste post-hoc é utilizado para comparar os p-valores das opções dois a dois (Games-Howell no caso de se usar o teste de Welch)
            metodo_post <- "Games-Howell"
          }
          #Se aceitar a homogeneidade de variâncias, ao nível de significância usual, é utilizado o teste ANOVA com teste post-hoc Tukey
          else
          {
            str_teste<-"Teste ANOVA"
            pvalor <- anova(lm(percentagem ~ col_escolhida))$'Pr(>F)'[1]
            #Devido à homogeneidade
            poolsd <- TRUE
            #O teste post-hoc é utilizado para comparar os p-valores das opções dois a dois (Tukey no caso de se usar o teste ANOVA)
            metodo_post <- "Tukey"
          }
        }
        else
        {
          #Em ambos os casos utiliza-se o T-teste. No entanto, eles variam consoante a homogeneidade de variâncias
          if (homogeneidade$'Pr(>F)'[1]<0.05)
          {
            str_teste <- "T-Teste (sem homogeneidade de variâncias)"
            pvalor <- t.test(percentagem ~ col_escolhida, var.equal=FALSE)$p.value
          }
          else
          {
            str_teste <- "T-Teste (com homogeneidade de variâncias)"
            pvalor <- t.test(percentagem ~ col_escolhida, var.equal=TRUE)$p.value
          }
        }
      }
      #Se o teste for não-paramêtrico
      else
      {
        #No caso de o número de opções for superior a 2, utiliza-se o teste de Kruskal-wallis
        if (i!=2)
        {
          str_teste <- "Teste Kruskal-Wallis"
          pvalor<-kruskal.test(percentagem ~ col_escolhida)$p.value
        }
        #Caso contrário, utiliza-se o teste de Wilcoxon
        else
        {
          str_teste <- "Teste de Wilcoxon"
          pvalor <- wilcox.test(percentagem ~ col_escolhida)$p.value
        }
      }
      #Escreve-se no programa os resultados obtidos
      glabel(str_teste,container = valor_janela$value)
      glabel(paste(paste(paste("p-valor do ",str_teste,sep=""), ": ",sep=""), pvalor, sep="" ), container = valor_janela$value)
      glabel("",container = valor_janela$value)
      if (pvalor < 0.05)
      {
        if (i!=2)
        {
          glabel("Como o p-valor é abaixo do nível de significância 0.05, rejeita-se a hipótese de que as médias entre os grupos são todas iguais.",container=valor_janela$value)
          glabel("Isto significa que existe diferenças significativas entre, pelo menos, dois dos grupos.", container = valor_janela$value)
        }
        else
        {
          glabel("Como o p-valor é abaixo do nível de significância 0.05, rejeita-se a hipótese de que as médias entre os dois grupos são iguais.",container=valor_janela$value)
          glabel("Isto significa que existe diferenças significativas entre os dois grupos.", container = valor_janela$value)
        }
      }
      else
      {
        if (i!=2)
        {
          glabel("Como o p-valor é acima do nível de significância 0.05, aceita-se a hipótese de que as médias entre os grupos são todas iguais.",container=valor_janela$value)
          glabel("Isto significa que não existe diferenças significativas entre os grupos.",container=valor_janela$value)
        }
        else
        {
          glabel("Como o p-valor é acima do nível de significância 0.05, aceita-se a hipótese de que as médias entre os dois grupos são iguais.",container=valor_janela$value)
          glabel("Isto significa que não existe diferenças significativas entre os dois grupos.",container=valor_janela$value)
        }
      }
      glabel("",container=valor_janela$value)
      #No caso de o número de opções ser superior a 2, então verifica-se entre que grupos é que existem diferenças significativas
      if (i!=2)
      {
        glabel("P-valores entre grupos:",container=valor_janela$value)
        #No caso paramêtrico
        if (!nao_parametrico)
        {
          #p-valor entre os grupos
          col_escolhida<-gsub("-","_",col_escolhida)
          #Faz-se o teste posthoc
          teste<-posthocTGH(percentagem,col_escolhida,method=metodo_post)
          #Escreve-se numa matriz as opções e os p-valores associados entre eles
          niveis<-levels(teste$intermediate$x)
          matriz<-data.frame(matrix(NA, nrow = (length(niveis)-1), ncol = (length(niveis)-1)))
          rownames(matriz)<-niveis[-1]
          colnames(matriz)<-niveis[-length(niveis)]
          k<-1
          for (i in 1:(length(niveis)-1))
          {
            for (j in i:(length(niveis)-1))
            {
              if (homogeneidade$'Pr(>F)'[1]<0.05)
              {
                matriz[j,i]<-teste$output$games.howell$p[k]
              }
              else
              {
                matriz[j,i]<-teste$output$tukey$p[k]
              }
              k<-k+1
            }
          }
          #Coloca-se no programa a matriz obtida
          matriz <- data.frame(multcompLetters(fullPTable(matriz))['Letters'])
          str_aux<-""
          for (i in 1:length(matriz[,1]))
          {
            str_aux<-paste(str_aux,rownames(matriz)[i],sep="\t")
            str_aux<-paste(str_aux,": ",sep="")
            str_aux<-paste(str_aux,matriz[1,1],sep="")
            str_aux<-paste(str_aux,"; ",sep="")
          }
        }
        #No caso não paramêtrico
        else
        {
          #p-valor entre os grupos
          test<-pairwise.wilcox.test(percentagem,col_escolhida,p.adj='bonferroni',exact=F)$p.value
          #Escreve-se numa matriz as opções e os p-valores associados entre eles
          matriz<-multcompLetters(fullPTable(test),
                                  compare="<",
                                  threshold=0.05, # p-value to use as significance threshold
                                  Letters=letters,
                                  reversed = FALSE)$LetterMatrix
          #Coloca-se no programa a matriz obtida
          str_aux<-""
          for (i in 1:length(matriz[,1]))
          {
            str_aux<-paste(str_aux,rownames(matriz)[i],sep="\t")
            str_aux<-paste(str_aux,": ",sep="")
            for (j in 1:length(matriz[1,]))
            {
              if (matriz[i,j])
              {
                str_aux<-paste(str_aux,colnames(matriz)[j],sep="")
              }
            }
            str_aux<-paste(str_aux,"; ",sep="")
          }
        }
        test<-pairwise.wilcox.test(percentagem,col_escolhida,p.adj='bonferroni',exact=F)$p.value
        t<-cbind(rownames(test),round(test,4))
        b<-gtable(t,container=valor_janela$value)
        size(b)<-c(825,200)

        glabel("Grupos:",container=valor_janela$value)
        glabel(str_aux,container=valor_janela$value)
        glabel("",container=valor_janela$value)
      }
      glabel(paste("Sumário de ",valor_testar,sep=""),container=valor_janela$value)
    }
    #Caso seja análise às EBs ou às OS
    else
    {
      if (analise_nome=="Análise EBs")
      {
        if (svalue(cb_dias_teste)=="Leituras Recebidas")
        {
          depen <-"leitura_obtida"
        }
        else
        {
          depen <-"fim_1dia"
        }
      }
      else
      {
        depen<-"sucesso"
      }
      valido<-FALSE
      #Utilizador tem de inserir a proporção que pretende ver realizada
      per<-ginput("Proporção a utilizar para o teste",text="",title="Proporção", icon="question")
      if (!is.na(as.numeric(per)))
      {
        if (as.numeric(per)>0 & as.numeric(per)<1)
        {
          valido<-TRUE
        }
      }
      if (valido)
      {
        #Para cada uma das variáveis realiza-se o teste binomial e escreve-se no programa
        for (i in 1:length(variaveis))
        {
          tabela_aux_bin<-tabela_aux[col_escolhida==variaveis[i],]
          texto_variavel<-paste("Teste binomial para a variável ",variaveis[i],sep="")
          if (analise_nome=="Análise EBs")
          {
            texto_variavel<-paste(paste(paste(texto_variavel, " (", sep=""),svalue(cb_dias_teste),sep=""), ")", sep="")
          }
          glabel(texto_variavel, container=valor_janela$value)
          num_suc<-tabela_aux_bin[tabela_aux_bin[[depen]]==1,][[depen]]
          num_insuc<-tabela_aux_bin[tabela_aux_bin[[depen]]==0,][[depen]]
          teste<-binom.test(c(length(num_suc),length(num_insuc)), p = as.numeric(per))
          media<-mean(tabela_aux_bin[[depen]])
          if (teste$p.value>=0.05)
          {
            glabel(paste(paste(paste(paste(paste("p-valor para o teste binomial bilateral (", variaveis[i],sep=""),"): ",toString(round(teste$p.value,4)),sep=""), " (=", sep=""),toString(as.numeric(per)*100), sep=""),"%)",sep=""), container=valor_janela$value)
          }
          else
          {
            glabel(paste(paste(paste(paste(paste("p-valor para o teste binomial bilateral (", variaveis[i],sep=""),"): ",toString(round(teste$p.value,4)),sep=""), " (<>", sep=""),toString(as.numeric(per)*100), sep=""),"%)",sep=""), container=valor_janela$value)
          }
          glabel("",container=valor_janela$value)
        }
        glabel("",container=valor_janela$value)
        glabel(paste("Médias",sep=""),container=valor_janela$value)
      }
      else
      {
        gmessage("Valor de teste inválido!")
      }
    }
    #Escreve-se no programa um sumário das opções estudadas (média, quadrantes, etc.). No caso de se ter utilizado o teste binomial, só se apresentará as médias
    sumarios<-c()
    aux_nomes<-c()
    for (i in 1:length(variaveis))
    {
      aux_nomes<-c(aux_nomes,toString(variaveis[i]))
      if (analise_nome=="Análise DTCs")
      {
        sumarios<-cbind(sumarios, do.call("<-",list(paste("sum_",variaveis[i],sep=""),summary(tabela_aux[tabela_aux[[valor_testar]]==variaveis[i],][[depen]]/tabela_aux[tabela_aux[[valor_testar]]==variaveis[i],][["qtd_EBs_regi"]]*100))))
      }
      else if (analise_nome=="Análise EBs")
      {
        sumarios<-cbind(sumarios, do.call("<-",list(paste("sum_",variaveis[i],sep=""),summary(tabela_aux[tabela_aux[[valor_testar]]==variaveis[i],][[depen]]))))
      }
      else
      {
        sumarios<-cbind(sumarios, do.call("<-",list(paste("sum_",variaveis[i],sep=""),summary(tabela_aux[tabela_aux[[valor_testar]]==variaveis[i],]$sucesso))))
      }
    }
    if (analise_nome=="Análise DTCs")
    {
      colnames(sumarios)<-aux_nomes
      s<-cbind(rownames(sumarios),round(sumarios,4))
      c<-gtable(s,container=valor_janela$value)
      size(c)<-c(825,200)
    }
    else
    {
      s<-c()
      for (i in 1:length(sumarios[4,]))
      {
        s<-cbind(s,round(sumarios[4,i],4))
      }
      colnames(s)<-aux_nomes
      if (valido)
      {
        c<-gtable(s,container=valor_janela$value)
      }
    }
  }
}

#Criação do botão "Realizar teste"
botao_teste <- gbutton("Realizar Teste", "teste", handler=realizar_teste)
add(tmpTestTempPercT,botao_teste)
