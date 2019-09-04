#Caso os packages não existem aquando a utilização do programa, são adicionados
if(!require(rcompanion)){install.packages("rcompanion")}
if(!require(FSA)){install.packages("FSA")}
if(!require(haven)){install.packages("haven")}
if(!require(psych)){install.packages("psych")}
if(!require(nortest)){install.packages("nortest")}
if(!require(car)){install.packages("car")}
if(!require(multcompView)){install.packages("multcompView")}
if(!require(emmeans)){install.packages("emmeans")}
if(!require(Rmisc)){install.packages("Rmisc")}
if(!require(prodlim)){install.packages("prodlim")}
if(!require(gWidgetsRGtk2)){install.packages("gWidgetsRGtk2", dep = TRUE)}
if(!require(userfriendlyscience)){install.packages("userfriendlyscience")}

#Bibliotecas utilizadas
library(rcompanion)
library(multcompView)
library(multcomp)
library(haven)
library(nortest)
library(prodlim)
library(grid)

#Bibliotecas requeridas para a utilização do interface gráfico
require(mgcv)
require(gWidgets)
options(guiToolkit="RGtk2")

#Função que cria ponteiros. Os ponteiros servem para que as variáveis que tenham esta característica sejam globais dentro do programa até ao seu encerramento
novo_ponteiro<-function(valor_inserir)
{  
  object<-new.env(parent=globalenv())  
  object$value<-valor_inserir  
  class(object)<-'pointer'
  
  return(object)  
}

#Função para atualizar o valor do ponteiro
atualizar_ponteiro<-function (object, ...) 
{
  UseMethod("atualizar_ponteiro")   
}

#Método para atualizar o valor do ponteiro
atualizar_ponteiro.pointer<-function(object,novo_valor)
{
  if (!is(object, "pointer")) 
  { 
    stop(" 'object' argument must be of class 'pointer' .") 
  }   
  object$value<-novo_valor  
}


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

#Função que cria o interface gráfico
interface<-function(tabela_original, analise_nome, simulador)
{
  #Alteração de data para dia da semana
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
  
  #As variáveis ponteiro criadas podem ser acedidas e alteradas em qualquer função do programa. Tem o mesmo funcionamento que uma variável global
  divisoes<-novo_ponteiro(c())
  tabela_a_trabalhar<-novo_ponteiro(tabela_original)
  
  #Opção que permite criar uma nova coluna na tabela que consiste na identificação do intervalo que cada DTC pertence dependendo do número de EBs
  fazer_div<-gconfirm("Deseja dividir os DTCs por grupos, consoante o número de EBs que possuem?")
  #Se a opção for a de criação da coluna
  if (fazer_div)
  {
    #Criação de grupos de EBs
    divisoes_aux<-funcao_divisao(min(tabela_a_trabalhar$value[["qtd_EBs_regi"]]),max(tabela_a_trabalhar$value[["qtd_EBs_regi"]]))
    #Verifica se as divisões foram criadas
    if (!all(divisoes_aux==c(-1)))
    {
      atualizar_ponteiro(divisoes,divisoes_aux)
    }
    #Colocar coluna em tabela se as divisões criadas forem válidas
    if (length(divisoes$value)!=0)
    {
      #Verifica se todos os intervalos contêm pelo menos um elemento
      atualizar_ponteiro(divisoes,funcao_verificar_divisoes(tabela_a_trabalhar$value, divisoes$value))
      if (length(divisoes$value)!=0)
      {
        atualizar_ponteiro(tabela_a_trabalhar,funcao_criar_coluna_num_EBs(tabela_a_trabalhar$value, divisoes$value))
      }
    }
  }
  
  #Tabela que será utilizada caso se pretende reiniciar a tabela_a_trabalhar
  tabela_sem_alteracoes<-novo_ponteiro(tabela_a_trabalhar$value)
  
  #Colunas que se poderão alterar
  if (analise_nome=="Análise DTCs") #DTCs
  {
    colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","fim_1dia","leitura_obtida","tempo_medio"))
  }
  else if (analise_nome=="Análise EBs") #EBs
  {
    colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","fim_1dia","leitura_obtida","tempo_leit_EB"))
  }
  else #OS
  {
    colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","sucesso","tempo_OS"))
  }
  
  if (!identical(colunas_retirar, integer(0)))
  {
    colunas_alterar<-novo_ponteiro(gsub(" ", "", colnames(summary(tabela_a_trabalhar$value))[-colunas_retirar], fixed = TRUE))
  }
  else
  {
    colunas_alterar<-novo_ponteiro(gsub(" ", "", colnames(summary(tabela_a_trabalhar$value)), fixed = TRUE))
  }
  
  
  #Colunas que se poderão testar
  if (analise_nome=="Análise EBs") #EBs
  {
    colunas_retirar<-which(colunas_alterar$value %in% c("config","marca_DTC","firmware"))
    if (!identical(colunas_retirar, integer(0)))
    {
      colunas_teste<-novo_ponteiro(colunas_alterar$value[-colunas_retirar])
    }
    else
    {
      colunas_teste<-novo_ponteiro(colunas_alterar$value)
    }
  }
  else #DTCs e OS
  {
    colunas_teste<-novo_ponteiro(colunas_alterar$value)
  }
  
  #Obtém todas as opções de cada uma das colunas que se poderão alterar 
  aux_lista_opcoes<-list()
  for (i in 1:length(colunas_alterar$value))
  {
    aux_lista_opcoes[[colunas_alterar$value[i]]]<-c(as.character(unique(tabela_a_trabalhar$value[[colunas_alterar$value[i]]])),"Todos")
  }
  lista_opcoes<-novo_ponteiro(aux_lista_opcoes)
  
  #Cria-se uma lista que terá todas as opções de cada uma das colunas que se poderão obter gráficos separadamente
  aux_lista_opcoes_qq<-list()
  for (i in 1:length(colunas_teste$value))
  {
    aux_lista_opcoes_qq[[colunas_teste$value[i]]]<-as.character(unique(tabela_a_trabalhar$value[[colunas_teste$value[i]]]))
  }
  lista_opcoes_qq<-novo_ponteiro(aux_lista_opcoes_qq)
  
  ######################################################## Layout ############################################################
  
  window <- gwindow(titulo) #Janela com o título
  gstatusbar("Autor: Bernardo Amaral", cont=window) #Autor
  getToolkitWidget(window)$maximize() #Maximiza a janela
  BigGroup <- ggroup(cont=window)
  group <- ggroup(horizontal=FALSE, container=BigGroup)
  
  #Adiciona um gráfico na janela
  valor_janela<-novo_ponteiro(ggraphics())
  add(BigGroup,valor_janela$value)
  
  ######################################################## Sumário ###########################################################
  
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
  
  ##################################################### Alterar Tabela #######################################################
  
  #Cria frame para a opção "Alterar Tabela"
  tmpAlt <- gframe("Alterar Tabela", container=group, horizontal=FALSE)
  
  #Função que altera o combobox das opções da coluna consoante a coluna selecionada
  alterar_coluna_opcao<-function(h,...)
  {
    #Apaga o Combobox das opções e o botao existentes no frame
    delete(tmpAlt,botao_alterar)
    delete(tmpAlt,alterar_opcao$value)
    #Aquando selecionada a coluna, as opções do combobox das opções são alteradas
    pos<-match(svalue(h$obj),colunas_alterar$value)
    atualizar_ponteiro(alterar_opcao,gcombobox(lista_opcoes$value[[colunas_alterar$value[pos]]]))
    #Adiciona o combobox alterado e o botão ao frame
    add(tmpAlt,alterar_opcao$value)
    add(tmpAlt,botao_alterar)
  }
  
  #Combobox das colunas que se podem alterar
  alterar_coluna <- novo_ponteiro(gcombobox(colunas_alterar$value, handler=alterar_coluna_opcao))
  add(tmpAlt,alterar_coluna$value)
  
  #Combobox das opções dentro da coluna selecionada do combobox anterior 
  alterar_opcao <- novo_ponteiro(gcombobox(lista_opcoes$value[[colunas_alterar$value[1]]]))
  add(tmpAlt,alterar_opcao$value)
  
  #Função que altera a tabela a trabalhar aquando carregado o botão "Alterar" consoante as opções escolhidas nos comboboxs
  #A opção escolhida é considerada, todas as outras opções da coluna selecionada são desconsideradas
  alterar_tabela<-function(h,...)
  {
    #Valor do combobox correspondente à coluna
    coluna_utilizada<-svalue(alterar_coluna$value)
    #Valor da tabela sem alterações
    tabela_auxiliar<-tabela_sem_alteracoes$value
    #Valor da opção escolhida
    alterar_valor<-svalue(alterar_opcao$value)
    #Encontra na tabela sem alterações a posição da coluna a alterar
    alterar<-which(coluna_utilizada==colnames(tabela_sem_alteracoes$value))
    for (i in 1:length(tabela_sem_alteracoes$value))
    {
      #Se não for a coluna a alterar
      if (i!=alterar)
      {
        #Verifica se a coluna é do tipo "factor"
        if (sapply(tabela_sem_alteracoes$value, class)[i]=="factor")
        {
          #Se for do tipo factor, verifica na tabela a trabalhar, a coluna só tinha o valor de uma opção 
          if (length(unique(tabela_a_trabalhar$value[,i]))==1)
          {
            #Se sim, é obtido o valor dessa opção para que a tabela mantenha essa opção alterada
            var_nome<-unique(tabela_a_trabalhar$value[,i])
            tabela_auxiliar<-tabela_auxiliar[tabela_auxiliar[,i]==var_nome,]
          }
        }
      }
    }
    #A alteração escolhida é realizada.
    if (alterar_valor!="Todos") #Utilizador escolheu uma opção especifica
    {
      tabela_auxiliar<-tabela_auxiliar[tabela_auxiliar[,alterar]==alterar_valor,]
    }
    #Verifica se com a alteração feita, a tabela é nula. Se sim, a operação não é realizada
    if (!empty(tabela_auxiliar))
    {
      #Apaga-se os botões e comboboxes relativos aos testes
      if (length(colunas_teste$value)!=0)
      {
        delete(tmpTestTempPercT,botao_teste)
        if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
        {
          if (analise_nome=="Análise EBs")
          {
            delete(tmpTestTempPercT,botao_tempo)
          }
          delete(tmpTestTempPercT,perc_func)
        }
        if (analise_nome=="Análise DTCs")
        {
          delete(tmpTestGraObriOpc,botao_qq)
          delete(tmpTestGraOpc,botao_big)
        }
        delete(tmpTestGraOpc,botao_plot)
        delete(tmpTestOpcGra,cb_opcao_graficos)
        if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
        {
          delete(tmpTestOpcGra,cb_dias_teste)
        }
        delete(tmpTestVar, teste_opcao$value)
        delete(tmpTestVar, teste_coluna$value)
      }
      
      #Atualiza-se a tabela consoante as alterações feitas
      atualizar_ponteiro(tabela_a_trabalhar,tabela_auxiliar)
      if (analise_nome=="Análise EBs")
      {
        colunas_retirar<-which(colunas_alterar$value %in% c("config","marca_DTC","firmware"))
        if (!identical(colunas_retirar, integer(0)))
        {
          aux_colunas_teste<-colunas_alterar$value[-colunas_retirar]
        }
        else
        {
          aux_colunas_teste<-colunas_alterar$value
        }
      }
      else
      {
        aux_colunas_teste<-colunas_alterar$value
      }
      i<-1
      #Retira nas colunas alterar as colunas que só possuem uma opção
      while (i<=length(aux_colunas_teste))
      {
        if (length(unique(tabela_a_trabalhar$value[[aux_colunas_teste[i]]]))==1)
        {
          aux_colunas_teste<-aux_colunas_teste[-i]
        }
        else
        {
          i<-i+1
        }
      }
      #Atualiza-se os combobox correspondentes ao teste de colunas
      atualizar_ponteiro(colunas_teste,aux_colunas_teste)
      atualizar_ponteiro(teste_coluna,gcombobox(colunas_teste$value, handler=teste_coluna_opcao))
      
      #Atualiza-se a lista que terá todas as opções de cada uma das colunas para se criar gráficos isoladamente
      aux_lista_opcoes_qq<-list()
      i<-1
      while(i<=length(colunas_teste$value))
      {
        aux_lista_opcoes_qq[[colunas_teste$value[i]]]<-as.character(unique(tabela_a_trabalhar$value[[colunas_teste$value[i]]]))
        i<-i+1
      }
      atualizar_ponteiro(lista_opcoes_qq,aux_lista_opcoes_qq)
      atualizar_ponteiro(teste_opcao, gcombobox(lista_opcoes_qq$value[[teste_coluna$value[1]]]))
      
      #Se houver colunas para teste, são repostas no frame "testes" os botões e comboboxes necessários
      if (length(colunas_teste$value)!=0)
      {
        add(tmpTestVar,teste_coluna$value)
        add(tmpTestVar,teste_opcao$value)
        if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
        {
          add(tmpTestOpcGra,cb_dias_teste)
        }
        add(tmpTestOpcGra,cb_opcao_graficos)
        add(tmpTestGraOpc,botao_plot)
        if (analise_nome=="Análise DTCs")
        {
          add(tmpTestGraOpc,botao_big)
          add(tmpTestGraObriOpc,botao_qq)
        }
        if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
        {
          add(tmpTestTempPercT,perc_func)
          if (analise_nome=="Análise EBs")
          {
            add(tmpTestTempPercT,botao_tempo)
          }
        }
        add(tmpTestTempPercT,botao_teste)
      }
      gmessage("Alteração feita com sucesso.")
    }
    else
    {
      gmessage("Alteração não realizada, pois a tabela fica vazia!")
    }
  }
  
  #Criação do botão "Alterar"
  botao_alterar <- gbutton("Alterar", "alterar",handler=alterar_tabela)
  add(tmpAlt,botao_alterar)
  
  ######################################################## Histograma ########################################################
  
  #Cria frame para a opção "Histograma da tabela atual"
  tmpHis<-gframe("Histograma da tabela atual", container=group, horizontal = FALSE)
  
  #Se o ficheiro for das leituras, cria um combobox com duas opções: receção de leituras ao primeiro dia/receção de leituras
  if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
  {
    cb_dias <- gcombobox(c("Leituras Recebidas","Leituras 1º dia"))
    add(tmpHis,cb_dias)
  }
  
  #Função que cria um histograma da tabela atual (Se a tabela for de leituras, então cria o histograma consoante a escolha no combobox anterior)
  plot_geral<-function(h,...)
  {
    #Atualiza-se o valor da janela e coloca-se um gráfico na janela (que posteriormente será um histograma)
    delete(BigGroup,valor_janela$value)
    atualizar_ponteiro(valor_janela,ggraphics())
    add(BigGroup,valor_janela$value)
    if (analise_nome=="Análise DTCs")
    {
      #Histograma da percentagem de EBs recebidas nos DTCs
      if (svalue(cb_dias)=="Leituras Recebidas")
      {
        depend<-tabela_a_trabalhar$value[["leitura_obtida"]]/tabela_a_trabalhar$value[["qtd_EBs_regi"]]*100
        hist(depend,breaks=20, main="Percentagem De Leituras DTCs", xlab = "Percentagem", ylab="Número de DTCs")
      }
      #Histograma da percentagem de EBs recebidas nos DTCs ao fim de um dia
      else
      {
        depend<-tabela_a_trabalhar$value[["fim_1dia"]]/tabela_a_trabalhar$value[["qtd_EBs_regi"]]*100
        hist(depend,breaks=20, main="Percentagem De Leituras DTCs 1º dia", xlab = "Percentagem", ylab="Número de DTCs")
      }
    }
    else if (analise_nome=="Análise EBs")
    {
      #Histograma que representa o sucesso (valor=1) ou insucesso (valor=0) das EBs estudadas
      if (svalue(cb_dias)=="Leituras Recebidas")
      {
        depend<-tabela_a_trabalhar$value[["leitura_obtida"]]
        hist(depend,breaks=20, main="Leituras EBs", xlab = "Leitura Realizada", ylab="Número de EBs")
      }
      #Histograma que representa o sucesso (valor=1) ou insucesso (valor=0) ao fim de um dia das EBs estudadas
      else
      {
        depend<-tabela_a_trabalhar$value[["fim_1dia"]]
        hist(depend,breaks=20, main="Leituras EBs 1º Dia", xlab = "Leitura Realizada", ylab="Número de EBs")
      }
    }
    #Histograma que representa o sucesso (valor=1) ou insucesso (valor=0) das OSs estudadas
    else
    {
      depend<-tabela_a_trabalhar$value[["sucesso"]]
      hist(depend,breaks=20, main="OS Realizadas", xlab = "OS realizada", ylab="Número de OS")
    }
  }
  
  #Criação do botão "Histograma"
  botao_hist <- gbutton("Histograma", "hist_tot", handler=plot_geral)
  add(tmpHis,botao_hist)
  
  ################################################### Testes e Gráficos ######################################################
  
  #Cria frame para a opção "Teste"
  tmpTest <- gframe("Realizar testes e desenhar gráficos", container=group, horizontal = FALSE)
  
  #Cria frame dentro do frame "Teste" para as variáveis
  tmpTestVar <- gframe("Variáveis e Opções", container=tmpTest, horizontal = FALSE)
  
  #Função que altera o combobox das opções da coluna consoante a coluna selecionada
  teste_coluna_opcao<-function(h,...)
  {
    #Apaga o Combobox das opções existentes no frame
    delete(tmpTestVar,teste_opcao$value)
    
    #Aquando selecionada a coluna, as opções do combobox das opções são alteradas e atualizadas
    pos<-match(svalue(h$obj),colunas_teste$value)
    lista_aux<-lista_opcoes_qq$value[[colunas_teste$value[pos]]]
    i<-1
    while (i<=length(lista_aux))
    {
      if (sum(tabela_a_trabalhar$value[[svalue(h$obj)]]==lista_aux[i])==0)
      {
        lista_aux<-lista_aux[-i]
      }
      else
      {
        i<-i+1
      }
    }
    atualizar_ponteiro(teste_opcao,gcombobox(lista_aux))
    #Adiciona o combobox alterado ao frame
    add(tmpTestVar,teste_opcao$value)
  }
  
  #Cria um combobox com as colunas disponíveis para teste
  teste_coluna <- novo_ponteiro(gcombobox(colunas_teste$value, handler=teste_coluna_opcao))
  add(tmpTestVar,teste_coluna$value)
  
  #Combobox das opções dentro da coluna selecionada do combobox na frame teste
  teste_opcao <- novo_ponteiro(gcombobox(lista_opcoes_qq$value[[teste_coluna$value[1]]]))
  add(tmpTestVar,teste_opcao$value)
  
  #Cria frame dentro do frame "Teste" com as opções para os gráficos que necessitam de escolhas do utilizador
  tmpTestOpcGra <- gframe("Gráficos Escolhas", container=tmpTest, horizontal = FALSE)
  
  #Se o ficheiro for das leituras, cria um combobox com duas opções: receção de leituras ao primeiro dia/receção de leituras
  if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
  {
    cb_dias_teste<-gcombobox(c("Leituras Recebidas","Leituras 1º dia"))
    add(tmpTestOpcGra,cb_dias_teste)
  }
  
  #Combobox com opções para mostrar nos gráficos todas as opções, ou apenas a opção selecionada
  cb_opcao_graficos <- gcombobox(c("Todas Opções","Apenas Opção Selecionada"))
  add(tmpTestOpcGra,cb_opcao_graficos)
  
  #Cria frame dentro do frame "Teste" para os gráficos que necessitam da opção para se desenhar
  tmpTestGraOpc <- gframe("Gráficos (com escolha)", container=tmpTest, horizontal = FALSE)
  
  #Função que permite criar o histograma do sucesso/insucesso (OS/EBs) ou percentagens (DTC) para cada variável na coluna associada
  realizar_plot<-function(h,...)
  {
    #Atualiza-se o valor da janela e coloca-se um gráfico na janela
    delete(BigGroup,valor_janela$value)
    atualizar_ponteiro(valor_janela,ggraphics())
    add(BigGroup,valor_janela$value)
    #Obtém-se o valor da coluna escolhida
    coluna_escolhida<-svalue(teste_coluna$value)
    #Verifica se é utilizada a tabela toda, ou apenas a opção selecionada
    if (svalue(cb_opcao_graficos)=="Apenas Opção Selecionada")
    {
      coluna_opcao_escolhida<-svalue(teste_opcao$value)
      tabela_aux<-droplevels(tabela_a_trabalhar$value[tabela_a_trabalhar$value[[coluna_escolhida]]==coluna_opcao_escolhida,])
    }
    else
    {
      tabela_aux<-tabela_a_trabalhar$value
    }
    #Se for análise das leituras, o programa verifica qual a opção escolhida no combobox existente no frame "teste" antes de criar os histogramas 
    if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
    {
      leituras_todas<-svalue(cb_dias_teste)
      if (leituras_todas=="Leituras 1º dia")
      {
        nome_depend<-"fim_1dia"
      }
      else
      {
        nome_depend<-"leitura_obtida"
      }
      #coluna correspondente ao número de leituras que foram enviadas da tabela utilizada
      depend<-tabela_aux[[nome_depend]]
      #Se for DTCs, calcula-se a percentagem e cria-se o histograma
      if (analise_nome=="Análise DTCs")
      {
        depend<-depend/tabela_aux[["qtd_EBs_regi"]]*100
        hist(depend ~ tabela_aux[[coluna_escolhida]],breaks=20,xlab = paste(paste("Percentagem de Leituras (",leituras_todas, sep=""), ")", sep=""),ylab="Número de DTCs")
      }
      #Se for EBs, cria-se o histograma
      else
      {
        hist(depend ~ tabela_aux[[coluna_escolhida]],breaks=20,xlab = paste(paste("Leitura realizada (",leituras_todas, sep=""), ")", sep=""),ylab="Número de EBs")
      }
    }
    #Se for OS, obtém-se o sucesso/insucesso das OSs realizadas e cria-se o histograma
    else
    {
      depend<-tabela_aux[["sucesso"]]
      hist(depend ~ tabela_aux[[coluna_escolhida]],breaks=20,xlab = "OS realizada",ylab="Número de OS")
    }
  }
  
  #Criação do botão "Histograma"
  botao_plot <- gbutton("Histograma", "hist", handler=realizar_plot)
  add(tmpTestGraOpc,botao_plot)
  
  #Se o ficheiro for a análise de leituras nos DTCs
  if (analise_nome=="Análise DTCs")
  {
    #Função que cria uma caixa de bigodes para cada variável na coluna associada
    realizar_big<-function(h,...)
    {
      #Atualiza-se o valor da janela e coloca-se um gráfico na janela
      delete(BigGroup,valor_janela$value)
      atualizar_ponteiro(valor_janela,ggraphics())
      add(BigGroup,valor_janela$value)
      #Obtém-se o valor da coluna escolhida
      coluna_escolhida<-svalue(teste_coluna$value)
      titulo<-coluna_escolhida
      titulo_x<-""
      #Verifica se é utilizada a tabela toda, ou apenas a opção selecionada
      if (svalue(cb_opcao_graficos)=="Apenas Opção Selecionada")
      {
        coluna_opcao_escolhida<-svalue(teste_opcao$value)
        tabela_aux<-droplevels(tabela_a_trabalhar$value[tabela_a_trabalhar$value[[coluna_escolhida]]==coluna_opcao_escolhida,])
        titulo_x<-coluna_opcao_escolhida
      }
      else
      {
        tabela_aux<-tabela_a_trabalhar$value
      }
      #Verifica qual a opção escolhida no combobox existente no frame "teste" antes de criar as caixas de bigodes 
      leituras_todas<-svalue(cb_dias_teste)
      if (leituras_todas=="Leituras 1º dia")
      {
        nome_depend<-"fim_1dia"
      }
      else
      {
        nome_depend<-"leitura_obtida"
      }
      #Calcula-se a percentagem e cria-se a caixa de bigodes
      depend<-tabela_aux[[nome_depend]]/tabela_aux[["qtd_EBs_regi"]]*100
      plot(depend ~ tabela_aux[[coluna_escolhida]], xlab=titulo_x, ylab=paste(paste("Percentagem de Leituras (",leituras_todas, sep=""), ")", sep=""), main=titulo)
    }
    
    #Criação do botão "Caixa de Bigodes"
    botao_big <- gbutton("Caixa de Bigodes", "cb", handler=realizar_big)
    add(tmpTestGraOpc,botao_big)
    
    #Cria frame dentro do frame "Teste" para os gráficos que só funcionam com a opção escolhida
    tmpTestGraObriOpc <- gframe("Gráficos (opção escolhida)", container=tmpTest, horizontal = FALSE)
    
    #Função que cria o QQPlot da variável escolhida do combobox acima
    criar_qqplot<-function(h,...)
    {
      #Atualiza-se o valor da janela e coloca-se um gráfico na janela
      delete(BigGroup,valor_janela$value)
      atualizar_ponteiro(valor_janela,ggraphics())
      add(BigGroup,valor_janela$value)
      
      #Obtém-se o valor da coluna escolhida
      coluna_escolhida<-svalue(teste_coluna$value)
      coluna_opcao_escolhida<-svalue(teste_opcao$value)
      titulo<-paste(paste(paste(coluna_escolhida, " (", sep=""), coluna_opcao_escolhida, sep=""), ")", sep="")
      tabela_aux<-droplevels(tabela_a_trabalhar$value[tabela_a_trabalhar$value[[coluna_escolhida]]==coluna_opcao_escolhida,])
      
      #Verifica qual a opção escolhida no combobox existente no frame "teste" antes de criar as caixas de bigodes 
      leituras_todas<-svalue(cb_dias_teste)
      if (leituras_todas=="Leituras 1º dia")
      {
        nome_depend<-"fim_1dia"
      }
      else
      {
        nome_depend<-"leitura_obtida"
      }
      #Calcula-se a percentagem e cria-se a caixa de bigodes
      depend<-tabela_aux[[nome_depend]]/tabela_aux[["qtd_EBs_regi"]]*100
      qqnorm(depend, main=titulo)
      qqline(depend, col = "steelblue")
    }
    
    #Criação do botão "QQPlot"
    botao_qq <- gbutton("QQPlot", "QQ", handler=criar_qqplot)
    add(tmpTestGraObriOpc,botao_qq)
  }
  
  #Cria frame dentro do frame "Teste" para o teste e as percentagens de leituras (DTCs) e tempos (EBs)
  tmpTestTempPercT <- gframe("Testes para analisar", container = tmpTest, horizontal = FALSE)
  
  if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
  {
    #Função que cria uma text box com as percentagens de leitura para a opção do combobox escolhida para cada um dos elementos da coluna a testar
    perc_leit<-function(h,...)
    {
      #Atualiza-se o valor da janela e coloca-se um grupo na janela
      delete(BigGroup,valor_janela$value)
      atualizar_ponteiro(valor_janela,ggroup(horizontal=FALSE, container=BigGroup))
      add(BigGroup,valor_janela$value)
      #Valor da coluna a testar
      valor_testar<-svalue(teste_coluna$value)
      #Retira os valores que existem na coluna a testar
      variaveis<-unique((tabela_a_trabalhar$value[[valor_testar]]))
      leit_suc_fim<-c()
      total<-c()
      leit_suc_um<-c()
      #Para cada uma das variáveis da coluna a testar
      for (i in 1:length(variaveis))
      {
        #Cria a tabela só com a variável selecionada
        tabela_aux<-tabela_a_trabalhar$value[tabela_a_trabalhar$value[[valor_testar]]==variaveis[i],]
        #sum(tabela_aux[["leitura_obtida"]]) -> Faz a soma de todos os EBs que enviaram a informação da leitura para os Sistemas Centrais
        leit_suc_fim<-c(leit_suc_fim,sum(tabela_aux[["leitura_obtida"]]))
        #Se for a análise de leituras nos EBs
        if (analise_nome=="Análise EBs")
        {
          #nrow(tabela_aux) -> Número de linhas 
          total<-c(total,nrow(tabela_aux))
        }
        #Se for análise de leituras nos DTCs
        else
        {
          #sum(tabela_aux[["qtd_EBs_regi"]]) -> Soma de todas as EBs em analise
          total<-c(total,sum(tabela_aux[["qtd_EBs_regi"]]))
        }
        #Obtém a percentagem de EBs que enviaram a sua informação da leitura para os Sistemas Centrais
        percentagem<-as.character(round(as.double(leit_suc_fim[i])/total[i]*100,4))
        #Coloca no programa os resultados
        glabel(paste(paste(paste(paste(paste(paste(paste(paste("Número de leituras (",variaveis[i],sep=""),"): ",sep=""),leit_suc_fim[i],sep=""), " em ",sep=""), total[i], sep=""), " (", sep=""), percentagem, sep=""), "%)", sep=""), container=valor_janela$value)
      }
      #Separador
      glabel("",container=valor_janela$value)
      #Para cada uma das variáveis da coluna a testar
      for (i in 1:length(variaveis))
      {
        #É criada a tabela com valores tais que contém a tal variável da coluna a testar
        tabela_aux<-tabela_a_trabalhar$value[tabela_a_trabalhar$value[[valor_testar]]==variaveis[i],]
        
        #Faz a soma de todas as EBs que enviaram a sua leitura ao fim de um dia
        leit_suc_um<-c(leit_suc_um,sum(tabela_aux[["fim_1dia"]]))
        #Obtém a percentagem de EBs que enviaram a sua informação da leitura para os Sistemas Centrais no próprio dia
        percentagem<-as.character(round(as.double(leit_suc_um[i])/total[i]*100,4))
        glabel(paste(paste(paste(paste(paste(paste(paste(paste("Número de leituras primeiro dia (",variaveis[i],sep=""),"): ",sep=""),leit_suc_um[i], sep=""), " em ", sep=""),total[i],sep=""), " (",sep=""), percentagem,sep=""), "%)",sep=""),container=valor_janela$value)
      }
      #Separador
      glabel("",container=valor_janela$value)
      for (i in 1:length(variaveis))
      {
        #Obtém a percentagem, das EBs que enviaram a informação da sua leitura para os Sistemas Centrais, daquelas que consigaram mandar no próprio dia
        percentagem<-as.character(round(as.double(leit_suc_um[i])/leit_suc_fim[i]*100,4))
        glabel(paste(paste(paste(paste("Percentagem primeiro dia (",variaveis[i],sep=""),"): ",sep=""),percentagem, sep=""), "%", sep=""),container=valor_janela$value)
      }
    }
    
    #Botão que permite mostrar as percentagens
    perc_func<-gbutton("Percentagem de leituras", "PL", handler=perc_leit)
    add(tmpTestTempPercT,perc_func)
    
    #Se o ficheiro a analisar for de análise às EBs
    if (analise_nome=="Análise EBs")
    {
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
    }
  }
  
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
  
  #################################################### Novos intervalos ######################################################
  
  #Função que permite criar novos intervalos para a divisão de DTCs por EBs (Nota: A tabela é reiniciada, todas as alterações são desfeitas)
  novos_intervalos<-function(h,...)
  {
    visible(window)<-FALSE
    
    #Criação de grupos de EBs
    divisoes_aux<-funcao_divisao(min(tabela_original[["qtd_EBs_regi"]]),max(tabela_original[["qtd_EBs_regi"]]))
    #Se a operação não foi cancelada
    if (!all(divisoes_aux==c(-1)))
    {
      #O programa verifica se as divisões inseridas são iguais às que já estavam inseridas
      if (all(divisoes_aux==divisoes$value) & !is.null(divisoes$value))
      {
        gmessage("As alterações não foram realizadas, porque os intervalos introduzidos são iguais aos que havia anteriormente!")
      }
      else
      {
        #Verifica se todos os intervalos contêm pelo menos um elemento
        divisoes_aux<-funcao_verificar_divisoes(tabela_original, divisoes_aux)
        #Se a operação não foi cancelada
        if (length(divisoes_aux)!=0)
        {
          #Apaga os comboboxs que se fizeram as alterações. Caso as opções de gráficos e testes não estiverem presentes no programa, adicionar posteriormente a realização das operações
          if (length(colunas_teste$value)!=0)
          {
            delete(tmpTestTempPercT,botao_teste)
            if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
            {
              if (analise_nome=="Análise EBs")
              {
                delete(tmpTestTempPercT,botao_tempo)
              }
              delete(tmpTestTempPercT,perc_func)
            }
            if (analise_nome=="Análise DTCs")
            {
              delete(tmpTestGraObriOpc,botao_qq)
              delete(tmpTestGraOpc,botao_big)
            }
            delete(tmpTestGraOpc,botao_plot)
            delete(tmpTestOpcGra,cb_opcao_graficos)
            if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
            {
              delete(tmpTestOpcGra,cb_dias_teste)
            }
            delete(tmpTestVar, teste_opcao$value)
            delete(tmpTestVar, teste_coluna$value)
          }
          
          delete(tmpAlt,botao_alterar)
          delete(tmpAlt,alterar_opcao$value)
          delete(tmpAlt,alterar_coluna$value)
          
          #Atualiza as divisões feitas
          atualizar_ponteiro(divisoes,divisoes_aux)
          
          #Colocar coluna em tabela
          tabela_aux<-funcao_criar_coluna_num_EBs(tabela_original,divisoes$value)
          #Atualiza a tabela
          atualizar_ponteiro(tabela_a_trabalhar,tabela_aux)
          atualizar_ponteiro(tabela_sem_alteracoes,tabela_a_trabalhar$value)
          
          #Atualiza as colunas que se poderão alterar
          if (analise_nome=="Análise DTCs") #DTCs
          {
            colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","fim_1dia","leitura_obtida","tempo_medio"))
          }
          else if (analise_nome=="Análise EBs") #EBs
          {
            colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","fim_1dia","leitura_obtida","tempo_leit_EB"))
          }
          else #OS
          {
            colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","sucesso","tempo_OS"))
          }
          
          if (!identical(colunas_retirar, integer(0)))
          {
            atualizar_ponteiro(colunas_alterar,gsub(" ", "", colnames(summary(tabela_a_trabalhar$value))[-colunas_retirar], fixed = TRUE))
          }
          else
          {
            atualizar_ponteiro(colunas_alterar,gsub(" ", "", colnames(summary(tabela_a_trabalhar$value)), fixed = TRUE))
          }
          
          #Atualiza as colunas que se poderão testar
          if (analise_nome=="Análise EBs")
          {
            colunas_retirar<-which(colunas_alterar$value %in% c("config","marca_DTC","firmware"))
            if (!identical(colunas_retirar, integer(0)))
            {
              atualizar_ponteiro(colunas_teste,colunas_alterar$value[-colunas_retirar])
            }
            else
            {
              atualizar_ponteiro(colunas_teste,colunas_alterar$value)
            }
          }
          else
          {
            atualizar_ponteiro(colunas_teste,colunas_alterar$value)
          }
          
          #Atualiza as opções de cada uma das colunas que se poderão alterar 
          aux_lista_opcoes<-list()
          for (i in 1:length(colunas_alterar$value))
          {
            aux_lista_opcoes[[colunas_alterar$value[i]]]<-c(as.character(unique(tabela_a_trabalhar$value[[colunas_alterar$value[i]]])),"Todos")
          }
          
          #Atualiza as opções de cada uma das colunas que se poderão testar
          aux_lista_opcoes_qq<-list()
          for (i in 1:length(colunas_teste$value))
          {
            aux_lista_opcoes_qq[[colunas_teste$value[i]]]<-as.character(unique(tabela_a_trabalhar$value[[colunas_teste$value[i]]]))
          }
          
          #Atualiza todas as alterações feitas
          atualizar_ponteiro(lista_opcoes,aux_lista_opcoes)
          atualizar_ponteiro(alterar_coluna,gcombobox(colunas_alterar$value, handler=alterar_coluna_opcao))
          atualizar_ponteiro(alterar_opcao,gcombobox(lista_opcoes$value[[colunas_alterar$value[1]]]))
          atualizar_ponteiro(lista_opcoes_qq, aux_lista_opcoes_qq)
          atualizar_ponteiro(teste_coluna,gcombobox(colunas_teste$value, handler=teste_coluna_opcao))
          atualizar_ponteiro(teste_opcao, gcombobox(lista_opcoes_qq$value[[teste_coluna$value[1]]]))
          
          #Adiciona os comboboxs e botões ao programa
          add(tmpAlt,alterar_coluna$value)
          add(tmpAlt,alterar_opcao$value)
          add(tmpAlt,botao_alterar)
          
          add(tmpTestVar, teste_coluna$value)
          add(tmpTestVar, teste_opcao$value)      
          if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
          {
            add(tmpTestOpcGra,cb_dias_teste)
          }
          add(tmpTestOpcGra,cb_opcao_graficos)
          add(tmpTestGraOpc,botao_plot)
          if (analise_nome=="Análise DTCs")
          {
            add(tmpTestGraOpc,botao_big)
            add(tmpTestGraObriOpc,botao_qq)
          }
          if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
          {
            add(tmpTestTempPercT,perc_func)
            if (analise_nome=="Análise EBs")
            {
              add(tmpTestTempPercT,botao_tempo)
            }
          }
          add(tmpTestTempPercT,botao_teste)
        }
      }
    }
    
    visible(window)<-TRUE
  }
  
  #Cria frame dentro do frame "Teste" para criar novos intervalos
  tmpInt <- gframe("Definir Novos Intervalos", container=group)
  
  #Botão que permite criar novos intervalos
  botao_def_int<-gbutton("Novos Intervalos", "intervalos", handler=novos_intervalos)
  add(tmpInt, botao_def_int)
  
  ################################################## Reiniciar Tabela ########################################################
  
  #Função que apaga todas as alterações feitas, incluíndo a coluna dos intervalos (caso tenha sido adicionado intervalos)
  reiniciar_val<-function(h,...)
  {
    gconfirm("Deseja reiniciar o programa? Todas as alterações feitas serão desfeitas!", title = "Reiniciar tabela", icon = "question", handler = function(h,...)
    {
      #Apaga os comboboxs que se fizeram as alterações. Caso as opções de gráficos e testes não estiverem presentes no programa, adicionar posteriormente a realização das operações
      if (length(colunas_teste$value)!=0)
      {
        delete(tmpTestTempPercT,botao_teste)
        if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
        {
          if (analise_nome=="Análise EBs")
          {
            delete(tmpTestTempPercT,botao_tempo)
          }
          delete(tmpTestTempPercT,perc_func)
        }
        if (analise_nome=="Análise DTCs")
        {
          delete(tmpTestGraObriOpc,botao_qq)
          delete(tmpTestGraOpc,botao_big)
        }
        delete(tmpTestGraOpc,botao_plot)
        delete(tmpTestOpcGra,cb_opcao_graficos)
        if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
        {
          delete(tmpTestOpcGra,cb_dias_teste)
        }
        delete(tmpTestVar, teste_opcao$value)
        delete(tmpTestVar, teste_coluna$value)
      }
      
      delete(tmpAlt,botao_alterar)
      delete(tmpAlt,alterar_opcao$value)
      delete(tmpAlt,alterar_coluna$value)
      
      #Atualiza a tabela
      atualizar_ponteiro(tabela_a_trabalhar,tabela_original)
      atualizar_ponteiro(tabela_sem_alteracoes,tabela_a_trabalhar$value)
      
      #Atualiza as colunas que se poderão alterar
      if (analise_nome=="Análise DTCs") #DTCs
      {
        colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","fim_1dia","leitura_obtida","tempo_medio"))
      }
      else if (analise_nome=="Análise EBs") #EBs
      {
        colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","fim_1dia","leitura_obtida","tempo_leit_EB"))
      }
      else #OS
      {
        colunas_retirar<-which(names(tabela_a_trabalhar$value) %in% c("qtd_EBs_regi","sucesso","tempo_OS"))
      }
      
      if (!identical(colunas_retirar, integer(0)))
      {
        atualizar_ponteiro(colunas_alterar,gsub(" ", "", colnames(summary(tabela_a_trabalhar$value))[-colunas_retirar], fixed = TRUE))
      }
      else
      {
        atualizar_ponteiro(colunas_alterar,gsub(" ", "", colnames(summary(tabela_a_trabalhar$value)), fixed = TRUE))
      }
      
      #Atualiza as colunas que se poderão testar
      if (analise_nome=="Análise EBs")
      {
        colunas_retirar<-which(colunas_alterar$value %in% c("config","marca_DTC","firmware"))
        if (!identical(colunas_retirar, integer(0)))
        {
          atualizar_ponteiro(colunas_teste,colunas_alterar$value[-colunas_retirar])
        }
        else
        {
          atualizar_ponteiro(colunas_teste,colunas_alterar$value)
        }
      }
      else
      {
        atualizar_ponteiro(colunas_teste,colunas_alterar$value)
      }
      
      #Atualiza as opções de cada uma das colunas que se poderão alterar 
      aux_lista_opcoes<-list()
      for (i in 1:length(colunas_alterar$value))
      {
        aux_lista_opcoes[[colunas_alterar$value[i]]]<-c(as.character(unique(tabela_a_trabalhar$value[[colunas_alterar$value[i]]])),"Todos")
      }
      
      #Atualiza as opções de cada uma das colunas que se poderão testar
      aux_lista_opcoes_qq<-list()
      for (i in 1:length(colunas_teste$value))
      {
        aux_lista_opcoes_qq[[colunas_teste$value[i]]]<-as.character(unique(tabela_a_trabalhar$value[[colunas_teste$value[i]]]))
      }
      
      #Atualiza todas as alterações feitas
      atualizar_ponteiro(lista_opcoes,aux_lista_opcoes)
      atualizar_ponteiro(alterar_coluna,gcombobox(colunas_alterar$value, handler=alterar_coluna_opcao))
      atualizar_ponteiro(alterar_opcao,gcombobox(lista_opcoes$value[[colunas_alterar$value[1]]]))
      atualizar_ponteiro(lista_opcoes_qq, aux_lista_opcoes_qq)
      atualizar_ponteiro(teste_coluna,gcombobox(colunas_teste$value, handler=teste_coluna_opcao))
      atualizar_ponteiro(teste_opcao, gcombobox(lista_opcoes_qq$value[[teste_coluna$value[1]]]))
      atualizar_ponteiro(divisoes,c())
      
      #Adiciona os comboboxs e botões ao programa
      add(tmpAlt,alterar_coluna$value)
      add(tmpAlt,alterar_opcao$value)
      add(tmpAlt,botao_alterar)
      
      add(tmpTestVar, teste_coluna$value)
      add(tmpTestVar, teste_opcao$value)      
          
      if (analise_nome=="Análise EBs" | analise_nome=="Análise DTCs")
      {
        add(tmpTestOpcGra,cb_dias_teste)
      }
      add(tmpTestOpcGra,cb_opcao_graficos)
      add(tmpTestGraOpc,botao_plot)
      if (analise_nome=="Análise DTCs")
      {
        add(tmpTestGraOpc,botao_big)
        add(tmpTestGraObriOpc,botao_qq)
      }
      if (analise_nome=="Análise DTCs" | analise_nome=="Análise EBs")
      {
        add(tmpTestTempPercT,perc_func)
        if (analise_nome=="Análise EBs")
        {
          add(tmpTestTempPercT,botao_tempo)
        }
      }
      add(tmpTestTempPercT,botao_teste)
    })
  }
  
  #Cria frame dentro do frame "Teste" para reiniciar a tabela
  tmpRei <- gframe("Reiniciar Tabela", container=group)
  
  #Botão que permite reiniciar tabela
  botao_reiniciar <-gbutton("Reiniciar Tabela", "reiniciar", handler=reiniciar_val)
  add(tmpRei,botao_reiniciar)
  
  ################################################ Gravar em Ficheiro ########################################################
  
  
  if ((analise_nome=="Análise EBs" | analise_nome=="Análise OS") & !simulador)
  {
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
  }
  
  ################################################# Encerrar Programa ########################################################
  
  #Handler que avisa o utilizador que está a encerrar o programa
  addHandlerUnrealize(window, handler = function(h,...) {
    val <- gconfirm("Deseja realmente fechar o programa?", parent=h$obj)
    if(as.logical(val))
    {
      return(FALSE)
    }
    else
    {
      return(TRUE)
    }
  })
}

main<-function()
{
  #Nome do ficheiro a utilizar
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
}

main()