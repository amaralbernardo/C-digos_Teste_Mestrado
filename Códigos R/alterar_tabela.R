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
