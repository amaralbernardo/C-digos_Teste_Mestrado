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
