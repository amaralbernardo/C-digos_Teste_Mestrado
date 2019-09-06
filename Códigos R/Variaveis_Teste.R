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
