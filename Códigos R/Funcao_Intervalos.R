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
