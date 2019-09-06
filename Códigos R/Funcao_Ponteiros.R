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
