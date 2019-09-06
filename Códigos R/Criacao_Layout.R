window <- gwindow(titulo) #Janela com o título
gstatusbar("Autor: Bernardo Amaral", cont=window) #Autor
getToolkitWidget(window)$maximize() #Maximiza a janela
BigGroup <- ggroup(cont=window)
group <- ggroup(horizontal=FALSE, container=BigGroup)

#Adiciona um gráfico na janela
valor_janela<-novo_ponteiro(ggraphics())
add(BigGroup,valor_janela$value)
