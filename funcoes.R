####################################################################
#####  Funções para a automatização da elaboração dos bancos  ######
#####  Votações por partido e cargo para os municípios do RJ  ######
####################################################################

setwd("~/seudiretorio")

library(pacman)
p_load(tidyverse, geobr)

#FUNÇOES PARA A PREPARACAO DOS BANCOS:

#sp_leg: função para criar bancos em perfil alongado. Transformação necessária para elaboração dos mapas
#Os argumentos da função são o nome do banco e o cargo. O nome do cargo deve ser escrito
#entre aspas

sp_leg<-function(banco,cargo){
  bc<-banco%>%
    select(ANO_ELEICAO,DESCRICAO_CARGO,NUM_TURNO, CODIGO_MUNICIPIO, NOME_MUNICIPIO, 
           SIGLA_PARTIDO, p_voto)%>%
    filter (DESCRICAO_CARGO==(!!cargo)) %>%
    spread(SIGLA_PARTIDO, p_voto)
}


##FUNCOES PARA A CRIACAO DOS MAPAS

#junta_shape: junta todos os bancos de uma lista a uma shape já estruturada
junta_shape<-function(lista, shape){
  lapply(lista, function(lista) left_join(shape, lista,by=c("cod_municipio_tse"="CODIGO_MUNICIPIO")))
}
  
#pega_sigla: coleta os nomes das colunas que indicam os partidos. Os vetores
#com os nomes dos partidos serão utilizados para automatizar a criação dos mapas
pega_sigla<-function(lista){
sigla<-lapply(lista, function(x) names(x[6:length(x)]))
}

