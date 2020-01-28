####################################################################
######         Coleta de dados/ limpeza de bancos             ######
#####  Votações por partido e cargo para os municípios do RJ  ######
####################################################################

#Apontando diretório
setwd("~/seudiretorio")

#Vou usar o pacote electionsBR para fazer o download dos dados. 
#Neste script eu trato os dados para governador, senador, deputado federal e 
#estadual. No entanto farei os mapas apenas para os cargos legislativos

############  DADOS DE VOTACAO  ###############
library(pacman)
p_load(electionsBR, rlist, tidyverse)

#vetor com os anos que farei download (de 1998 a 2018)
anos<-c(1998,2002,2006,2010,2014,2018)

# baixando os bancos pelo electionsBR usando o lapply e salvando em lista
rj_list<-lapply(anos, function(anos) party_mun_zone_fed(anos, uf="RJ"))

#colocando nomes nos dataframes dentro da lista
names(rj_list)<-c('el98', 'el02', 'el06', 'el10', 'el14','el18')

#dando uma olhada nas dimensões dos bancos
map(rj_list, dim)

#tem uma coluna a mais em 2014; em 2018 tem 7 a mais. 
#Em 2014 tem uma coluna chamada TRANSITO que não estava nos bancos anteriores. 
#Vou retirar:

rj_list[[5]]$TRANSITO<-NULL

#Vou selecionar no banco de eleições 2018 apenas as colunas
#que são iguais as de 2014

#salvando o nome das colunas de el14 em um vetor
#e slicing em el18 apenas com as colunas que são iguais as de el14
col14<-names(rj_list$el14)

#Slicing
rj_list$el18<-rj_list$el18[ , which (names(rj_list$el18) %in% col14)]

#colocando el18 na mesma ordem que el14  
rj_list$el18<-rj_list$el18[, col14]

#verificando se as colunas de el14 e el18 são iguais e estão na mesma ordem
names(rj_list$el18)==names(rj_list$el14)

#Retirando presidente dos bancos: 

#Loop para colocar as descrições dos cargos em letras minúsculas
for(i in seq_along(rj_list)){
   rj_list[[i]]$DESCRICAO_CARGO<-tolower(rj_list[[i]]$DESCRICAO_CARGO)
}

#retirando presidente
for(i in seq_along(rj_list)){
rj_list[[i]]<-(rj_list[[i]][!(rj_list[[i]]$DESCRICAO_CARGO == "presidente"), ])
}

#Destes bancos ainda é preciso:
#1 - agrupar os votos nos municípios e turno por partido
#2 - somar votos nominais e de legenda para ter a votação total
#3 - selecionar apenas as variáveis de interesse que são:
# ANO_ELEICAO, NUM_TURNO, SIGLA_UF, CODIGO_MUNICIPIO, NOME_MUNICIPIO,
# DESCRICAO_CARGO, SIGLA_PARTIDO, NOME_PARTIDO, QTE_VOTOS_NOMINAIS
# QTDE_VOTOS-LEGENDA
#

#Fazendo isso tudo em um loop:

for (i in seq_along(rj_list)){
rj_list[[i]]<- rj_list[[i]] %>%
  select(ANO_ELEICAO, NUM_TURNO,CODIGO_MUNICIPIO, NOME_MUNICIPIO,
         DESCRICAO_CARGO, SIGLA_PARTIDO, NOME_PARTIDO, QTDE_VOTOS_NOMINAIS,
         QTDE_VOTOS_LEGENDA) %>%
  group_by(ANO_ELEICAO, NUM_TURNO, CODIGO_MUNICIPIO, NOME_MUNICIPIO, DESCRICAO_CARGO,
           SIGLA_PARTIDO, NOME_PARTIDO) %>%
  summarise_at(vars(QTDE_VOTOS_NOMINAIS, QTDE_VOTOS_LEGENDA), sum)%>%
  mutate(votos = QTDE_VOTOS_NOMINAIS + QTDE_VOTOS_LEGENDA)
}


#Verificando os cargos presentes em cada banco
#vou criar uma função para agrupar os cargos e vou aplicá-la n lista
#com "map" do purr
check<- function(ano) {
  rj_list[[i]] %>%
  select(DESCRICAO_CARGO)%>%
  group_by(DESCRICAO_CARGO)%>%
  count()
}

map(rj_list, check)

############################################################
##Preparando os dados de COMPARECIMENTO (detalhes de votação)

##fazendo download via electionsBR e criando uma lista os com bancos
comp_list<-lapply(anos, function(anos) details_mun_zone_fed(anos, uf="RJ"))

#colocando nomes dos dataframes na lista
names(comp_list)<-c('comp98', 'comp02', 'comp06', 'comp10', 'comp14','comp18')

#dando uma olhada nas dimensões dos bancos
map(comp_list, dim)

##os bancos não estão exatamente iguais, mas não irei precisar de todas as 
#variáveis. Assim, vou selecionar apenas as que preciso e que estão em todos os
#bancos. São elas:
#ANO_ELEICAO, NUM_TURNO, CODIGO_MUNICIPIO, NOME_MUNICIPIO,
#DESCRICAO_CARGO, QTD_COMPARECIMENTO

for (i in seq_along(comp_list)){
  comp_list[[i]]<- comp_list[[i]] %>%
  select(ANO_ELEICAO, NUM_TURNO, DESCRICAO_CARGO,
         CODIGO_MUNICIPIO,NOME_MUNICIPIO, QTD_COMPARECIMENTO)%>%
  group_by(NUM_TURNO, DESCRICAO_CARGO, CODIGO_MUNICIPIO, NOME_MUNICIPIO)%>%
  summarise(COMPARECIMENTO=sum(QTD_COMPARECIMENTO))
}

#Colocando as descrições dos cargos em letras minúsculas
for(i in seq_along(comp_list)){
  comp_list[[i]]$DESCRICAO_CARGO<-tolower(comp_list[[i]]$DESCRICAO_CARGO)
}

#fazendo um loop para juntar banco de votação com o de comparecimento
#e calcular as porcentagens de votação

for (i in seq_along(rj_list)){ 
  rj_list[[i]]<-rj_list[[i]]%>%
    left_join(comp_list[[i]])%>%
    mutate(p_voto=(round(votos*100/COMPARECIMENTO, 2)))
} 


# fazendo back up. Salvarei 6 bancos em painel como csv

for(i in names(rj_list)){
  write.csv(rj_list[[i]], paste0(i,".csv"))
}



