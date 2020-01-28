################################################################
###########    Script para a criação de mapas   ###############
###  Votações por partido e cargo para os municípios do RJ  ###
###############################################################

setwd("~/seudiretorio")

library(pacman)
p_load(tidyverse, geobr, purrr)

source("funcoes.R")

#trazendo os bancos elaborados e salvos no script "coleta_dados.R"
list_bc<-list.files(pattern = "el")
rj_list=lapply(list_bc, read.csv)

#dando nome aos bancos da lista. Darei o mesmo nome dos documentos 
list_bc<-gsub(".csv", "", list_bc)
names(rj_list)<-list_bc

# Agora preciso criar os bancos para a elaboração dos mapas. Os mapas apresentarão
# a porcentagem de votação recebida por cada partido nos municípios do estado do 
# Rio de Janeiro. Para isso o banco deve estar organizado de forma que os municípios
# estejam nas linhas e as colunas sejam a porcentagem de votação recebida por cada
# partido nos municípios. Desta forma é preciso que o banco esteja em formato
# alongado (spread). Para isso vou utilizar as funções que criei no script "funcoes.R"
# e que trouxe para este ambiente pelo source. Explicações de como utilizar as funções
# e quais argumentos devem ser inseridos ler explicações no script "funcoes".
# importante lembrar que o script só irá rodar se funcoes.R e este script (mapas.R)
# estiverem apontando para o mesmo diretório (setwd(/seu diretório))

# CRIANDO BANCOS:

#Deputado Federal
depfed<-lapply(rj_list, function(rj_list) sp_leg(rj_list,"deputado federal"))

#coletando as siglas dos partidos por ano. Cada posição da lista será um vetor com 
#as siglas dos partidos para cada eleição e cargo. 
#Precisarei desses vetores como argumentos da 
#função para a criação automatizadas dos mapas
part_depfed<-pega_sigla(depfed)


#Deputado Estadual
depest<-lapply(rj_list, function(x) sp_leg(x,"deputado estadual"))
part_depest<-pega_sigla(depest)

#Senador
sen<-lapply(rj_list, function(rj_list) sp_leg(rj_list,"senador"))
part_sen<-pega_sigla(sen)

## PLOTANDO MAPAS

#usarei o shape do estado do RJ do pacote geobr
#baixando e limpando o shape file que será usado
rj_sh<- read_municipality(code_muni ="RJ", year= 2000)

rj_sh<- rj_sh%>%
  filter(abbrev_state=="RJ")

#trazendo csv com compatilibilidade dos códigos do IBGE e do TSE e filtrando
#apenas RJ
compat<- read.csv("comp_mun.csv")

compat_RJ<- compat %>%
  filter(uf=="RJ")

#juntando shape com o banco de compatibilidade

rj_sh<-left_join(rj_sh, compat_RJ, by=c("code_muni"="cod_municipio"))

#reorganizando o shape apenas com as variáveis de interesse
rj_sh<-rj_sh[, c(1,2,3,4,7,8)]


#juntando o banco de dados e o shape usando a função junta_shape 
#criada no script funcoes.R

#juntando o shape com os bancos de deputado federal
df_sh<-junta_shape(depfed,rj_sh)

#deputado estadual
dest_sh<-junta_shape(depest, rj_sh)

#senador
sen_sh<-junta_shape(sen,rj_sh)



###PAREI AQUI: AINDA PRECISO DESENVOLVER A AUTOMATIZACAO DA CRIACAO DOS MAPAS
###O SCRIPT A PARTIR DAQUI AINDA ESTA EM DESENVOLVIMENTO


anos<-c("2002", "2006", "2010", "2014", "2018","1998")

###Código para um ano/cargo/partido
ggplot()+
  geom_sf(data=sh_02, aes(fill = cut_number(PT, 5))) + 
  geom_polygon(color="dark gray", size=0.08) +   
  scale_fill_brewer(palette="Oranges", name= " Votos (%)", direction = - 1)+
  labs(title = "PT-Deputado Federal,2002", 
       caption="Fonte: TSE e IBGE. Elaboração: Natalia Block")

###
plot_maps<-function(x){
  geom_sf(sh_02, aes(fill=cut_number(x,5)))+
    geom_polygon(color="dark gray", size=0.08)+
    scale_fill_brewer(palette="Oranges", name= " Votos (%)", direction = - 1)
}





                