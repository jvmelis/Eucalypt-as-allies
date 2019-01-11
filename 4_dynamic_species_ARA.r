######## ARA
wd<-"~/ARTIGOS - Meus/0 MS - Eucalipto e regenerantes"
setwd(wd)
rm(list=ls())

if(!require(tidyverse)){install.packages("tidyverse")}  
if(!require(readxl)){install.packages("readxl")}       # read_excel
if(!require(iNEXT)){install.packages("iNEXT")}         # rarefaction curves
if(!require(vegan)){install.packages("vegan")}         # diversity index
if(!require(mgcv)){install.packages('mgcv')}
if(!require(gamm4)){install.packages('gamm4')}
if(!require(MuMIn)){install.packages('MuMIn')}

# Colheita em 2015
### ARACRUZ
ara_2015 <- as.data.frame(read_xlsx('Regenera_Especies_Carina3.xlsx' , sheet='ARA')) %>%
  filter(`forma vida`=="Árvore"|`forma vida`=="Arbusto")
rownames(ara_2015) <-ara_2015[,1]
tempo1 <- ara_2015 %>% mutate(w_Eucalypt=NED+EUC, Only_Native=NAT) %>%
  dplyr::select(Only_Native, w_Eucalypt) # ? 2015 - Carina

# Exclude: "Arbusto indeterminado", "Asteraceae", "Lipia sp."
ara_2018 <- read_xlsx("Regenera_Aracruz_taisi2018.xlsx", sheet="Plan1") %>%
  mutate(Modelo=ifelse(Modelo==1,'Only_Native','w_Eucalypt'))
ara_2018<-ara_2018 %>% with(table(Modelo,Coleta)) %>% t()


tempo2 <- as.data.frame(ara_2018) %>% spread(Modelo, Freq)
rownames(tempo2) <- tempo2[,1]
tempo2 <- tempo2 %>%dplyr::select(Only_Native, w_Eucalypt)


iNEXT(tempo1,datatype="abundance") %>% plot()
iNEXT(tempo2,datatype="abundance") %>% plot()

# 1. Rarefied species richness
rarefaz<-function(x){
  n<-min(colSums(x))
  resulta<-t(as.data.frame(rarefy(x, n, se=T, MARGIN = 2)))
  return(data.frame(resulta, n))
}
resulta<-rbind(rarefaz(tempo1), rarefaz(tempo2))
# write.csv(data.frame(resulta,N=c(colSums(tempo1),colSums(tempo2),colSums(tempo3),colSums(tempo4))),"tab_diversidade.csv")

resulta <- data.frame(resulta, 
                      tempo = c(1,1,38,38),
                      trata = c('only_native', "eucalypts"))
ggplot(resulta, aes(x=tempo, y=S, group=trata)) +
  geom_line(aes(color=trata)) +
  geom_errorbar(aes(ymin=S-se, ymax=S+se, color=trata), width=.1) +
  geom_point(size=2, aes(color=trata), shape=16)+theme_classic() +
  ggtitle("Site 3")+xlab('Sampled time (months)') +
  ylab('Rarefied Species Richness')
ggsave("OCT_rarefied_dynamic.jpg")

### Species composition similarity
# tempo1, tempo2,tempo3, tempo4

diversidade<-function(x){
  x<-t(x)
  H <- diversity(x)
  simp <- diversity(x, "simpson")
  invsimp <- diversity(x, "inv")
  unbias.simp <- rarefy(x, 2) - 1 ## Unbiased Simpson (Hurlbert 1971, eq. 5) with rarefy:
  alpha <- fisher.alpha(x) ## Fisher alpha
  S <- specnumber(x) ## Species richness (S)
  J <- H/log(S)      ## and Pielou's evenness (J)
  N=colSums(t(x))
  return(data.frame(N,H,simp,invsimp,unbias.simp,alpha,S,J))
}
# write.csv(
rbind(diversidade(tempo1),diversidade(tempo2))
# , 'tab_diversidade.csv')


