######## OCT
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

# Colheita em abril 2015 (IGR):
dados <- as.data.frame(read_xlsx('Regenera_Especies_Carina_dinamica.xlsx'))
dado <- read_xlsx('Regenerantes_OCT_Igrapiuna.xlsx', sheet='taisi') 
rownames(dados)<-dados[,1]
tempo1 <- dados %>% select(NAT1, CCC1, CSC1) # 1 mai 2015 - Carina = 1: 46
tempo2 <- dados %>% select(NAT2, CCC2, CSC2) # 2 dez 2015 - Carina = 8: 53
tempo3 <- dados %>% select(NAT3, CCC3, CSC3) # 3 jul 2016 - Carina = 15: 60
tempo4 <- dado %>% with(table(Modelo,Coleta)) %>% t()
colnames(tempo4) <- c("NAT4","CCC4","CSC4")  # 4 mai 2018 - Taisi = 38 meses: 83
tempo4 <- as.data.frame(tempo4) %>% spread(Modelo, Freq)
rownames(tempo4) <- tempo4[,1]
tempo4 <- tempo4 %>% select(NAT4, CCC4, CSC4)

iNEXT(tempo1,datatype="abundance") %>% plot()
iNEXT(tempo2,datatype="abundance") %>% plot()
iNEXT(tempo3,datatype="abundance") %>% plot()
iNEXT(tempo4,datatype="abundance") %>% plot()

# 1. Rarefied species richness
rarefaz<-function(x){
  n<-min(colSums(x))
  resulta<-t(as.data.frame(rarefy(x, n, se=T, MARGIN = 2)))
  return(data.frame(resulta, n))
}
resulta<-rbind(rarefaz(tempo1), rarefaz(tempo2),
               rarefaz(tempo3), rarefaz(tempo4))
# write.csv(data.frame(resulta,N=c(colSums(tempo1),colSums(tempo2),colSums(tempo3),colSums(tempo4))),"tab_diversidade.csv")

resulta <- data.frame(resulta, 
                      tempo = c(1,1,1,8,8,8,15,15,15,38,38,38),
                      trata = c('native', "harvested", "no_harvested"))
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
  N<-colSums(t(x))
  return(data.frame(H,simp,invsimp,unbias.simp,alpha,S,J,N))
}
# write.csv
rbind(diversidade(tempo1),diversidade(tempo2), diversidade(tempo3),diversidade(tempo4))
# , 'tab_diversidade.csv')


#### Density
## Tempo 1,2,3
sp_dens <- read_xlsx('Regeneracao_brutos_IGR.xlsx', sheet='Colheita') %>%
  dplyr::select(Data_Coleta,Trata, ID_Parc, N_reg) %>%
  group_by(Data_Coleta, Trata, ID_Parc) %>% 
  dplyr::summarise(N_reg=sum(N_reg)) %>% ungroup() %>% spread(Data_Coleta,N_reg) 
colnames(sp_dens)<-c('Modelo','ID_parc','time1','time2','time3')
sp_dens$Modelo<-factor(sp_dens$Modelo, levels=c("NAT","NEColheita","NE_Sem"), ordered=TRUE)

## Tempo 4
sp_dens4 <- dado %>%  group_by(Modelo, ID_parc) %>% 
  dplyr::select(Modelo, ID_parc, N_ind) %>%
  dplyr::summarise(time4=sum(N_ind, na.rm = T)) %>% ungroup()%>% mutate(Modelo=as.ordered(Modelo))
levels(sp_dens4$Modelo) <-c("NAT","NEColheita","NE_Sem")

din_dens <- inner_join(sp_dens,sp_dens4) %>% 
  gather(key=time, value = N_reg, time1:time4)

din_dens<- din_dens %>% 
  mutate(time=factor(time),
         months=c(rep(1,102),rep(8,102),rep(15,102),rep(38,102)),
         ID_parc=factor(ID_parc),
         Modelo=factor(unclass(din_dens$Modelo)))
levels(din_dens$Modelo)<-c("Only Natives","Harvested Euc","Non_Harvested Euc")
din_dens$N_reg[is.na(din_dens$N_reg)]<-0

# Generalized Additive Mixed-Model
mod0 <- uGamm(N_reg ~ s(months, k=4),
              random = ~ (1|ID_parc), family = poisson, REML=T,
              data = din_dens, lme4=TRUE)

mod1 <- uGamm(N_reg ~ s(months, by = Modelo, k=4),
              random = ~ (1|ID_parc), family = poisson, REML=T,
              data=din_dens, lme4=TRUE)

mod2 <- uGamm(N_reg ~ s(months, by = Modelo, k=4) + Modelo, # tem intera??o mais o Tratamento explicando (+Tratamento)
              random = ~ (1|ID_parc), family = poisson, REML=T,
              data = din_dens, lme4=TRUE)

model.sel(mod0, mod1, mod2)
summary(mod2$mer)
summary(mod2$gam)
plot(mod2$mer)
par(mfrow=c(2,2))
plot(mod2$gam)
vis.gam(mod2$gam)
gam.check(mod2$gam)
dev.off()
anova(mod2$gam)
summary(mod2$gam)


# Visualize model fit
newbie <- expand.grid(months = seq(min(din_dens$months), max(din_dens$months), 
                                 length.out=100), 
                      Modelo=levels(din_dens$Modelo))
predi <- predict.gam(object = mod2$gam,  newdata = newbie, 
                     se.fit = TRUE, type="response")
predi <-cbind(newbie, as.data.frame.list(predi))

ggplot(data = din_dens, mapping = aes(x = months, y=N_reg, group=Modelo))+ 
  #geom_point(aes(col=ID_parc)) +
  #geom_point(size=0.5) +
  theme_bw()+ theme(legend.position="none")+
  #geom_smooth(se = TRUE, method="loess") +
  facet_grid( Modelo~.) +
  geom_line(data = predi, mapping = aes(x = months, y=fit), col="blue") +
  geom_line(data = predi, mapping = aes(x = months, y=fit + 1.96*se.fit), 
            col="blue", linetype = 2) +
  geom_line(data = predi, mapping = aes(x = months, y=fit - 1.96*se.fit), 
            col="blue", linetype = 2) +
  labs(x ="Months after harvesting", y = "Number of Regenerants/plot")
ggsave("gamm_resulta_OCT.jpg")

# https://www.fromthebottomoftheheap.net/2017/10/10/difference-splines-i/

