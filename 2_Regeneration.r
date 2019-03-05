setwd("C:\\Users\\User\\Documents\\ARTIGOS - Meus\\0 MS - Eucalipto e regenerantes\\Data")
# Regeneration environment
# Libraries:
if(!require(tidyverse)){install.packages("tidyverse")} # ggplot, tidyr, broom, dplyr
if(!require(lme4)){install.packages("lme4")}           # lmer
if(!require(statmod)){install.packages("statmod")}     # tweedie
if(!require(pscl)){install.packages("pscl")}           # zeroinfl
if(!require(lmtest)){install.packages("lmtest")}       # lrtest
if(!require(iNEXT)){install.packages("iNEXT")}         # rarefaction curves
if(!require(vegan)){install.packages("vegan")}         # diversity index

## Light Environment in the understory
# Site 1
light_site1<-read.csv("2_LAI_site1.csv",header = T,sep=",") %>% 
  dplyr::select(parcela, Tratamento, IAF_pre,IAF_pos, IAF_7ms) %>% 
  filter(Tratamento!="EEW") %>% # excluding only eucalypt sites
  gather(tempo,IAF,-c(parcela, Tratamento)) %>%
  mutate(Tempo=as.factor(ifelse(tempo=="IAF_pre","t1",ifelse(tempo=="IAF_pos", 
                                                             "t2", "t3"))),
         Trata = as.factor(ifelse(Tratamento=="NNW","Nat","Euc")))

data_mod<-filter(light_site1, Tempo!="t2")
mod_full<-lmer(IAF~Trata*Tempo+(1|parcela), data_mod)
mod_solo<-lmer(IAF~Trata+Tempo+(1|parcela), data_mod)
mod_trat<-lmer(IAF~Trata+(1|parcela), data_mod)
mod_tempo<-lmer(IAF~Tempo+(1|parcela), data_mod)
AIC(mod_full,mod_tempo,mod_trat,mod_solo)
plot(mod_full)
shapiro.test(residuals(mod_full))
summary(mod_full)

light_site1 %>%filter(Tempo=="t1") %>% with(t.test(IAF~Trata)) # Prior
light_site1 %>% filter(Tempo!="t3") %>% filter(Trata=="Euc") %>%
  with(t.test(IAF~Tempo, paired=T)) # After
light_site1 %>% filter(Tempo!="t2") %>% filter(Trata=="Euc") %>%
  with(t.test(IAF~Tempo, paired=T)) # After-Prior t-test

lsmeans::lsmeans(mod_full,"Trata")
lsmeans::lsmeans(mod_full,"Tempo")

# Line graph (time trend)
marca<-function(x){
  x<-as.character(x)
  ifelse(x=="1","Pre-logging", ifelse(x=="2","Post-logging","7-months later"))
}
Fig4a<-light_site1 %>%   group_by(Trata, Tempo) %>% 
  dplyr::summarise(IAF_min = as.numeric(mean_se(IAF)[2]),
                   IAF_med = as.numeric(mean_se(IAF)[1]),
                   IAF_max = as.numeric(mean_se(IAF)[3])) %>%
  ggplot(aes(x=as.numeric(Tempo), y=IAF_med, 
             group=Trata, color=Trata, fill = Trata)) + 
  geom_point(shape=16) + 
  geom_errorbar(aes(ymin=IAF_min, ymax=IAF_max), width=.2) +
  geom_line() +
  geom_ribbon(aes(ymin=IAF_min, ymax=IAF_max, color=NULL), alpha=0.2)+
  theme_classic()+ylab("Leaf Area Index (unitless)")+
  theme(axis.text.x = element_text( hjust = .5),
        legend.position = "bottom")+
  scale_y_continuous(limits = c(0,5),expand = c(0,0))+
  scale_x_continuous(breaks=c(1,2,3),labels = marca, name="")

# Site 3
light_site3<-read.csv("2_LAI_site3.csv",header = T,sep=",") %>% 
  dplyr::select(tratamento, parcela, ponto, RFR_pre,RFR_pos, RFR_1ano) %>%
  gather(tempo,RFR,-c(tratamento, parcela, ponto)) %>%
  mutate(Tempo=as.factor(ifelse(tempo=="RFR_pre","t1",
                                ifelse(tempo=="RFR_pos","t2","t3"))),
         Trata = as.factor(ifelse(tratamento=="NNW","Nat",
                                  ifelse(tratamento=="NEW",
                                         "Euc_Unlogged","Euc_Logged"))))
mod_data<-filter(light_site3,Tempo!="t2")
mod_full<-lmer(RFR~Trata*Tempo+(1|parcela), mod_data)
mod_trat<-lmer(RFR~Trata+(1|parcela), mod_data)
mod_tempo<-lmer(RFR~Tempo+(1|parcela), mod_data)
AIC(mod_full,mod_tempo,mod_trat)
plot(mod_full)
par(mfrow=c(1,1))
qqnorm(residuals(mod_full));qqline(residuals(mod_full))
summary(mod_full)
lsmeans::lsmeans(mod_full,"Trata")
lsmeans::lsmeans(mod_full,"Tempo")

light_site3 %>%filter(Tempo=="t1") %>% with(anova(lm(RFR~Trata))) # Prior
light_site3 %>% filter(Tempo!="t3") %>% filter(Trata=="Euc_Harvested") %>%
  with(t.test(RFR~Tempo, paired=T)) # After
light_site3 %>%filter(Tempo=="t3") %>% 
  with(anova(lm(RFR~Trata))) # After-Prior Anova

marca<-function(x){
  x<-as.character(x)
  ifelse(x=="1","Pre-logging", ifelse(x=="2","Post-logging","12-months later"))
}

Fig4b<-light_site3 %>%   group_by(Trata, Tempo) %>% 
  dplyr::summarise(RFR_min = as.numeric(mean_se(RFR)[2]),
                   RFR_med = as.numeric(mean_se(RFR)[1]),
                   RFR_max = as.numeric(mean_se(RFR)[3])) %>%
  ggplot(aes(x=as.numeric(Tempo), y=RFR_med, 
             group=Trata, color=Trata, fill = Trata)) + 
  geom_point(shape=16) + 
  geom_errorbar(aes(ymin=RFR_min, ymax=RFR_max), width=.2) +
  geom_line() +
  geom_ribbon(aes(ymin=RFR_min, ymax=RFR_max, color=NULL), alpha=0.2)+
  theme_classic()+ylab("Red:Far Red ratio (unitless)")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "bottom")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks=c(1,2,3), labels = marca, name="")

# Fig4 - Temporal variation of light environment in the understory
cowplot::plot_grid(Fig4a, Fig4b, ncol=2)

rm(list=ls())

## Invasive Grass Cover
Gram<-read.table("2_GrassCover.csv",sep=",",header=T)

# Site 1  Aracruz
gramA <- Gram %>% filter(Site=="ARA") %>% droplevels()
(tipos<-levels(gramA$treatm))
par(mfrow=c(2,2))
for (i in 1:length(tipos)){
  hist(gramA$G1[gramA$treatm==tipos[i]],
       probability = T,main=paste(tipos[i]),xlab="")
  curve(dnorm(x,mean(gramA$G1[gramA$treatm==tipos[i]]),
              sd(gramA$G1[gramA$treatm==tipos[i]])),add=T,col="red")
  curve(dgamma(x,mean(gramA$G1[gramA$treatm==tipos[i]],na.rm=T)),add=T,col="blue")
}

# ZERO-INFLATED Gamma for grass
mod.Ara<-glm(gramA$G1 ~ gramA$treatm+gramA$LAI,
             family = tweedie(var.power = 2, link.power = 0))
plot(mod.Ara)
par(mfrow=c(1,1))
summary(aov(mod.Ara))
anova(mod.Ara)

# ZIP, ZINB for LAI 
# LAI has to be multiplied by 100 because zeroinfl does not accept non-integer values
ZIP<-zeroinfl(100*G1~treatm*LAI,dist="poisson",link="logit",data=Gram)
ZINB<-zeroinfl(100*G1 ~treatm*LAI,dist="negbin",link="logit",data=Gram) 
lrtest(ZIP,ZINB) # look at LogLik, then ZINB is better
summary(ZINB) # ZINB shows no difference for LAI between treatments

## Site 3 - Igrapiuna
gramI <- Gram %>% filter(Site=="IGR") %>% droplevels()
(tipos<-levels(gramI$treatm))
par(mfrow=c(1,2))
gram0 <- gramI[gramI$G1>0,]
for (i in 1:length(tipos)){
  hist(gram0$G1,probability = T,main=paste(tipos[i]),xlab = "")
  curve(dnorm(x,mean(gram0$G1[gram0$treatm==tipos[i]]),
              sd(gram0$G1[gram0$treatm==tipos[i]])),add=T,col="red")
  curve(dgamma(x,mean(gram0$G1[gram0$treatm==tipos[i]],na.rm=T)),add=T,col="blue")
}
for (i in 1:length(tipos)){
  hist(gramI$G1,probability = T,main=paste(tipos[i]),xlab = "")
  curve(dnorm(x,mean(gramI$G1[gramI$treatm==tipos[i]]),
              sd(gramI$G1[gramI$treatm==tipos[i]])),add=T,col="red")
  curve(dgamma(x,mean(gramI$G1[gramI$treatm==tipos[i]],na.rm=T)),add=T,col="blue")
}

mod.Igr<-glm(gramI$G1~gramI$treatm*gramI$R_FR,
             family = tweedie(var.power = 2,link.power = 0))

par(mfrow=c(2,2))
plot(mod.Igr)
summary(aov(mod.Igr))
par(mfrow=c(1,1))
anova(mod.Igr)

ZIP<-zeroinfl(100*G1 ~ treatm*R_FR, dist="poisson",link="logit",data=Gram)
ZINB<-zeroinfl(100*G1 ~treatm*R_FR, dist="negbin",link="logit",data=Gram) 
lrtest(ZIP,ZINB) # look at LogLik, then ZINB is better
summary(ZINB) # ZINB shows no difference for Red-FarRed between treatments

rm(list = ls())

### Regeneration of native woody species  
## Site 1 - Logged in 2015
# Reading and preparing data
site1_pre <- read.csv('2_regen_site1_pre.csv') %>%
  filter(lifeform =="tree") %>% 
  mutate(w_Eucalypt=NED+EUC, Only_Native=NAT) 
rownames(site1_pre) <-site1_pre[,1]
site1_pre <- dplyr::select(site1_pre, Only_Native, w_Eucalypt) # Carina

site1_pos <- read.csv("2_regen_site1_pos.csv") %>%  
  mutate(Treatm=ifelse(Treatm==1,'Only_Native','w_Eucalypt')) %>%
  filter(lifeform=="Tree") %>% mutate(small=ifelse(is.na(small), 0, small),
                                      big=ifelse(is.na(big), 0, big),
                                      Dens = small+big) %>%
  filter(lifeform=="Tree") %>%
  group_by(Treatm, Fieldname) %>% summarise(Freq = sum(Dens)) %>%
  spread(Treatm, Freq) %>% as.data.frame() %>%
  mutate(Only_Native = ifelse(is.na(Only_Native),0,Only_Native),
         w_Eucalypt = ifelse(is.na(w_Eucalypt),0,w_Eucalypt))

rownames(site1_pos) <- site1_pos[,1]
site1_pos <- dplyr::select(site1_pos, Only_Native, w_Eucalypt)
  

## Site 3 - Igrapiuna
site3_pre <- read.csv('2_regen_site3_pre.csv')
rownames(site3_pre)<-site3_pre[,1]
site3_pre <- site3_pre %>% dplyr::select(NAT1, CCC1, CSC1) # 1 mai 2015 - Carina = 1: 46

site3_pos <- read.csv('2_regen_site3_pos.csv')
rownames(site3_pos)<-site3_pos$fieldname
site3_pos <- site3_pos %>% dplyr::select(NAT4,CCC4,CSC4) %>%  # 4 mai 2018 - Taisi = 38 meses: 83
  mutate(NAT4=ifelse(is.na(NAT4),0,NAT4),
         CCC4=ifelse(is.na(CCC4),0,CCC4),
         CSC4=ifelse(is.na(CSC4),0,CSC4))

## Abundance
# Site 1 - pre
dens1_pre <- read.csv("2_regen_site1_pre_plot.csv") %>% 
  mutate(Site="Site1",Period="Before")

# Site 1 - pos 
dens1_pos <- read.csv("2_regen_site1_pos.csv") %>%  
  mutate(Treatm=ifelse(Treatm==1,'only_native','w_eucalypt')) %>%
  filter(lifeform=="Tree") %>% mutate(small=ifelse(is.na(small), 0, small),
                                      big=ifelse(is.na(big), 0, big),
                                      Dens = small+big) %>%
  group_by(Treatm,Plot) %>% summarise(Dens=sum(Dens))%>% 
  mutate(Site="Site1",Period="After")

# Site 3 - pre - OK
dens3_pre<- read.csv("2_regen_site3_pre_full.csv") %>%
  filter(lifeform=="tree") %>% mutate(small=ifelse(is.na(small), 0, small),
                                      big=ifelse(is.na(big), 0, big),
                                      Dens = small+big) %>% 
  group_by(Modelo, ID_Parc) %>% summarise(Dens=sum(Dens))%>% 
  mutate(Site="Site3",Period="Before")
levels(dens3_pre$Modelo)<-c('1_Native', "2_Mixed_Logged", "3_Mixed_Unlogged")
# Site 3 - pos
dens3_pos <- read.csv("2_regen_site3_pos_full.csv") %>%
  filter(lifeform=="tree") %>% 
  mutate(small=ifelse(is.na(small), 0, small),
         big=ifelse(is.na(big), 0, big),
         Dens = (small+big),
         Modelo = ifelse(Modelo==1,"1_Native",ifelse(Modelo==2,"2_Mixed_Logged",
                                                "3_Mixed_Unlogged"))) %>% 
  group_by(Modelo, ID_parc) %>% summarise(Dens=sum(Dens))%>%
  mutate(Site="Site3",Period="After")

# Generalized Linear Model
colnames(dens1_pre)<-colnames(dens1_pos)<-
  colnames(dens3_pre)<-colnames(dens3_pos)<-
  c("Treatm","Plot","Dens","Site","Period")
dat<-bind_rows(dens1_pre, dens1_pos, dens3_pre,dens3_pos) %>%
  mutate(Treatm=factor(Treatm), 
         Period=ordered(Period,levels=c("Before", "After")))

# Site 1
mod0<-dat %>% filter(Site=="Site1") %>%
  mutate(Period=as.numeric(Period)) %>% droplevels() %>%
  with(glmer(Dens~(1|Plot), family=poisson))

mod1<-dat %>% filter(Site=="Site1") %>%
  mutate(Period=as.numeric(Period)) %>% droplevels() %>%
  with(glmer(Dens~Treatm+(1|Plot), family=poisson))

mod2<-dat %>% filter(Site=="Site1")%>%
  mutate(Period=as.numeric(Period)) %>% droplevels() %>%
  with(glmer(Dens~Period+(1|Plot), family=poisson))

mod_site1<-dat%>% filter(Site=="Site1")%>%
  mutate(Period=as.numeric(Period)) %>% droplevels() %>%
  with(glmer(Dens~Treatm*Period+(1|Plot), family=poisson))

anova(mod0,mod1,mod2, mod_site1)
plot(mod_site1)

# Site 3
mod0<-dat %>% filter(Site=="Site3")%>% droplevels() %>%
  mutate(Period=as.numeric(Period))%>% 
  with(glmer(Dens~(1|Plot), family=poisson))

mod1<-dat %>% filter(Site=="Site3")%>% droplevels() %>%
  mutate(Period=as.numeric(Period))%>% 
  with(glmer(Dens~Treatm+(1|Plot), family=poisson))

mod2<-dat %>% filter(Site=="Site3")%>% droplevels() %>%
  mutate(Period=as.numeric(Period))%>% 
  with(glmer(Dens~Period+(1|Plot), family=poisson))

mod_site3<-dat %>% filter(Site=="Site3")%>% droplevels() %>%
  mutate(Period=as.numeric(Period))%>% 
  with(glmer(Dens~Treatm*Period+(1|Plot), family=poisson))

anova(mod0,mod1,mod2, mod_site3)


# Summary
summary(mod_site1)
summary(mod_site3)

(post_hoc<-lsmeans::lstrends(mod_site1, "Treatm", var="Period"))
pairs(post_hoc)
(post_hoc<-lsmeans::lstrends(mod_site3, "Treatm", var="Period"))
pairs(post_hoc)

lsmeans::lsmeans(mod_site1, "Treatm")
lsmeans::lsmeans(mod_site3, "Treatm")


# Visualize
dat %>% group_by(Site, Treatm,Period) %>% 
  summarise(media = mean_cl_boot(Dens)[[1]],
            media_min = mean_cl_boot(Dens)[[2]],
            media_max = mean_cl_boot(Dens)[[3]]) 
  
ggplot(dat,aes(x=Period, y=Dens, group=Treatm)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = Treatm, color = Treatm)) +
  stat_summary(geom = "line", fun.y = mean, aes(color=Treatm)) +
  facet_grid(Site~.)+theme_bw()

## Rarefaction curves
raref_plot<-function(df){
  df<-iNEXT(df,datatype="abundance") %>% fortify(type=1) %>% 
    filter(method!="extrapolated")
  ggplot(df, aes(x=x, y=y, color=site)) + theme_bw()+
    geom_point(shape=16, data=df[which(df$method=="observed"),]) + 
    geom_line(data=df[which(df$method!="observed"),]) +
    geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,fill=site, color=NULL), alpha=0.2)+
    ggtitle(" ")+theme(legend.position = "top")+
    xlab("Number of Individuals")+ ylab("Species Richness")+
    guides(color= F, shape = F, fill=guide_legend(override.aes = list(alpha=1), title = ""))
}
(p1<-raref_plot(site1_pre))
(p2<-raref_plot(site1_pos))

(p3<-raref_plot(site3_pre))
(p4<-raref_plot(site3_pos))

cowplot::plot_grid(p1,p2,p3,p4, 
          labels = c("Site 1 - Pre","Site 1 - Pos","Site 3 - Pre","Site 3 - Pos"))


# Rarefied species richness
rarefaz<-function(x){
  n<-min(colSums(x))
  resulta<-t(as.data.frame(rarefy(x, n, se=T, MARGIN = 2)))
  return(data.frame(resulta, n))
}

data.frame(rbind(rarefaz(site1_pre), rarefaz(site1_pos)), 
                      tempo = c(1,1,38,38),
                      trata = c('only_native', "eucalypts")) %>%
  ggplot(aes(x=tempo, y=S, group=trata)) +
  geom_line(aes(color=trata)) + 
  geom_errorbar(aes(ymin=S-se, ymax=S+se, color=trata), width=.1) +
  geom_point(size=2, aes(color=trata), shape=16)+theme_classic() +
  ggtitle("Site 1") + xlab('Sampled time (months)') +
  ylab('Rarefied Species Richness')

data.frame(rbind(rarefaz(site3_pre), rarefaz(site3_pos)), 
                      tempo = c(1,1,1,38,38,38),
                      trata = c('1_Native', "2_Mixed_Logged", "3_Mixed_Unlogged"))%>%
  ggplot(aes(x=tempo, y=S, group=trata)) +
  geom_line(aes(color=trata)) +
  geom_errorbar(aes(ymin=S-se, ymax=S+se, color=trata), width=.1) +
  geom_point(size=2, aes(color=trata), shape=16)+theme_classic() +
  ggtitle("Site 3")+xlab('Sampled time (months)') +
  ylab('Rarefied Species Richness')

# Species composition similarity
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

vegdist(t(site1_pos), method='jaccard')
vegdist(t(site3_pos), method='jaccard')
rbind(diversidade(site1_pre),diversidade(site1_pos))
rbind(diversidade(site3_pre),diversidade(site3_pos))

## END