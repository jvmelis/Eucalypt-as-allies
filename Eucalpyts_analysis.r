wd<-"~/ARTIGOS - Meus/0 MS - Eucalipto e regenerantes"
setwd(wd)
rm(list=ls())

# Libraries:
if(!require(tidyverse)){install.packages("tidyverse")} # ggplot, tidyr, broom, dplyr...
if(!require(readxl)){install.packages("readxl")}       # read_excel

################### Aboveground Biomass ###################################
# Reading data
biomass <- read_excel("Biomassa_3_sites.xlsx",sheet="Planilha6",na="") 

#  Comparing Site 1 - Total Biomass and Non-pioneer tree Biomass 
ara <- biomass  %>% mutate(local=paste(site, treatment,sep="_") ) %>%
  filter(site=='ara') %>% filter(treatment!="EUC")%>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>% 
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC)
ara %>% with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"]))  # site 1
ara %>%    with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))    # NS
ggplot(ara, aes(x=SEC,group=treatment,fill=treatment))+geom_density()+scale_x_continuous(limits=c(2,7))+ggtitle("Aracruz - Site 1")

#  Comparing Site 2 - Total Biomass and Non-pioneer tree Biomass 
muc <- biomass  %>% mutate(local=paste(site,treatment,sep="_") ) %>%
  filter(site=='muc') %>% filter(treatment!="EUC")%>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>%
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC)
muc %>% with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"]))  # p=0.002
muc %>% with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))    # p=0.01 
muc %>% ggplot(aes(x=SEC, group=treatment, fill=treatment))+geom_density()+scale_x_continuous(limits=c(2,7))+ggtitle("Mucuri - Site 2")

#  Comparing Site 3 - Total Biomass and Non-pioneer tree Biomass 
igr <- biomass  %>% mutate(local=paste(site,treatment,sep="_") ) %>%
  filter(site=='oct') %>% filter(treatment!="EUC") %>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>%
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC)
igr %>% with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"])) # site 3
igr %>% with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))
igr %>% ggplot(aes(x=SEC,group=treatment,fill=treatment))+geom_density()+scale_x_continuous(limits=c(0,12))+ggtitle("Site 3 - Igrapiuna")

## Figure
df <- biomass  %>% dplyr::select(site, sppbiomass, treatment, Mg_ha) %>% 
  filter(treatment!="EUC")  %>% group_by(site,treatment, sppbiomass)%>% 
  summarise(media=mean(Mg_ha, na.rm=T),
            ymax= as.numeric(paste(mean_se(Mg_ha)[3])),
            ymin= as.numeric(paste(mean_se(Mg_ha)[2]))) %>% 
  ungroup()%>% mutate(local=paste(site,treatment,sep="_")) %>% mutate(local=factor(local))

df$ymax[df$sppbiomass=='EUCALYPTUS']<-df$ymax[df$sppbiomass=='EUCALYPTUS']+df$ymax[df$sppbiomass=='SEC'&df$treatment!='NNW']
df$ymin[df$sppbiomass=='EUCALYPTUS']<-df$ymin[df$sppbiomass=='EUCALYPTUS']+df$ymin[df$sppbiomass=='SEC'&df$treatment!='NNW']
df$ymax[df$sppbiomass=='PIONEER']<-df$ymax[df$sppbiomass=='PIONEER']+df$ymax[df$sppbiomass=='SEC'&df$treatment=='NNW']
df$ymin[df$sppbiomass=='PIONEER']<-df$ymin[df$sppbiomass=='PIONEER']+df$ymin[df$sppbiomass=='SEC'&df$treatment=='NNW']

## Figure
ggplot(df, aes(x=local, y=media, fill=sppbiomass)) + 
  geom_bar(stat="identity",size=.5,width = 0.7, color="black") + 
  geom_errorbar(width=.3,size=.5, aes(ymin=(ymin), ymax=(ymax)))+
  scale_x_discrete(labels=c(
    "ara_NEW"="EUC\nN-PIO","ara_NNW"="PIO\nN-PIO",     # NS
    "muc_NEW"="EUC\nN-PIO","muc_NNW"="PIO\nN-PIO",     # p=0.01
    "oct_NEW"="EUC\nN-PIO", "oct_NNW"="PIO\nN-PIO"))+  # p=0.004
  scale_fill_manual(values =c("slateblue","coral2","springgreen3"), 
                    labels = c("Eucalpyt", "Pioneer tree", "Non-pioneer tree"))+
  xlab("")+ ylab(expression(paste("Aboveground biomass (Mg ",ha^-1,")")))+
  scale_y_continuous(breaks=c(0,20,40,70,120,170),
                     labels=c(0,20,50,100,150,200),
                     expand = c(0,0),limits=c(0,180))+
  theme_classic()+ theme(legend.justification=c(0.5, 0),axis.title.y = 
                           element_text(margin = 
                                      margin(t = -1, r = -1, b = -1, l = -1)),
        axis.line = element_line(size = .5, color = 'black'),
        legend.margin=margin(0,0,-2,0), axis.ticks.y = element_blank(),
        legend.box.margin=margin(5,-3,-1,-5), axis.title = element_text(size=10),
        axis.text.x = element_text(size=8, color = "black"),  
        axis.text.y = element_text(size=8, color = "black"),
        legend.position = "bottom", legend.text = element_text(size=8),
        legend.title=element_blank())+        
  guides(fill=guide_legend(nrow = 1, keywidth = 1, keyheight = 1))+
  annotate(geom="text", x=c(4,6), y=80, hjust=0, label="*", size=4)+
  annotate(geom="text", x=1.5, y=170, size=3, hjust=0.5, label="Site 1")+
  annotate(geom="text", x=3.5, y=170, size=3, hjust=0.5, label="Site 2")+
  annotate(geom="text", x=5.5, y=170, size=3, hjust=0.5, label="Site 3")+
  geom_vline(xintercept = c(2.5,4.5), size=.5, linetype=3)+
  annotate("segment", x = 0.55, xend = 1.45, y = 21.5, yend = 23, colour = "black")+
  annotate("segment", x = 2.55, xend = 3.45, y = 21.5, yend = 23, colour = "black")+
  annotate("segment", x = 4.55, xend = 5.45, y = 21.5, yend = 23, colour = "black")+
  annotate("segment", x = 0.55, xend = 1.45, y = 23.5, yend = 25, colour = "black")+
  annotate("segment", x = 2.55, xend = 3.45, y = 23.5, yend = 25, colour = "black")+
  annotate("segment", x = 4.55, xend = 5.45, y = 23.5, yend = 25, colour = "black")

rm(list = ls())
################### Regeneration Environment ##############################
# Libraries
if(!require(statmod)){install.packages("statmod")}     # tweedie
if(!require(pscl)){install.packages("pscl")}           # zeroinfl
if(!require(lmtest)){install.packages("lmtest")}       # lrtest

# Read data - Grass
Gram<-read.table("3_Gram.csv",sep=",",header=T)
names(Gram)<-c("Site", "plot","treatm","Bloco","ID", 
               "Sub","G1", "G2","Above_PAR","Below_PAR","LAI","R_FR")

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
# Red - Far Red
ZIP<-zeroinfl(100*G1 ~ treatm*R_FR, dist="poisson",link="logit",data=Gram)
ZINB<-zeroinfl(100*G1 ~treatm*R_FR, dist="negbin",link="logit",data=Gram) 
lrtest(ZIP,ZINB) # look at LogLik, then ZINB is better
summary(ZINB) # ZINB shows no difference for Red-FarRed between treatments
rm(list = ls())

## Light dynamic
# Site 1
light_site1<-read.csv("4_LUZ_ARA.csv",header = T,sep=",") %>% 
  dplyr::select(parcela, Tratamento, IAF_pre, IAF_7ms) %>% 
  filter(Tratamento!="EEW") %>%
  gather(tempo,IAF,-c(parcela, Tratamento)) %>%
  mutate(Tempo=as.factor(ifelse(tempo=="IAF_pre","t1","t2")),
         Trata = as.factor(ifelse(Tratamento=="NNW","Nat","Euc")))

mod_full<-lmer(IAF~Trata*Tempo+(1|parcela), light_site1)
mod_solo<-lmer(IAF~Trata+Tempo+(1|parcela), light_site1)
mod_trat<-lmer(IAF~Trata+(1|parcela), light_site1)
mod_tempo<-lmer(IAF~Tempo+(1|parcela), light_site1)
AIC(mod_full,mod_tempo,mod_trat,mod_solo)
plot(mod_full)
shapiro.test(residuals(mod_full))
summary(mod_full)
require(lsmeans)
lsmeans(mod_full,"Trata")
lsmeans(mod_full,"Tempo")
resulta<-as.data.frame(difflsmeans(mod_full, test.effs= "Trata:Tempo"))[,c(1,9)]
resulta$`Pr(>|t|)`<-round(resulta$`Pr(>|t|)`,4)
resulta

# Barplot light site 1
light_site1 %>%   group_by(Trata, Tempo) %>%
  dplyr::summarise(IAF_min = as.numeric(mean_se(IAF)[2]),
                   IAF_med = as.numeric(mean_se(IAF)[1]),
                   IAF_max = as.numeric(mean_se(IAF)[3])) %>%
  ggplot(aes(x=Tempo, y=IAF_med, fill=Trata)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=IAF_min, ymax=IAF_max),
                width=.2,position=position_dodge(.9)) + 
  theme_bw()+ylab("Indice Abertura Florestal")+
  scale_y_continuous(limits = c(0,5),expand = c(0,0))+
  geom_text(x=0.75, y=4.5, label="Ab")+
  geom_text(x=1.25, y=4.5, label="Aa")+
  geom_text(x=1.75, y=4.5, label="Aa")+
  geom_text(x=2.25, y=4.5, label="Bb")
ggsave("Fig_barplot_IAF.jpg")
# LETTERS: Treatment in the same time
# letters: Times in the same treatment

# Site 3
light_site3<-read.csv("4_LUZ_IGR.csv",header = T,sep=",") %>% 
  dplyr::select(tratamento, parcela, ponto, RFR_pre,RFR_1ano) %>%
  gather(tempo,RFR,-c(tratamento, parcela, ponto)) %>%
  mutate(Tempo=as.factor(ifelse(tempo=="RFR_pre","t1","t2")),
         Trata = as.factor(ifelse(tratamento=="NNW","Nat","Euc")))
head(light_site3)

mod_full<-lmer(RFR~Trata*Tempo+(1|parcela), light_site3)
mod_trat<-lmer(RFR~Trata+(1|parcela), light_site3)
mod_tempo<-lmer(RFR~Tempo+(1|parcela), light_site3)
AIC(mod_full,mod_tempo,mod_trat)
plot(mod_full)
par(mfrow=c(1,1))
qqnorm(residuals(mod_full));qqline(residuals(mod_full))
summary(mod_solo)

lsmeans(mod_full,"Trata")
lsmeans(mod_full,"Tempo")
resulta<-as.data.frame(difflsmeans(mod_full, test.effs= "Trata:Tempo"))
resulta$`Pr(>|t|)`<-round(resulta$`Pr(>|t|)`,4)
resulta

light_site3 %>%   group_by(Trata, Tempo) %>%
  dplyr::summarise(RFR_min = as.numeric(mean_se(RFR)[2]),
                   RFR_med = as.numeric(mean_se(RFR)[1]),
                   RFR_max = as.numeric(mean_se(RFR)[3])) %>%
  ggplot(aes(x=Tempo, y=RFR_med, fill=Trata)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=RFR_min, ymax=RFR_max),
                width=.2,position=position_dodge(.9)) + 
  theme_bw()+ylab("R/FR")+
  scale_y_continuous(limits = c(0,1),expand = c(0,0))+
  geom_text(x=0.75, y=0.93, label="Ba")+
  geom_text(x=1.25, y=0.93, label="Aa")+
  geom_text(x=1.75, y=0.93, label="Bb")+
  geom_text(x=2.25, y=0.93, label="Ab")
ggsave("Fig_barplot_RFR.jpg")
# LETTERS: Treatment in the same time
# letters: Times in the same treatment

# (----PAREI AQUI---)
rm(list=ls())
################### Regeneration of native woody species  #################
# Libraries:
if(!require(iNEXT)){install.packages("iNEXT")}         # rarefaction curves
if(!require(vegan)){install.packages("vegan")}         # diversity index
# if(!require(mgcv)){install.packages('mgcv')}
if(!require(gamm4)){install.packages('gamm4')}         # GAMM
if(!require(MuMIn)){install.packages('MuMIn')}         # uGamm

# Site 1
# Logged in 2015
### ARACRUZ
# Carina 
site1_pre <- as.data.frame(read_xlsx('Regenera_Especies_Carina3.xlsx' , sheet='ARA')) %>%
  filter(`forma vida`=="Árvore") %>% 
  mutate(w_Eucalypt=NED+EUC, Only_Native=NAT) 
rownames(site1_pre) <-site1_pre[,1]
site1_pre <- dplyr::select(site1_pre, Only_Native, w_Eucalypt) # Carina

site1_pos <- read_xlsx("Regenera_Aracruz_taisi2018.xlsx", sheet="Plan1") %>%# Taisi
  mutate(Modelo=ifelse(Modelo==1,'Only_Native','w_Eucalypt')) %>%
  filter(OBS=="Tree") %>% 
  with(table(Modelo,Coleta)) %>% t() %>% as.data.frame() %>% 
  spread(Modelo, Freq)
rownames(site1_pos) <- site1_pos[,1]
site1_pos <- dplyr::select(site1_pos, Only_Native, w_Eucalypt)

# Site 3 - Igrapiuna
site3_pre <- as.data.frame(read_xlsx('Regenera_Especies_OCT_dinamica_PB.xlsx', sheet = "Carina"))
rownames(site3_pre)<-site3_pre[,1]
site3_pre <- site3_pre %>% dplyr::select(NAT1, CCC1, CSC1) # 1 mai 2015 - Carina = 1: 46

site3_pos <- read_xlsx('Regenera_Especies_OCT_dinamica_PB.xlsx', 
                       sheet = "Taisi") %>%as.data.frame()
rownames(site3_pos)<-site3_pos$Nome
site3_pos <- site3_pos %>% dplyr::select(NAT4,CCC4,CSC4) %>%  # 4 mai 2018 - Taisi = 38 meses: 83
  mutate(NAT4=ifelse(is.na(NAT4),0,NAT4),
         CCC4=ifelse(is.na(CCC4),0,CCC4),
         CSC4=ifelse(is.na(CSC4),0,CSC4))

# Rarefaction curves
iNEXT(site1_pre,datatype="abundance") %>% plot()
iNEXT(site1_pos,datatype="abundance") %>% plot()

iNEXT(site3_pre,datatype="abundance") %>% plot()
iNEXT(site3_pos,datatype="abundance") %>% plot()

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
                      trata = c('native', "harvested", "no_harvested"))%>%
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

rbind(diversidade(site1_pre),diversidade(site1_pos))
rbind(diversidade(site3_pre),diversidade(site3_pos))

################### Growth of planted non-pioneer trees ###################
# Libraries:
if(!require(nlme)){install.packages('nlme')}           # mixed models
if(!require(lsmeans)){install.packages('lsmeans')}     # lsmeans, lstrends

### Reading and managing Data: 
## ARACRUZ: Harvested 57 months
ARA <- read_xlsx("Dynamic_Basal_Area_ARA_IGR.xlsx",sheet = "ARA") %>%
  mutate(spp = `nome cientifico`) %>%
  dplyr::select(plot,tree, spp, SPP, block, treatment, wood,
                g20m2, g39m2, g52m2, g58m2)
ARA$wood[ARA$wood!='INI']<-'SEC'
ARA$treatment[ARA$treatment!='NNW']<-'EUC'
ARA$SPP[ARA$SPP!='EUC']<-'NAT'

ARA_83<-read_xlsx("Plantio_misto_ES_BA.xlsx", sheet = "Fibria - Aracruz") %>% 
  mutate(g83m2 = ((soma_cap^2)/(4*pi))*0.0001,tree = treeposition) %>% 
  dplyr::select(plot, tree, g83m2) 

ARAtotal <- left_join(ARA, ARA_83, by=c('plot', 'tree'))

# Is there some Eucalypts?
ARAtotal %>%  filter(SPP=='EUC') %>% filter(!is.na(g83m2)) %>%
  mutate(dif=g52m2-g83m2) %>% filter(dif>0)%>%
  with(table(plot))
# excluding plots with Eucalypts
ARAtotal<- subset(ARAtotal, !(plot %in% c(1,12)))

ARA_long <- ARAtotal %>% group_by(plot,treatment, block, wood, SPP) %>%
  gather(time, basal_area,c(g20m2, g39m2, g52m2, g58m2, g83m2), factor_key = T) %>%
  mutate(time = as.numeric(time)) %>% unite(tipo, c("SPP","wood"))
ARA_long$time[ARA_long$time==1]<-20
ARA_long$time[ARA_long$time==2]<-39
ARA_long$time[ARA_long$time==3]<-52
ARA_long$time[ARA_long$time==4]<-58
ARA_long$time[ARA_long$time==5]<-83

# Any outlier?
ARA_long %>% filter(treatment =='EUC'&tipo=='NAT_INI')
# Excluding Jacaratia (pioneer tree) of treatment EUC (only eucalypt as pioneer)
ARA_long<-ARA_long %>% filter(treatment!='EUC'|tipo!='NAT_INI') %>% 
  droplevels() %>% 
  mutate(logBA=ifelse(is.na(basal_area),NA,log(basal_area+1)))

ARA_long  %>% filter(!is.na(basal_area))%>% 
  group_by(treatment,  tipo, time) %>%
  dplyr::summarise(Girth_min = as.numeric(mean_se(basal_area)[2]),
                   Girth_med = as.numeric(mean_se(basal_area)[1]),
                   Girth_max = as.numeric(mean_se(basal_area)[3])) 
rm(ARA,ARA_83,ARAtotal)

suma<-function(x){sum(x,na.rm=T)}
ARA_LONG <- ARA_long %>% filter(!is.na(basal_area)) %>%
  group_by(time,plot,treatment, tipo) %>% 
  summarise(basal_area = 
              (suma(basal_area)/540)*10000)  #plot size = 2,160 m

## IGRAPIUNA: Harvested 53 months
OCT <- read_xlsx("Dynamic_Basal_Area_ARA_IGR.xlsx",sheet = "IGR") %>%
  dplyr::select(plot,tree, SPP,block, treatment, wood,
                g31m2, g45m2,g53m2, g60m2)
OCT$wood[OCT$wood!='INI']<-'SEC'
OCT$SPP[OCT$SPP!='EUC']<-'NAT'
OCT$treatment[OCT$treatment=='NNW']<-'Only_Nat'
OCT$treatment[OCT$treatment=='NEW']<-'Euc_NonHarvest'
OCT$treatment[OCT$treatment=='NEC']<-'Euc_Harvest'

OCT_83<-read_xlsx("Plantio_misto_ES_BA.xlsx",
                  sheet = "OCT - Igr") %>%  mutate(
                    g83m2= ((soma_cap^2)/(4*pi))*0.0001, # circunf (cm) to basal area (m2)
                    spp=`nome cientifico`, tree = treeposition) %>%                      # names changed 
  dplyr::select(plot, tree,  g83m2)

OCTtotal <- left_join(OCT, OCT_83, by=c('plot', 'tree'))

## Excluding plots with eucalypts
# OCTtotal<- subset(OCTtotal, !(plot %in% c(1,8,15,19,22,27)))

OCT_long <- OCTtotal %>% group_by(plot,treatment, block, wood, SPP) %>%
  gather(time, basal_area,c(g31m2, g45m2,g53m2, g60m2, g83m2), factor_key = T) %>%
  mutate(time = as.numeric(time)) %>% unite(tipo, c("SPP","wood"))
OCT_long$time[OCT_long$time==1]<-31
OCT_long$time[OCT_long$time==2]<-45
OCT_long$time[OCT_long$time==3]<-53
OCT_long$time[OCT_long$time==4]<-60
OCT_long$time[OCT_long$time==5]<-83


OCT_long %>% filter(!is.na(basal_area)) %>% with(table(plot,time))
OCT_long %>% group_by(treatment,  tipo, time)%>%
  dplyr::summarise(Girth_min = as.numeric(mean_se(basal_area)[2]),
                   Girth_med = as.numeric(mean_se(basal_area)[1]),
                   Girth_max = as.numeric(mean_se(basal_area)[3])) 
rm(OCT,OCT_83,OCTtotal)

OCT_LONG <- OCT_long %>% filter(!is.na(basal_area)) %>%
  group_by(time,plot,treatment, tipo) %>% 
  summarise(basal_area = (suma(basal_area)*10000)/390) #plot size = 2,160 m

### Figures
ggplot(ARA_LONG,aes(x=time, y=basal_area, group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()+
  ylab("basal area (m2/ha)")

ggplot(OCT_LONG, aes(x=time,y=basal_area,group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()

## ARA: site 1
### Compare NAT_SEC between treatments BEFORE harvesting:
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot, ftime = as.factor(time)) %>% filter(!is.na(basal_area))

if(!require(nlme)){install.packages('nlme')}
mod0<-lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
          method='REML', data=dados)
plot(mod0) # problem on residuals

mod_site1 <- lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
                 method='REML', data=dados,
                 weights = varPower()) # Power of the variance covariate
plot(mod_site1) # 
anova(mod_site1, mod0)
# The log likelihood ratio statistic is 5.68, indicating that 
# the variance structure in `mod1` is considerably better than the 
# constant variance in the linear regression model `mod0`. 
# Hence, the `varPower` option provides a significantly 
# better variance structure than the one used for the 
# linear regression model in `mod0`. 
# In a paper, you would write this as L=5.68 (df = 8-7 = 1, p<0.05).

anova(mod_site1)
summary(mod_site1, cor=F)
VarCorr(mod_site1)

## OCT: site 3
### Compare NAT_SEC between treatments
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot,ftime=as.factor(time))

mod0<-lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
          method='REML', na.action=na.exclude, data=dados)

plot(mod0) # problem on residuals

mod_site3 <- lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
                 method='REML', na.action=na.exclude, 
                 data=dados,
                 weights = varPower()) # Power of the variance covariate
plot(mod_site3) 

anova(mod_site3, mod0)
# The log likelihood ratio statistic is 133.66, indicating that 
# the variance structure # in `mod1` is considerably better than the 
# constant variance in the linear regression # model `mod0`. 
# Hence, the `varPower` option provides a significantly better 
# variance structure than the one used for the linear regression model 
# in mod_site3. In a paper,# you would write this as L = 133.66 (df = 1, p < 0.001).
anova(mod_site3)
summary(mod_site3)

## ARA: site 1
### Compare post harvesting 

dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>57) %>% filter(!is.na(basal_area)) %>%
  mutate(ID = plot) 

mod_post1 <- lme(basal_area ~ treatment, random= ~ 1|ID,
                 weights = varExp(form=~time),
                 correlation=corAR1(),
                 data=dados)
plot(mod_post1)
summary(mod_post1)
anova(mod_post1)

## OCT
### Post harvesting
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time>55) %>%
  mutate(ID = plot) 

mod_post3 <- lme(basal_area ~ treatment, random= ~ 1|ID,
                 weights = varExp(form=~time),
                 correlation=corAR1(), na.action =na.omit, 
                 data=dados)
plot(mod_post3)
summary(mod_post3, ddf = "Kenward-Roger")
anova(mod_post3)

# Relatorio
rm(mod0)

# Before Harvesting
if(!require(lsmeans)){install.packages('lsmeans')}
# Site 1 ARA_LONG
levels(factor(ARA_long$time))
levels(factor(OCT_long$time))

site1 <- ARA_long %>% filter(time==83) %>% filter(tipo=="NAT_SEC") %>%
  ungroup()  %>% group_by(treatment) %>% 
  summarise(media = mean_cl_boot(basal_area)[[1]],
            media_min = mean_cl_boot(basal_area)[[2]],
            media_max = mean_cl_boot(basal_area)[[3]],
            Site = "Site_1")
site3 <- OCT_long %>% filter(time==83) %>% filter(tipo=="NAT_SEC") %>%
  ungroup()  %>% group_by(treatment) %>% 
  summarise(media = mean_cl_boot(basal_area)[[1]],
            media_min = mean_cl_boot(basal_area)[[2]],
            media_max = mean_cl_boot(basal_area)[[3]],
            Site = "Site_3")
rbind(site1,site3)

anova.lme(mod_site3)
fixef(mod_site3) # m2.ha-1 
summary(mod_site3)
intervals(mod_site3, which='fixed')
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot,ftime=as.factor(time))
lsmeans::lsmeans(mod_site3, ~ treatment|time, trend = "time", type="response")


anova.lme(mod_site1)
fixef(mod_site1) # m2.ha-1
summary(mod_site1)
intervals(mod_site1, which='fixed')
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot,
         ftime = as.factor(time)) %>% filter(!is.na(basal_area))
lsmeans::lsmeans(mod_site1, ~ treatment|time, trend = "time", type="response")

# Post Harvesting
fixef(mod_post1) # m2.ha-1 
summary(mod_post1)
anova(mod_post1)
intervals(mod_post1, which='fixed')
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>57) %>% filter(!is.na(basal_area)) %>%
  mutate(ID = plot) 
lsmeans::lsmeans(mod_post1, "treatment")

dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time>55) %>%
  mutate(ID = plot) 
lsmeans::lsmeans(mod_post3, "treatment")
fixef(mod_post3) # m2.ha-1 
summary(mod_post3)
anova(mod_post3)
intervals(mod_post3, which='fixed')

##
ARA_BAI<-ARA_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>55) %>% filter(!is.na(basal_area)) %>% ungroup() %>%
  mutate(ID = plot, time=as.factor(time)) %>% 
  dplyr::select(treatment, ID, basal_area, time) %>%
  spread(time, basal_area) %>% mutate(BAI = `83`-`58`)
ARA_BAI %>% with(t.test(BAI~treatment))
ggplot(ARA_BAI,aes(BAI))+
  geom_density()+facet_grid(.~treatment)

## BA Final - site 1
ARA_BAI %>%  mutate(Final = `83`) %>% 
   #filter(treatment=="NNW") %>% with(shapiro.test(BAI))
   #filter(treatment=="EUC") %>% with(shapiro.test(BAI))
  with(t.test(Final~treatment))

# Comparar BAI entre colhida e não colhida
OCT_BAI<-OCT_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>57) %>% filter(!is.na(basal_area)) %>% ungroup() %>%
  mutate(ID = plot, time=as.factor(time),
         treatment=as.factor(treatment)) %>% 
  dplyr::select(treatment, ID, basal_area, time) %>%
  spread(time, basal_area) %>% mutate(BAI = `83`-`60`) 

OCT_BAI %>% filter(treatment=="Euc_Harvest") %>% 
  with(shapiro.test(BAI))

OCT_BAI %>% filter(treatment=="Euc_NonHarvest") %>% 
  with(shapiro.test(BAI))

OCT_BAI %>% filter(treatment!="Only_Nat") %>% droplevels()%>%
  with(t.test(BAI~treatment))

ggplot(OCT_BAI,aes(BAI))+
  geom_density()+facet_grid(.~treatment)

# Comparar valor final de BA de nativas
OCT_BAI$Final<-OCT_BAI$`83`
OCT_BAI$plant <- ifelse(OCT_BAI$treatment=="Only_Nat", "Nat", "Euc")
shapiro.test(OCT_BAI$`83`[OCT_BAI$plant=="Euc"])
shapiro.test(OCT_BAI$`83`[OCT_BAI$plant=="Nat"])
OCT_BAI %>% with(t.test(Final~plant))
mean(OCT_BAI$`83`[OCT_BAI$plant=="Nat"])/mean(OCT_BAI$`83`[OCT_BAI$plant=="Euc"])
sd(OCT_BAI$`83`[OCT_BAI$plant=="Nat"])
sd(OCT_BAI$`83`[OCT_BAI$plant=="Euc"])

## END