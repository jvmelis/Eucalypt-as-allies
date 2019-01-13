wd<-"~/ARTIGOS - Meus/0 MS - Eucalipto e regenerantes"
setwd(wd)
rm(list=ls())

# General:
if(!require(tidyverse)){install.packages("tidyverse")} # ggplot, tidyr, broom, dplyr...
if(!require(readxl)){install.packages("readxl")}       # read_excel
# Part 2
if(!require(statmod)){install.packages("statmod")}     # tweedie
if(!require(pscl)){install.packages("pscl")}           # zeroinfl
if(!require(lmtest)){install.packages("lmtest")}       # lrtest

# Part 3:
if(!require(iNEXT)){install.packages("iNEXT")}         # rarefaction curves
if(!require(vegan)){install.packages("vegan")}         # diversity index
# if(!require(mgcv)){install.packages('mgcv')}
if(!require(gamm4)){install.packages('gamm4')}         # GAMM
# if(!require(MuMIn)){install.packages('MuMIn')}         # 

# Part 4:
if(!require(nlme)){install.packages('nlme')}           # mixed models
if(!require(lsmeans)){install.packages('lsmeans')}     # lsmeans, lstrends


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
# Read data
Gram<-read.table("3_Gram.csv",sep=",",header=T)
names(Gram)<-c("Site", "plot","treatm","Bloco","ID", 
               "Sub","G1", "G2","Above_PAR","Below_PAR","LAI","R_FR")

# Site 1  Aracruz
gramA <- Gram %>% filter(Site=="ARA") %>% droplevels()
(tipos<-levels(gramA$treatm))
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
par(mfrow=c(2,2))
plot(mod.Ara)
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
par(mfrow=c(2,2))
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
anova(mod.Igr)
# Red - Far Red
ZIP<-zeroinfl(100*G1 ~ treatm*R_FR, dist="poisson",link="logit",data=Gram)
ZINB<-zeroinfl(100*G1 ~treatm*R_FR, dist="negbin",link="logit",data=Gram) 
lrtest(ZIP,ZINB) # look at LogLik, then ZINB is better
summary(ZINB) # ZINB shows no difference for Red-FarRed between treatments
rm(list = ls())
################### Regeneration of native woody species  #################


################### Growth of planted non-pioneer trees ###################

