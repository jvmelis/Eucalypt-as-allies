wd<-"~/ARTIGOS - Meus/0 MS - Eucalipto e regenerantes"
setwd(wd)
rm(list=ls())

if(!require(ggplot2)){install.packages("ggplot2")} 
if(!require(dplyr)){install.packages("dplyr")} 
if(!require(tidyr)){install.packages("tidyr")} 
if(!require(readxl)){install.packages("readxl")}       # read_excel
if(!require(Hmisc)){install.packages("Hmisc")}
# Dynamic in ARACRUZ
# Harvested 57 months
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

ARAtotal %>%  filter(SPP=='EUC') %>% filter(!is.na(g83m2)) %>%
  mutate(dif=g52m2-g83m2) %>% filter(dif>0)%>%
  with(table(block,plot))
with(ARAtotal,table(block, plot))

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

# Excluding Jacaratia (pioneer tree) of treatment EUC (only eucalypt as pioneer)
ARA_long %>% filter(treatment =='EUC'&tipo=='NAT_INI')
ARA_long<-ARA_long %>% filter(treatment!='EUC'|tipo!='NAT_INI') %>% 
  droplevels() %>% 
  mutate(logBA=ifelse(is.na(basal_area),NA,log(basal_area+1)))


ARA_long  %>% filter(!is.na(basal_area))%>% 
  group_by(treatment,  tipo, time) %>%
  dplyr::summarise(Girth_min = as.numeric(mean_se(basal_area)[2]),
                   Girth_med = as.numeric(mean_se(basal_area)[1]),
                   Girth_max = as.numeric(mean_se(basal_area)[3])) 
suma<-function(x){sum(x,na.rm=T)}
ARA_LONG<-ARA_long %>% filter(!is.na(basal_area)) %>%
  group_by(time,plot,treatment, tipo) %>% 
  summarise(basal_area = (suma(basal_area)/540)*10000) #plot size = 2,160 m
ARA_LONG %>% 
  ggplot(aes(x=time, y=basal_area, group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  #geom_point(aes(color=tipo),size=1)+
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()+
  ylab("basal area (m2/ha)")
ggsave('ARACRUZ_BasalArea_dynamics.jpg')

ARA_LONG %>% filter(!is.na(basal_area)) %>% 
  mutate(grafo=ifelse(tipo=="NAT_SEC", "Non_pioneer","Pioneer"))%>%
  ggplot(aes(x=time, y=basal_area, group=plot)) +
  geom_point(aes(color=tipo, group=time))+
  geom_smooth(method="lm", se=FALSE, aes(group=tipo, color=tipo))+
  facet_grid(grafo~treatment)+theme_bw()+
  ylab("basal area (m2/ha)")
ggsave('ARACRUZ_BasalArea_linear.jpg')

### Dynamic in IGRAPIUNA
OCT <- read_xlsx("Dynamic_Basal_Area_ARA_IGR.xlsx",sheet = "IGR") %>%
  dplyr::select(plot,tree, SPP,block, treatment, wood,
                g31m2, g45m2,g53m2, g60m2)
OCT$wood[OCT$wood!='INI']<-'SEC'
OCT$SPP[OCT$SPP!='EUC']<-'NAT'
levels(factor(OCT$treatment))
OCT$treatment[OCT$treatment=='NNW']<-'Only_Nat'
OCT$treatment[OCT$treatment=='NEW']<-'Euc_Harvest'
OCT$treatment[OCT$treatment=='NEC']<-'Euc_NonHarvest'

OCT_83<-read_xlsx("Plantio_misto_ES_BA.xlsx",
                   sheet = "OCT - Igr") %>%  mutate(
  g83m2= ((soma_cap^2)/(4*pi))*0.0001, # circunf (cm) to basal area (m2)
  spp=`nome cientifico`, tree = treeposition) %>%                      # names changed 
  dplyr::select(plot, tree,  g83m2)

OCTtotal <- left_join(OCT, OCT_83, by=c('plot', 'tree'))

## Differing harvested and non_harvested blocks
OCTtotal %>%  filter(SPP=='EUC') %>% filter(!is.na(g83m2)) %>%
  mutate(dif=g45m2-g83m2) %>% filter(dif>0)%>%
  with(table(block,plot))
with(OCTtotal,table(block, plot))

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

OCT_LONG <- OCT_long %>% filter(!is.na(basal_area)) %>%
  group_by(time,plot,treatment, tipo) %>% 
  summarise(basal_area = (suma(basal_area)*10000)/390) #plot size = 2,160 m

ggplot(OCT_LONG, aes(x=time,y=basal_area,group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()
ggsave('OCT_BasalArea_dynamics.jpg')

OCT_LONG %>% filter(!is.na(basal_area)) %>% 
  mutate(grafo=ifelse(tipo=="NAT_SEC", "Non_pioneer","Pioneer"))%>%
  ggplot(aes(x=time,y=basal_area,group=plot)) +
  geom_point(aes(color=tipo, group=time))+
  geom_smooth(method="lm", se=FALSE, aes(color=tipo, group=tipo))+
  facet_grid(grafo~treatment)+theme_bw()
ggsave('OCT_BasalArea_linear.jpg')

#############################################################################
## ARA: site 1
### Compare NAT_SEC between treatments BEFORE harvesting:
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot,
         ftime = as.factor(time)) %>% filter(!is.na(basal_area))

if(!require(nlme)){install.packages('nlme')}
# discussion on: https://dynamicecology.wordpress.com/2015/11/04/is-it-a-fixed-or-random-effect/
mod0<-lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
          method='REML', data=dados)
plot(mod0) # problem on residuals

mod_site1 <- lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
            method='REML',
            data=dados,
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
fixef(mod_site1)
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
ARA_LONG %>% filter(!is.na(basal_area)) %>% with(table(time))
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>57) %>% filter(!is.na(basal_area)) %>%
  mutate(ID = plot) 

# https://stats.stackexchange.com/q/352478
mod_post1 <- lme(basal_area ~ treatment, random= ~ 1|ID,
                weights = varExp(form=~time),
                correlation=corAR1(),
                data=dados)
plot(mod_post1)
# if(!require(lme4)){install.packages('lme4')}
# mod_pos <- lmer(basal_area ~ treatment+(1|ID), data=dados)
# confint(mod_pos)
summary(mod_post1, ddf = "Kenward-Roger")
summary(mod_post1)
anova(mod_post1)

## OCT
### Post harvesting
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time>55) %>%
  mutate(ID = plot) 

# https://stats.stackexchange.com/q/352478
mod_post3 <- lme(basal_area ~ treatment, random= ~ 1|ID,
                 weights = varExp(form=~time),
                 correlation=corAR1(), na.action =na.omit, 
                 data=dados)
plot(mod_post3)
summary(mod_post3, ddf = "Kenward-Roger")
anova(mod_post3)

######################## Relatorio
if(!require(lsmeans)){install.packages('lsmeans')}
anova.lme(mod_site1)
anova.lme(mod_site3)

fixef(mod_site1) # m2.ha-1
fixef(mod_site3) # m2.ha-1 

summary(mod_site1)
summary(mod_site3)

intervals(mod_site1, which='fixed')
intervals(mod_site3, which='fixed')

fixef(mod_post1) # m2.ha-1 
summary(mod_post1)
anova(mod_post1)
intervals(mod_post1, which='fixed')
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
  spread(time, basal_area) %>% mutate(BAI = `83`-`58`) #%>% filter(BAI>0)
ARA_BAI %>% with(wilcox.test(BAI~treatment))
ggplot(ARA_BAI,aes(BAI))+
  geom_density()+facet_grid(.~treatment)

OCT_BAI<-OCT_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>57) %>% filter(!is.na(basal_area)) %>% ungroup() %>%
  mutate(ID = plot, time=as.factor(time)) %>% 
  dplyr::select(treatment, ID, basal_area, time) %>%
  spread(time, basal_area) %>% mutate(BAI = `83`-`60`) 
OCT_BAI %>% with(wilcox.test(BAI~treatment))
ggplot(OCT_BAI,aes(BAI))+
  geom_density()+facet_grid(.~treatment)
