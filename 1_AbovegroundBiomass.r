# Aboveground biomass accumulation and growth of planted trees
# Libraries:
if(!require(tidyverse)){install.packages("tidyverse")} # ggplot, tidyr, broom, dplyr
if(!require(lme4)){install.packages("lme4")}           # mixed models
if(!require(nlme)){install.packages('nlme')}           # mixed models
if(!require(lsmeans)){install.packages('lsmeans')}     # lsmeans, lstrends

##  Comparing Site 1 - Total Biomass and Non-pioneer tree Biomass 
# Reading data
biomass <- read.csv("1_Biomass_3_sites.csv") 
ara <- biomass  %>% mutate(local=paste(site, treatment,sep="_") ) %>%
  filter(site=='ara') %>% filter(treatment!="EUC")%>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>% 
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC)
ara %>% with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"]))  # site 1
ara %>%    with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))    # NS

#  Comparing Site 2 - Total Biomass and Non-pioneer tree Biomass 
muc <- biomass  %>% mutate(local=paste(site,treatment,sep="_") ) %>%
  filter(site=='muc') %>% filter(treatment!="EUC")%>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>%
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC)
muc %>% with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"]))  # p=0.002
muc %>% with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))    # p=0.01 

#  Comparing Site 3 - Total Biomass and Non-pioneer tree Biomass 
igr <- biomass  %>% mutate(local=paste(site,treatment,sep="_") ) %>%
  filter(site=='oct') %>% filter(treatment!="EUC") %>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>%
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC)
igr %>% with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"])) # site 3
igr %>% with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))

## Figure 2 - Aboveground biomass (AGB) accumulation 
df <- biomass  %>% dplyr::select(site, sppbiomass, treatment, Mg_ha) %>% 
  filter(treatment!="EUC")  %>% group_by(site,treatment, sppbiomass)%>% 
  summarise(media=mean(Mg_ha, na.rm=T),
            ymax= as.numeric(paste(mean_se(Mg_ha)[3])),
            ymin= as.numeric(paste(mean_se(Mg_ha)[2]))) %>% 
  ungroup()%>% mutate(local=paste(site,treatment,sep="_")) %>% 
  mutate(local=factor(local))

df$ymax[df$sppbiomass=='EUCALYPTUS']<-df$ymax[df$sppbiomass=='EUCALYPTUS']+df$ymax[df$sppbiomass=='SEC'&df$treatment!='NNW']
df$ymin[df$sppbiomass=='EUCALYPTUS']<-df$ymin[df$sppbiomass=='EUCALYPTUS']+df$ymin[df$sppbiomass=='SEC'&df$treatment!='NNW']
df$ymax[df$sppbiomass=='PIONEER']<-df$ymax[df$sppbiomass=='PIONEER']+df$ymax[df$sppbiomass=='SEC'&df$treatment=='NNW']
df$ymin[df$sppbiomass=='PIONEER']<-df$ymin[df$sppbiomass=='PIONEER']+df$ymin[df$sppbiomass=='SEC'&df$treatment=='NNW']

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
rm(list=ls())
#### Growth of planted non-pioneer trees 
### Reading and managing Data: 
source("1_readingData.r")

## Figure 3. Temporal variation in basal area of species groups
A.Site1<-ggplot(ARA_LONG,aes(x=time, y=basal_area, group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()+
  ylab("basal area (m2/ha)")

B.Site3<-ggplot(OCT_LONG, aes(x=time,y=basal_area,group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()
cowplot::plot_grid(A.Site1, B.Site3,labels = c("A. Site 1", "B. Site 3"), ncol = 1)


# Basal Area of trees BEFORE Harvesting
site1 <- ARA_long %>% filter(time==83) %>% filter(tipo=="NAT_SEC") %>%
  ungroup()  %>% group_by(treatment) %>% 
  summarise(AverageBasalArea = mean_cl_boot(basal_area)[[1]],
            AverageBasalArea_min = mean_cl_boot(basal_area)[[2]],
            AverageBasalArea_max = mean_cl_boot(basal_area)[[3]],
            Site = "Site_1")
site3 <- OCT_long %>% filter(time==83) %>% filter(tipo=="NAT_SEC") %>%
  ungroup()  %>% group_by(treatment) %>% 
  summarise(AverageBasalArea = mean_cl_boot(basal_area)[[1]],
            AverageBasalArea_min = mean_cl_boot(basal_area)[[2]],
            AverageBasalArea_max = mean_cl_boot(basal_area)[[3]],
            Site = "Site_3")
rbind(site1,site3) # EUC: Eucalypts; NNW = Native 
rm(site1,site3)

## ARA: site 1
# Compare the growth of planted non-pioneer trees (NAT_SEC):
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% 
  mutate(ID = plot, ftime = as.factor(time)) %>% filter(!is.na(basal_area))

mod0<-lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
          method='REML', data=dados)
plot(mod0) # problem on residuals

mod_site1 <- lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
                 method='REML', data=dados,
                 weights = varExp()) # Power of the variance covariate
plot(mod_site1) # 
anova(mod_site1, mod0)
# The log likelihood ratio statistic is 40.27, indicating that 
# the variance structure in `mod1` is considerably better than the 
# constant variance in the linear regression model `mod0`. 
## L = 40.27 (df = 8-7 = 1, p<0.001).
# Hence, the `varPower` option provides a significantly 
# better variance structure than the one used for the 
# linear regression model in `mod0`. 

anova(mod_site1)
summary(mod_site1, cor=F)
VarCorr(mod_site1)
intervals(mod_site1, which='fixed')

## OCT: site 3
# Compare the growth of planted non-pioneer trees (NAT_SEC):
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% 
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
# The log likelihood ratio statistic is 28.236, indicating that 
# the variance structure # in `mod1` is considerably better than the 
# constant variance in the linear regression model `mod0`. 
## L = 28.24 (df = 1, p < 0.001).
# Hence, the `varPower` option provides a significantly 
# better variance structure than the one used for 
# the linear regression model in `mod_site3`. 

anova(mod_site3)
summary(mod_site3)
intervals(mod_site3, which='fixed')

## Site 1:
# Compare the growth of planted non-pioneer trees (NAT_SEC) BEFORE harvesting:
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot, ftime = as.factor(time)) %>% filter(!is.na(basal_area))

mod0<-lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
          method='REML', data=dados)
plot(mod0) # problem on residuals

mod_site_pre1 <- lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
                 method='REML', data=dados,
                 weights = varExp()) # Power of the variance covariate
plot(mod_site_pre1) # 
anova(mod_site_pre1, mod0)

# Compare the growth of planted non-pioneer trees (NAT_SEC) AFTER harvesting:
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

## Site 3:
# Compare the growth of planted non-pioneer trees (NAT_SEC) BEFORE harvesting:
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot,ftime=as.factor(time))

mod0<-lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
          method='REML', na.action=na.exclude, data=dados)

plot(mod0) # problem on residuals

mod_site_pre3 <- lme(basal_area ~ treatment*time, random = ~1|ftime/ID, 
                 method='REML', na.action=na.exclude, 
                 data=dados,
                 weights = varPower()) # Power of the variance covariate
plot(mod_site_pre3) 

anova(mod_site_pre3, mod0)
# Compare the growth of planted non-pioneer trees (NAT_SEC) AFTER harvesting:
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time>55) %>%
  mutate(ID = plot) 

mod_post3 <- lme(basal_area ~ treatment, random= ~ 1|ID,
                 weights = varExp(form=~time),
                 correlation=corAR1(), na.action =na.omit, 
                 data=dados)
plot(mod_post3)
summary(mod_post3, ddf = "Kenward-Roger")
anova(mod_post3)

## Summary of BEFORE/AFTER harvesting
# Site 1 - Before
anova.lme(mod_site_pre1)
fixef(mod_site_pre1) # m2.ha-1
summary(mod_site_pre1)
intervals(mod_site_pre1, which='fixed')
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot,
         ftime = as.factor(time)) %>% filter(!is.na(basal_area))
lsmeans::lsmeans(mod_site_pre1, ~ treatment|time, trend = "time", type="response")

# Site 3 - Before
anova.lme(mod_site3)
fixef(mod_site_pre3) # m2.ha-1 
summary(mod_site_pre3)
intervals(mod_site_pre3, which='fixed')
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time<70) %>%
  mutate(ID = plot,ftime=as.factor(time))
lsmeans::lsmeans(mod_site_pre3, ~ treatment|time, trend = "time", type="response")

# Site 1 - After
fixef(mod_post1) # m2.ha-1 
summary(mod_post1)
anova(mod_post1)
intervals(mod_post1, which='fixed')
dados <- ARA_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>57) %>% filter(!is.na(basal_area)) %>%
  mutate(ID = plot) 
lsmeans::lsmeans(mod_post1, "treatment")

# Site 3 - After
dados <- OCT_LONG %>% filter(tipo=='NAT_SEC') %>% filter(time>55) %>%
  mutate(ID = plot) 
lsmeans::lsmeans(mod_post3, "treatment")
fixef(mod_post3) # m2.ha-1 
summary(mod_post3)
anova(mod_post3)
intervals(mod_post3, which='fixed')

## Basal Area Increment (BAI) for non-pioneer trees
# Site 1
ARA_BAI<-ARA_LONG %>% filter(tipo=='NAT_SEC') %>% 
  filter(time>55) %>% filter(!is.na(basal_area)) %>% ungroup() %>%
  mutate(ID = plot, time=as.factor(time)) %>% 
  dplyr::select(treatment, ID, basal_area, time) %>%
  spread(time, basal_area) %>% mutate(BAI = `83`-`58`)
ARA_BAI %>% with(t.test(BAI~treatment))
ggplot(ARA_BAI,aes(BAI))+
  geom_density()+facet_grid(.~treatment)

ARA_BAI %>%  mutate(Final = `83`) %>% 
  #filter(treatment=="NNW") %>% with(shapiro.test(BAI))
  #filter(treatment=="EUC") %>% with(shapiro.test(BAI))
  with(t.test(Final~treatment))

# Site 3
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

OCT_BAI$Final<-OCT_BAI$`83`
OCT_BAI$plant <- ifelse(OCT_BAI$treatment=="Only_Nat", "Nat", "Euc")
shapiro.test(OCT_BAI$`83`[OCT_BAI$plant=="Euc"])
shapiro.test(OCT_BAI$`83`[OCT_BAI$plant=="Nat"])
OCT_BAI %>% with(t.test(Final~plant))
mean(OCT_BAI$`83`[OCT_BAI$plant=="Nat"])/mean(OCT_BAI$`83`[OCT_BAI$plant=="Euc"])
sd(OCT_BAI$`83`[OCT_BAI$plant=="Nat"])
sd(OCT_BAI$`83`[OCT_BAI$plant=="Euc"])

# END