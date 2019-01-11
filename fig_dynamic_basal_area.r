if(!require(tidyverse)){install.packages("tidyverse")}  
if(!require(readxl)){install.packages("readxl")}       # read_excel

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
ARAtotal %>%  filter(SPP=='EUC') %>% filter(!is.na(g83m2)) %>%
  mutate(dif=g52m2-g83m2) %>% filter(dif>0)%>%
  with(table(block,plot)) 

ARA_long <- ARAtotal %>% group_by(plot,treatment, block, wood, SPP) %>%
  gather(time, basal_area,c(g20m2, g39m2, g52m2, g58m2, g83m2), factor_key = T) %>%
  mutate(time = as.numeric(time)) %>% unite(tipo, c("SPP","wood"))
ARA_long$time[ARA_long$time==1]<-20
ARA_long$time[ARA_long$time==2]<-39
ARA_long$time[ARA_long$time==3]<-52
ARA_long$time[ARA_long$time==4]<-58
ARA_long$time[ARA_long$time==5]<-83

# Retirando outlier
ARA_long<-ARA_long %>%filter(treatment !='EUC'|tipo!='NAT_INI') %>%
  droplevels()

ARA_long  %>% filter(!is.na(basal_area))%>% 
  group_by(treatment,  tipo, time) %>%
  dplyr::summarise(Girth_min = as.numeric(mean_se(basal_area)[2]),
                   Girth_med = as.numeric(mean_se(basal_area)[1]),
                   Girth_max = as.numeric(mean_se(basal_area)[3])) 

ARA_long %>% filter(!is.na(basal_area)) %>%
  ggplot(aes(x=time, y=basal_area, group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  #geom_point(aes(color=tipo),size=1)+
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()
ggsave('ARACRUZ_BasalArea_dynamics.jpg')

ARA_long %>% filter(!is.na(basal_area)) %>% 
  mutate(grafo=ifelse(tipo=="NAT_SEC", "Non_pioneer","Pioneer"))%>%
  ggplot(aes(x=time, y=basal_area, group=plot)) +
  geom_point(aes(color=tipo, group=time))+
  geom_smooth(method="lm", se=FALSE, aes(group=tipo, color=tipo))+
  facet_grid(grafo~treatment)+theme_bw()
ggsave('ARACRUZ_BasalArea_linear.jpg')


#############################################################################
# Dynamic in IGRAPIUNA
OCT <- read_xlsx("Dynamic_Basal_Area_ARA_IGR.xlsx",sheet = "IGR") %>%
  dplyr::select(plot,tree, SPP,block, treatment, wood,
                g31m2, g45m2,g53m2, g60m2)
OCT$wood[OCT$wood!='INI']<-'SEC'
OCT$treatment[OCT$treatment!='NNW']<-'EUC'
OCT$SPP[OCT$SPP!='EUC']<-'NAT'

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
OCTtotal<- subset(OCTtotal, !(plot %in% c(1,8,15,19,22,27)))

OCT_long <- OCTtotal %>% group_by(plot,treatment, block, wood, SPP) %>%
  gather(time, basal_area,c(g31m2, g45m2,g53m2, g60m2, g83m2), factor_key = T) %>%
  mutate(time = as.numeric(time)) %>% unite(tipo, c("SPP","wood"))
OCT_long$time[OCT_long$time==1]<-31
OCT_long$time[OCT_long$time==2]<-45
OCT_long$time[OCT_long$time==3]<-53
OCT_long$time[OCT_long$time==4]<-60
OCT_long$time[OCT_long$time==5]<-83
 
OCT_long %>% filter(!is.na(basal_area)) %>% with(table(time))
OCT_long %>% group_by(treatment,  tipo, time)%>%
  dplyr::summarise(Girth_min = as.numeric(mean_se(basal_area)[2]),
                   Girth_med = as.numeric(mean_se(basal_area)[1]),
                   Girth_max = as.numeric(mean_se(basal_area)[3])) 

OCT_long %>% filter(!is.na(basal_area)) %>%
  ggplot(aes(x=time,y=basal_area,group=tipo)) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha=0.3, 
               aes(fill = tipo, color = tipo)) +
  stat_summary(geom = "line", fun.y = mean, aes(color=tipo)) +
  facet_grid(.~treatment)+theme_bw()
ggsave('OCT_BasalArea_dynamics.jpg')

OCT_long %>% filter(!is.na(basal_area)) %>% 
  mutate(grafo=ifelse(tipo=="NAT_SEC", "Non_pioneer","Pioneer")) %>%
ggplot(aes(x=time,y=basal_area,group=plot)) +
  geom_point(aes(color=tipo, group=time))+
  geom_smooth(method="lm", se=FALSE, aes(color=tipo, group=tipo))+
  facet_grid(grafo~treatment)+theme_bw()
ggsave('OCT_BasalArea_linear.jpg')

