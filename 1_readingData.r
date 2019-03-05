## ARACRUZ: Harvested 57 months - Site 1
ARA <- read.csv("1_ARA_BasalArea_Dynamic.csv", as.is = T) %>% 
  dplyr::select(plot,tree, spp, SPP, block, treatment, wood,
                g20m2, g39m2, g52m2, g58m2) %>% 
  mutate(wood = ifelse(wood=="INI", "INI", "SEC"),
         treatment = ifelse(treatment!='NNW','EUC', "NNW"),
         SPP = ifelse(SPP!='EUC', 'NAT', 'EUC'))

ARA_83<-read.csv("1_ARA_BasalArea_Dynamic83.csv") %>% 
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
  mutate(time = as.numeric(time),
         time = ifelse(time==1, 20, 
                       ifelse(time==2,39,
                              ifelse(time==3,52,
                                     ifelse(time==4,58,83))))) %>% 
  unite(tipo, c("SPP","wood")) 

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

## IGRAPIUNA: Harvested 53 months - Site 3
OCT <- read.csv("1_IGR_BasalArea_Dynamic.csv", as.is=T) %>%
  dplyr::select(plot,tree, SPP,block, treatment, wood,
                g31m2, g45m2,g53m2, g60m2) %>%
  mutate(wood = ifelse(wood!='INI','SEC', 'INI'),
         SPP = ifelse(SPP!='EUC','NAT','EUC'),
         treatment = ifelse(treatment=='NNW', 'Only_Nat',
                            ifelse(treatment=='NEW','Euc_NonHarvest',
                                   'Euc_Harvest')))

OCT_83 <- read.csv("1_IGR_BasalArea_Dynamic83.csv", as.is=T) %>%
  mutate(g83m2= ((soma_cap^2)/(4*pi))*0.0001, # circunf (cm) to basal area (m2)
         tree = treeposition) %>%  # names changed 
  dplyr::select(plot, tree,  g83m2)

OCTtotal <- left_join(OCT, OCT_83, by=c('plot', 'tree'))

OCT_long <- OCTtotal %>% group_by(plot,treatment, block, wood, SPP) %>%
  gather(time, basal_area,c(g31m2, g45m2,g53m2, g60m2, g83m2), factor_key = T) %>%
  mutate(time = as.numeric(time),
         time = ifelse(time==1, 31, 
                       ifelse(time==2,45,
                              ifelse(time==3,53,
                                     ifelse(time==4,60,83)))))%>% 
  unite(tipo, c("SPP","wood"))

OCT_long %>% filter(!is.na(basal_area)) %>% with(table(plot,time))
OCT_long %>% group_by(treatment,  tipo, time)%>%
  dplyr::summarise(Girth_min = as.numeric(mean_se(basal_area)[2]),
                   Girth_med = as.numeric(mean_se(basal_area)[1]),
                   Girth_max = as.numeric(mean_se(basal_area)[3])) 
rm(OCT,OCT_83,OCTtotal)

OCT_LONG <- OCT_long %>% filter(!is.na(basal_area)) %>%
  group_by(time,plot,treatment, tipo) %>% 
  summarise(basal_area = (suma(basal_area)*10000)/390) #plot size = 2,160 m
