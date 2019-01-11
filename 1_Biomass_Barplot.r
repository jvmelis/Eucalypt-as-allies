setwd(choose.dir())
rm(list=ls())

if(!require(tidyverse)){install.packages("tidyverse")}  # dplyr, ggplot, pipes,etc
if(!require(readxl)){install.packages("readxl")}       # read_excel

dados<-read_excel("Biomassa_3_sites.xlsx",sheet="Planilha6",na="") 

############################# Pairwise compairson #################################
dados %>% mutate(local=paste(site,treatment,sep="_") ) %>%
  filter(site=='ara') %>% filter(treatment!="EUC")%>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>% 
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC) %>%
  #with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"])) # site 1
  #with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))    # sec nao signif
  ggplot(aes(x=SEC,group=treatment,fill=treatment))+geom_density()+scale_x_continuous(limits=c(2,7))+ggtitle("Aracruz - Site 1")

dados %>% mutate(local=paste(site,treatment,sep="_") ) %>%
  filter(site=='muc') %>% filter(treatment!="EUC")%>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>%
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC) %>%
  #with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"])) # site 2
  #with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))    # p=0.01 
  ggplot(aes(x=SEC,group=treatment,fill=treatment))+geom_density()+scale_x_continuous(limits=c(2,7))+ggtitle("Mucuri - Site 2")

dados %>% mutate(local=paste(site,treatment,sep="_") ) %>%
  filter(site=='oct') %>% filter(treatment!="EUC") %>% 
  dplyr::select(block,treatment,sppbiomass,Mg_ha) %>%
  spread(sppbiomass,Mg_ha,fill=0) %>% mutate(soma=EUCALYPTUS+PIONEER+SEC)%>%
  #with(t.test(soma[treatment=="NEW"],soma[treatment=="NNW"])) # site 3
  #with(t.test(SEC[treatment=="NEW"],SEC[treatment=="NNW"]))
  ggplot(aes(x=SEC,group=treatment,fill=treatment))+geom_density()+scale_x_continuous(limits=c(0,12))+ggtitle("Site 3 - Igrapiuna")

###
df<-dados %>% dplyr::select(site, sppbiomass, treatment, Mg_ha) %>% 
  filter(treatment!="EUC")  %>%
  group_by(site,treatment, sppbiomass)%>% 
  summarise(media=mean(Mg_ha, na.rm=T),
            ymax= as.numeric(paste(mean_se(Mg_ha)[3])),
            ymin= as.numeric(paste(mean_se(Mg_ha)[2]))) %>% 
  ungroup()%>% mutate(local=paste(site,treatment,sep="_")) %>%mutate(local=factor(local))

df$ymax[df$sppbiomass=='EUCALYPTUS']<-df$ymax[df$sppbiomass=='EUCALYPTUS']+df$ymax[df$sppbiomass=='SEC'&df$treatment!='NNW']
df$ymin[df$sppbiomass=='EUCALYPTUS']<-df$ymin[df$sppbiomass=='EUCALYPTUS']+df$ymin[df$sppbiomass=='SEC'&df$treatment!='NNW']
df$ymax[df$sppbiomass=='PIONEER']<-df$ymax[df$sppbiomass=='PIONEER']+df$ymax[df$sppbiomass=='SEC'&df$treatment=='NNW']
df$ymin[df$sppbiomass=='PIONEER']<-df$ymin[df$sppbiomass=='PIONEER']+df$ymin[df$sppbiomass=='SEC'&df$treatment=='NNW']

##################### Grafico 1 ######################
p1<-ggplot(df, aes(x=local, y=media, fill=sppbiomass)) + 
  geom_bar(stat="identity",size=.5,width = 0.7, color="black") + 
  geom_errorbar(width=.3,size=.5, aes(ymin=(ymin), ymax=(ymax)))+
  scale_x_discrete(labels=c(
                  "ara_NEW"="EUC\nN-PIO","ara_NNW"="PIO\nN-PIO",     # NS
                  "muc_NEW"="EUC\nN-PIO","muc_NNW"="PIO\nN-PIO",     # p=0.01
                  "oct_NEW"="EUC\nN-PIO", "oct_NNW"="PIO\nN-PIO"))+  # p=0.004
  scale_fill_manual(values =c("blue","orange","red2"), 
                    labels = c("EUC", "PIO", "N-PIO"))+
  xlab("")+ ylab(expression(paste("Total Biomass (Mg ",ha^-1,")")))+
  scale_y_continuous(breaks=c(0,20,40,70,120,170),
                     labels=c(0,20,50,100,150,200),
                     expand = c(0,0),limits=c(0,180))+
  theme_classic()+
  theme(legend.justification=c(0.5, 0),
          axis.title.y = element_text(margin = margin(t = -1, 
                                                      r = -1, 
                                                      b = -1, 
                                                      l = -1)),
          axis.line = element_line(size = .5, color = 'black'),
          legend.margin=margin(0,0,-2,0),
          axis.ticks.y = element_blank(),
          legend.box.margin=margin(5,-3,-1,-5),
          axis.title = element_text(size=10),
          axis.text.x = element_text(size=8, color = "black"), 
          axis.text.y = element_text(size=8, color = "black"),
          legend.position = "top",
          legend.text = element_text(size=8),
          legend.title=element_blank())+        
   guides(fill=guide_legend(nrow = 1, keywidth = 1, keyheight = 1))+
  annotate(geom="text", x=c(4,6), y=80, hjust=0, label="*", size=4)+
  annotate(geom="text", x=2, y=130, hjust=0,size=3, label="Site 1")+
  annotate(geom="text", x=4, y=130, size=3, hjust=0,label="Site 2")+
  annotate(geom="text", x=6, y=130, size=3, hjust=0,label="Site 3")+
  geom_vline(xintercept = c(2.5,4.5), size=.5, linetype=3)+coord_flip()

p1<-p1+annotate("segment", x = 0.55, xend = 1.45, y = 21.5, yend = 23, colour = "black")+
  annotate("segment", x = 2.55, xend = 3.45, y = 21.5, yend = 23, colour = "black")+
  annotate("segment", x = 4.55, xend = 5.45, y = 21.5, yend = 23, colour = "black")+
  annotate("segment", x = 0.55, xend = 1.45, y = 23.5, yend = 25, colour = "black")+
  annotate("segment", x = 2.55, xend = 3.45, y = 23.5, yend = 25, colour = "black")+
  annotate("segment", x = 4.55, xend = 5.45, y = 23.5, yend = 25, colour = "black")
p1
# 
ggsave("Biomass_Experiments.jpg",
       p1,units="mm",width = 58, height = 75, dpi=1200)
