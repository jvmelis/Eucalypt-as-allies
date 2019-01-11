wd<-"~/ARTIGOS - Meus/0 MS - Eucalipto e regenerantes"
setwd(wd)
rm(list=ls())

if(!require(tidyverse)){install.packages("tidyverse")}  
if(!require(readxl)){install.packages("readxl")}       # read_excel
if(!require(iNEXT)){install.packages("iNEXT")}         # rarefaction curves
if(!require(vegan)){install.packages("vegan")}         # diversity index

## rarefaction curve
PA<-as.data.frame(read_xlsx("Regeneracao_IGR_ARA.xlsx",sheet="rarefa"))
rownames(PA)<-PA$spp
PA <- PA %>% select(-one_of('spp')) %>%
  mutate(IGR_EUC=`IGR_NE_Colheita`+`IGR_NE_semcolheita`,
         ARA_EUCA = ARA_EUC + ARA_NED)%>% 
  select(IGR_EUC,IGR_NAT,ARA_NAT,ARA_EUCA)
colSums(PA)
df <- PA %>% iNEXT(datatype="abundance") %>% fortify(type=1)
df.point <- df[which(df$method=="observed"),]
df <- df %>% separate(site,c('area','trata')) %>% 
  mutate(site=paste(area,trata,sep="_")) %>% 
  filter(method!="extrapolated")

## Plot
### IGR

IGR <- df %>% filter(area=='IGR') %>%
  ggplot(aes(x=x, y=y, color=site))+
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr, 
                  fill=site, color=NULL), alpha=0.4)+
  geom_line(aes(linetype=method)) +
  scale_color_manual(values =c("darkblue","red"))+
  scale_fill_manual(values =c("darkblue","red"),
                    labels = c("IGR_NAT" = "Igrapiuna: Only native species",
                               "IGR_EUC" = "Igrapiuna: Eucalypt + Native species"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,50),
                     breaks = c(0, 15, 30, 45))+
  guides(color= F, 
         fill=guide_legend(override.aes = list(alpha=1)),
         shape = F, 
         linetype=F)+
  labs(shape ="", color="",linetype="",fill="")+theme_bw()+
  xlab("Total individuals")+ ylab("Species Richness") + 
  geom_point(shape=16, data=df.point[1:2,]) +
  theme(legend.position = c(0.5,.245), # Ho- x Ve|
          legend.key = element_rect(color = NA, fill =NA, 
                                    linetype='solid'),
          legend.key.height = unit(2.7, "mm"), 
          legend.key.width = unit(2.7, "mm"),
          legend.text=element_text(size=9), # tamanho 2
          legend.background = element_blank(),
          plot.title = element_text(hjust = -.4, size=10),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size=9,color='black'), # tamanho 1
          axis.title = element_text(size=10), # tamanho 1
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
IGR
### ARA
ARA <- df %>% filter(area!='IGR') %>%
  ggplot(aes(x=x, y=y, color=site))+
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr, 
                  fill=site, color=NULL), alpha=0.4)+
  geom_line(aes(linetype=method)) +
  scale_color_manual(values =c("blue","darkred"))+
  scale_fill_manual(values =c("blue","darkred"),
                    labels = c("ARA_EUCA" = "Aracruz: Eucalypt + Native species",
                               "ARA_NAT" = "Aracruz: Only native species"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,25),
                     breaks = c(0, 5,10, 15,20))+
  guides(color= F, 
         fill=guide_legend(override.aes = list(alpha=1)),
         shape = F, 
         linetype=F)+
  labs(shape ="", color="",linetype="",fill="")+theme_bw()+
  xlab("Total individuals")+ ylab("Species Richness") + 
  geom_point(shape=16, data=df.point[3:4,]) +
  theme(legend.position = c(0.5,.15), # Ho- x Ve|
        legend.key = element_rect(color = NA, fill =NA, 
                                  linetype='solid'),
        legend.key.height = unit(2.7, "mm"), 
        legend.key.width = unit(2.7, "mm"),
        legend.text=element_text(size=9), # tamanho 2
        legend.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size=9,color='black'), # tamanho 1
        axis.title = element_text(size=10), # tamanho 1
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ARA

# both
if(!require(cowplot)){install.packages('cowplot')}
(p_total<-plot_grid(ARA, IGR, align = "hv",ncol=1,nrow=2))
ggsave("Fig_rarefa.jpg", p_total,units="in",width = 2.3, height = 6.6, dpi=1200)
dev.off()

## Diversity
(H<-diversity(t(PA)))
fisher.alpha(t(PA))
H/specnumber(t(PA)) # Pielou evenness

# fazer similaridade de diversidade
