library(tidyverse)
library(grid)
library(gridExtra)
library(betareg)
library(rstantools)
library(broom)
library(mgcv)
library(forcats)
library(viridis)
library(trend)
library(ggthemes)
library(betareg)
library(ggridges)
library(tidybayes)
library(tidyselect)
library(scales)

# ----------------------------------------------------------------------------------------------------
#  Compile all recoveries for LPW
# ----------------------------------------------------------------------------------------------------

#  Run a recovery query in RMIS. 
#  Use "Recoveries by tag code", pasting in the tag codes from the Indicators_2_with_dupes.xlsx sheet.
#  Select Report: "Comma-separated values custom file (CSV)" 
#  Select fields:
#  Run year
#  Tag code
#  Estimated number
#  Brood year
#  Stock location name
#  Length
#  Sex
#  Fishery
#  Recovery location code
#  Recovery locatioh name
#  Sampling site
#  Sampling Agency

#  Save this query as "All_Recovery_Estimated_Number.txt"

recoverdat <- read_csv("Data/All_Recovery_Estimated_Number.txt") %>%   
  rename(stock=stock_location_name) %>% 
  mutate(age=(run_year-brood_year),
         recovery=ifelse(recovery_location_code=="1F1NE109 10","LPW","Marine_Harvest"),
         estimated_number=ifelse(recovery=="LPW",1,estimated_number)) %>% 
  filter(stock!="KETA R 101-30" & brood_year>=1981 & brood_year<=2013) %>% 
  mutate(stock=ifelse(stock=="CHICKAMIN R 101-71","Chickamin","Unuk"),
         fage=case_when(age==2~"1.0",
                        age==3~"1.1",
                        age==4~"1.2",
                        age==5~"1.3",
                        age==6~"1.4",
                        age==7~"1.5"))

#recoverdat %>% 
#  group_by(brood_year,stock,age) %>% 
#  summarise(n=n(),
#            minlength=min(length,na.rm=TRUE),
#            maxlength=max(length,na.rm=TRUE),
#            meanlength=mean(length,na.rm=TRUE)) %>% 
#  write.csv("Recoveries_BY_Stock_Age.csv")

#recoverdat %>% 
#  group_by(brood_year,stock,age,sex) %>% 
#  summarise(n=n(),
#            minlength=min(length,na.rm=TRUE),
#            maxlength=max(length,na.rm=TRUE),
#            meanlength=mean(length,na.rm=TRUE)) %>% 
#  left_join(recoverdat %>% 
#              group_by(brood_year,stock,sex) %>% 
#              summarise(total=n())) %>% 
#  mutate(prop=n/total) %>% 
#  write.csv("Recoveries_BY_Stock_Age_Sex_prop.csv")

#recoverdat %>% 
#  group_by(brood_year,stock,age,fishery) %>% 
#  summarise(n=n(),
#            minlength=min(length,na.rm=TRUE),
#            maxlength=max(length,na.rm=TRUE),
#            meanlength=mean(length,na.rm=TRUE)) %>% 
#  write.csv("Recoveries_BY_Stock_Age_fishery.csv")


# ----------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------
#  Compile all releases for LPW
# ----------------------------------------------------------------------------------------------------

#  Query Releases > Tagged Releases
#  Paste in the tag codes from the Indicators_2_with_dupes.xlsx sheet.
#  Select Report: "Comma-separated values custom file (CSV)" 
#  Select fields:
#  Tag Code Or Release ID
#  Brood Year
#  CWT 1st Mark Count
#  Stock Location Name

#  We truncated the data at 1981 to remove potential hatchery effects. 
releasedat <- read_csv("Data/All_releases_1976_through_2019_byYear_byStock.txt") %>% 
  rename(stock=stock_location_name) %>% 
  filter(stock!="KETA R 101-30" & brood_year>=1981) %>% 
  mutate(stock=ifelse(stock=="CHICKAMIN R 101-71","Chickamin","Unuk"))

release <- releasedat %>% 
  group_by(brood_year,stock) %>% 
  summarise(released=sum(cwt_1st_mark_count)) 

#release %>% 
#  write.csv("Releases_BY_stock.csv")

# ----------------------------------------------------------------------------------------------------

#  Survivals are calculated by dividing the "estimated_number" by the cwt_1st_mark_count

surv <- recoverdat %>% 
  group_by(stock,brood_year) %>% 
  summarise(est=sum(estimated_number,na.rm=TRUE)) %>% 
  inner_join(release) %>% 
  group_by(brood_year,stock) %>% 
  summarise(survival=100*(est/released),
            released) %>% 
  data.frame

# ----------------------------------------------------------------------------------------------------
#  Create Table 1 for manuscript
# ----------------------------------------------------------------------------------------------------

recoverdat %>% 
  mutate(stock=ifelse(stock=="Chickamin","Chick",stock)) %>% 
  group_by(brood_year,stock,recovery) %>% 
  summarise(n=sum(estimated_number,na.rm=TRUE)) %>% 
  pivot_wider(values_from=n,
              names_from = c(stock,recovery)) %>% 
  inner_join(recoverdat %>% 
               mutate(stock=ifelse(stock=="Chickamin","Chick",stock),
                      sex=case_when(
                 !is.na(sex) & sex=="M"~"Male",
                 !is.na(sex) & sex=="F"~"Female"),
                 sex=ifelse(recovery!="LPW",NA,sex)) %>% 
               group_by(brood_year,stock,sex) %>% 
               tally() %>% 
               pivot_wider(values_from=n,
                           names_from = c(stock,sex))) %>% 
  data.frame %>% 
  mutate(Chick_Female_Perc=100*round(Chick_Female/Chick_LPW,3),
         Chick_Male_Perc=100*round(Chick_Male/Chick_LPW,3),
         Unuk_Female_Perc=100*round(Unuk_Female/Unuk_LPW,3),
         Unuk_Male_Perc=100*round(Unuk_Male/Unuk_LPW,3),
         Unuk_Total=Unuk_Marine_Harvest+Unuk_LPW,
         Chick_Total=Chick_Marine_Harvest+Chick_LPW) %>% 
  inner_join(surv %>% 
               mutate(stock=ifelse(stock=="Chickamin","Chick",stock),
                             survival=round(survival,1)) %>% 
               pivot_wider(values_from= c(survival,released),
                           names_from = c(stock))) %>% 
  select(`Brood Year`=brood_year,
         `Unuk_Smolts Released`=released_Unuk,
         `Unuk_Total Recoveries`=Unuk_Total,
         `Unuk_Marine Harvest`=Unuk_Marine_Harvest,
         `Unuk_LPW Recoveries`=Unuk_LPW,
         `Unuk_Female Proportion`=Unuk_Female_Perc,
         `Unuk_Male Proportion`=Unuk_Male_Perc,
         `Unuk_Survival`=survival_Unuk,
         `Chick_Smolts Released`=released_Chick,
         `Chick_Total Recoveries`=Chick_Total,
         `Chick_Marine Harvest`=Chick_Marine_Harvest,
         `Chick_LPW Recoveries`=Chick_LPW,
         `Chick_Female Proportion`=Chick_Female_Perc,
         `Chick_Male Proportion`=Chick_Male_Perc,
         `Chick_Survival`=survival_Chick) %>% 
  write.csv("Manuscript/Table1_summary_updated.csv",row.names=FALSE)

recoverdat %>% 
  group_by(brood_year,stock,recovery) %>% 
  tally() %>% 
  pivot_wider(values_from=n,
              names_from = c(stock,recovery)) %>% 
  inner_join(recoverdat %>% 
               mutate(sex=case_when(
                 !is.na(sex) & sex=="M"~"Male",
                 !is.na(sex) & sex=="F"~"Female")) %>% 
               group_by(brood_year,stock,sex) %>% 
               tally() %>% 
               pivot_wider(values_from=n,
                           names_from = c(stock,sex))) %>% 
  inner_join(surv %>% 
               mutate(survival=round(survival,1)) %>% 
               pivot_wider(values_from= c(survival,released),
                           names_from = c(stock))) %>% 
  select(Chickamin_LPW,`Chickamin_Non-LPW`,Chickamin_Female,Chickamin_Male,Chickamin_NA,survival_Chickamin,released_Chickamin,
         Unuk_LPW,`Unuk_Non-LPW`,Unuk_Female,Unuk_Male,Unuk_NA,survival_Unuk,released_Unuk) %>% 
  write.csv("Manuscript/Table1_summary_updated.csv",row.names=FALSE)
  
# ----------------------------------------------------------------------------------------------------
#  Create Figure 2 for manuscript
#  Summary of recoveries
# ----------------------------------------------------------------------------------------------------

png("Manuscript/Figure_2_harvest_summary_stacked.png",w=6.5,h=4.5,units="in",res=300)
recoverdat %>% 
  mutate(recovery=ifelse(recovery=="LPW","Little Port Walter","Marine Harvest"),
         stock=fct_relevel(stock,"Unuk")) %>% 
  group_by(brood_year,stock,recovery) %>% 
  tally() %>% 
  ggplot(aes(brood_year,n,fill=factor(recovery))) +
  geom_bar(stat="identity") +
  facet_wrap(~stock,ncol=2) + 
  scale_fill_manual(values=c("blue","red")) +
  theme_bw() + 
  theme(legend.position=c(0.75,0.8)) + 
  guides(fill=guide_legend(title="")) + 
  xlab("Brood Year") + 
  ylab("Number of Recoveries")
dev.off()



# ----------------------------------------------------------------------------------------------------
#  Create Figure 3 for manuscript
#  Survivals for Chickamin and Unuk. 
# ----------------------------------------------------------------------------------------------------

mylab <- seq(0,0.1,by=0.02)

png(file="Manuscript/Figure_3_Survival_brood_stock.png",w=6.5,h=4.5,units="in",res=300)
surv %>% 
  mutate(stock=fct_rev(factor(stock)),
         survival=survival/100) %>% 
  ggplot(aes(brood_year,survival,color=stock)) + 
  geom_point() +
  facet_wrap(~stock,ncol=2) + 
  theme_bw() + 
  geom_smooth(method="betareg",
              aes(color=stock),
              linetype=2,
              se=F,
              size=0.65) +
  stat_smooth(method="gam",
              formula=y~s(x),
              method.args = list(family = betar(link="logit")),
              linetype=1,
              se=F) +
  xlab("Brood Year") + 
  ylab("Survival (%)") + 
  scale_color_manual(values=c("blue","red")) +
  theme(legend.position="none",
        strip.text=element_text(size=12)) + 
  scale_y_continuous(breaks=mylab,labels=mylab*100)
dev.off() 

# ----------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------
#  Create Figure 4 for manuscript
#  Survivals for Chickamin and Unuk by age
# ----------------------------------------------------------------------------------------------------

recovery <- recoverdat %>% 
  group_by(brood_year,fage,stock) %>% 
  summarise(est=sum(estimated_number,na.rm=TRUE),
            age=age[1])

survbyagestock <- recovery %>% 
  inner_join(release) %>% 
  mutate(survivalperc=(est/released)) %>% 
  filter(age<7)

mylab <- seq(0,0.05,by=0.01)

png(file="Manuscript/Figure_4_Survival_age_brood_stock.png",w=6.5,h=6.5,units="in",res=300)
survbyagestock %>% 
  filter(fage%in%c("1.2","1.3","1.4")) %>% 
  ungroup %>% 
  mutate(survival=survivalperc,
         stockage=paste(stock,fage,sep=""),
         stockage=fct_relevel(stockage,
                              "Unuk 1.2","Unuk 1.3","Unuk 1.4"),
         fage=paste0("Age ",fage)) %>% 
  ggplot(aes(brood_year,survival,color=stock)) + 
  geom_point(size=1) +
  facet_wrap(~fage,ncol=1,dir="v") + 
  theme_bw() + 
  geom_smooth(method="betareg",
              aes(color=stock),
              linetype=2,
              se=F,
              size=0.45) +
  stat_smooth(method="gam",
              formula=y~s(x),
              method.args = list(family = betar(link="logit")),
              linetype=1,
              se=F) + 
  xlab("Brood Year") + 
  ylab("Survival (%)") + 
  scale_color_manual(values=c("red","blue")) +
  theme(legend.position="none") + 
  scale_y_continuous(breaks=mylab,labels=mylab*100)
dev.off()
# ----------------------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
#  Figures 5 and 6. 
#  Age composition figures
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

#  This version plots all 6 panels.
max.year <- 2013
myageplot6 <- function(mystock){
  p <- recoverdat %>% 
    filter(!is.na(sex) & fage%in%c("1.2","1.3","1.4")) %>% 
    mutate(sex=fct_recode(sex,"Male"="M",
                          "Female"="F")) %>% 
    group_by(sex,brood_year,stock) %>% 
    summarise(`1.2`=length(age[age==4])/n(),
              `1.3`=length(age[age==5])/n(),
              `1.4`=length(age[age==6])/n()) %>% 
    gather(age,value,-brood_year,-sex,-stock) %>% 
    data.frame %>% 
    right_join(expand.grid(brood_year=unique(recoverdat$brood_year),
                           sex=c("Male","Female"),
                           age=c("1.2","1.3","1.4"),
                           stock=c("Unuk","Chickamin"))) %>% 
    mutate(agesex=paste(sex,age,sep="; "),
           agesex=fct_expand(factor(agesex),"Female; 1.2"),
           agesex=fct_relevel(agesex,"Male; 1.2","Male; 1.3","Male; 1.4","Female; 1.2"),
           value=ifelse(value==0,0.00001,value),
           value=ifelse(value==1,0.99999,value)) %>% 
    filter(stock==mystock) %>% 
    ggplot(aes(brood_year,value)) + 
    stat_smooth(method="gam",
                formula=y~s(x),
                method.args = list(family = betar(link="logit")),
                linetype=1,
                se=T) +
    geom_line() + 
    geom_point() + 
    facet_wrap(~agesex,ncol=2,dir="v",drop=FALSE) + 
    theme_bw() + 
    xlab("Brood Year") + 
    ylab("Proportion of return") + 
    scale_fill_viridis(discrete=TRUE,name="") + 
    scale_x_continuous(breaks=seq(1981,max.year),
                       labels=c("1981","","","","1985","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013"),
                       expand=c(0.025,0.025)) + 
    ylim(0,1) +
    theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
          panel.grid.minor = element_blank())
}

unuk <- myageplot6("Unuk")
ggsave("Manuscript/Figure5_Proportion_at_age_Unuk6.png",grid.draw(unuk),width=6,height=6,units="in",dpi=300)
rm(unuk)
graphics.off()

chickamin <- myageplot6("Chickamin")
ggsave("Manuscript/Figure6_Proportion_at_age_Chickamin6.png",grid.draw(chickamin),width=6,height=6,units="in",dpi=300)
rm(chickamin)
graphics.off()


#  This is the older version, which omits the female 1.2 year old fish
max.year <- 2013
myageplot <- function(mystock){
  p <- recoverdat %>% 
    filter(!is.na(sex) & fage%in%c("1.2","1.3","1.4")) %>% 
    mutate(sex=fct_recode(sex,"Male"="M",
                          "Female"="F")) %>% 
    group_by(sex,brood_year,stock) %>% 
    summarise(`1.2`=length(age[age==4])/n(),
              `1.3`=length(age[age==5])/n(),
              `1.4`=length(age[age==6])/n()) %>% 
    gather(age,value,-brood_year,-sex,-stock) %>% 
    data.frame %>% 
    right_join(expand.grid(brood_year=unique(recoverdat$brood_year),
                           sex=c("Male","Female"),
                           age=c("1.2","1.3","1.4"),
                           stock=c("Unuk","Chickamin"))) %>% 
    mutate(agesex=paste(sex,age,sep="; "),
           agesex=fct_expand(factor(agesex),"Female; 1.2"),
           agesex=fct_relevel(agesex,"Male; 1.2","Male; 1.3","Male; 1.4","Female; 1.2"),
           value=ifelse(value==0,0.00001,value),
           value=ifelse(value==1,0.99999,value)) %>% 
    filter(stock==mystock) %>% 
    ggplot(aes(brood_year,value)) + 
    stat_smooth(method="gam",
                formula=y~s(x),
                method.args = list(family = betar(link="logit")),
                linetype=1,
                se=T) +
  geom_line() + 
    geom_point() + 
    facet_wrap(~agesex,ncol=2,dir="v",drop=FALSE) + 
    theme_bw() + 
    xlab("Brood Year") + 
    ylab("Proportion of return") + 
    scale_fill_viridis(discrete=TRUE,name="") + 
    scale_x_continuous(breaks=seq(1981,max.year),
                       labels=c("1981","","","","1985","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013"),
                       expand=c(0.025,0.025)) + 
    ylim(0,1) +
    theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
          panel.grid.minor = element_blank())
  
  g <- ggplotGrob(p)
  # get the grobs that must be removed
  rm_grobs <- g$layout$name %in% c("panel-2-2", "strip-t-2-1")
  # remove grobs
  g$grobs[rm_grobs] <- NULL
  g$layout <- g$layout[!rm_grobs, ]
  ## move axis closer to panel
  graphics.off()
  g$layout[g$layout$name == "axis-b-1-2", c("t", "b")] = c(9, 9)
  #  If you don't use scales=free_y in the facet, you'll want this.
  #g$layout[g$layout$name == "axis-l-2-1", c("l", "r")] = c(7, 7)
  #grid.newpage()
  #grid.draw(g)
  return(g)
}

unuk <- myageplot("Unuk")
ggsave("Manuscript/Figure5_Proportion_at_age_Unuk.png",grid.draw(unuk),width=6,height=6,units="in",dpi=300)
rm(unuk)
graphics.off()

chickamin <- myageplot("Chickamin")
ggsave("Manuscript/Figure6_Proportion_at_age_Chickamin.png",grid.draw(chickamin),width=6,height=6,units="in",dpi=300)
rm(chickamin)
graphics.off()

### ---------------------------------------------------
##  End Figures 5 and 6
### ---------------------------------------------------


# ---------------------------------------------------
#  Bayesian model - Figure 7
# ---------------------------------------------------

mydf <- rstan::extract(readRDS("Male_15000iter_delta99999_tree20.RDS"))$sd_pro %>% 
  data.frame %>% 
  gather(year,value) %>% 
  mutate(year=as.numeric(gsub("X","",year)),
         sex="Male") %>% 
  bind_rows(rstan::extract(readRDS("Female_3_ages_30000iter_delta_999999_tree20.RDS"))$sd_pro %>% 
              data.frame %>% 
              gather(year,value) %>% 
              mutate(year=as.numeric(gsub("X","",year)),
                     sex="Female"))
max.year <- 2013

p1 <- mydf %>% 
  filter(sex=="Male") %>% 
  ggplot() +
  geom_boxplot(aes(x=factor(year), y=value),outlier.shape = NA) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) + 
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1),limits = c(0,5)) +
  annotate("text",x=17,y=5,label="Male")

p2 <- mydf %>% 
  filter(sex=="Female") %>% 
  ggplot() +
  geom_boxplot(aes(x=factor(year), y=value),outlier.shape = NA) + 
  theme_bw() +
  scale_x_discrete(labels=c("1981","","","","1985","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013"),
                   expand=c(0.025,0.025)) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5),
        axis.title.y = element_blank()) + 
  xlab("Brood Year") +
  ylim(0,1.5) +
  annotate("text",x=17,y=1.5,label="Female") 

png("Manuscript/Figure_7_Deviance_boxplot.png",width=6,height=6,units="in",res=300)
grid.arrange(arrangeGrob(p1,p2,nrow = 2,
                         left = textGrob("Deviance of proportion of age-at-return", rot = 90, vjust = 1)))
dev.off()




yearsexmed <- mydf %>% 
  group_by(year,sex) %>% 
  summarise(mymed=median(value))

png("Manuscript/Figure_7_Deviance_density_scales_y_free.png",width=6,height=6,units="in",res=300)
mydf %>% 
  ggplot() +
  geom_vridgeline(aes(x=factor(year), y=value, width = ..density..),stat="ydensity", trim=FALSE, alpha = 0.85, scale = 0.4) + 
  geom_point(data=yearsexmed,aes(x=factor(year),y=mymed)) +
  scale_x_discrete(labels=as.character(1981:2013)) + 
  theme_bw() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5)) + 
  facet_wrap(~sex,scales="free_y",ncol=1) +
  ylab("Deviance of proportion of age-at-return") + 
  xlab("Brood Year") 
dev.off()


sddat <- mydf %>% 
  inner_join(data.frame(year=1:33,year2=1981:2013))

myprob <- seq(0.01,0.99,by=0.01)

#  I can't get scale_alpha_manual to work right so instead I use scale_fill_manual but create a vector of colors with an alpha value appended to the hex name for the color.
#  So I am looking at 99 different credible intervals, ranging from the 99% to 1%, and for each one, I have a different color, with the transparency equal to the value of the credible interval
#  (i.e., the 99% credible interval has alpha=0.99, the 80% CI has alpha=0.8, and so on)
myalpha <- as.vector((data.frame(x=1:99) %>% 
                        mutate(alpha=ifelse(x<10,paste0("#7F7F7F0",x),paste0("#7F7F7F",x))))$alpha)

#  Now plot the posterior mean of the sd_pro parameter, with a shaded gradient of credible interval values.
sddat %>% 
  ggplot(aes(x = year)) +
  stat_lineribbon(aes(x=year2,y = value),color="black",size=0.75) +
  #scale_fill_manual(values=c(myalpha),guide=FALSE) +
  facet_wrap(~sex,scales="free") + 
  ylab("Deviance of proportion of age-at-return") + 
  xlab("Year") + 
  theme_bw()



# ----------------------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
#  Set-up length regressions
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------

#  Define the maximum year in the data
max.year <- 2013

#  Create the age / sex combinations
p <- recoverdat %>% 
  filter(!is.na(sex) & fage%in%c("1.2","1.3","1.4")) %>% 
  mutate(sex=fct_recode(sex,"Male"="M",
                        "Female"="F"),
         agesex=paste(sex,fage,sep="; "),
         agesex=fct_expand(factor(agesex),"Female; 1.2"),
         agesex=fct_relevel(agesex,"Male; 1.2","Male; 1.3","Male; 1.4","Female; 1.2"))  #%>% 
  #filter(agesex!="Female; 1.2")

test <- mdat %>% 
  mutate(by2=ifelse(stock=="Chickamin",brood_year-0.15,brood_year+0.15)) %>% 
  ggplot(aes(by2,length,color=stock)) +
  geom_point(alpha=0.5,size=0.25) + 
  facet_wrap(~agesex,drop = FALSE,scales="free_y",ncol=2,dir="v") +
  stat_smooth(aes(color=stock),se=FALSE,method="gam",linetype=1,size=0.75,alpha=0.1) + 
  #stat_smooth(aes(color=stock),se=FALSE,method="lm",linetype=2,size=0.75,alpha=0.1) + 
  ylab("Length (mm)") + 
  xlab("Brood Year") + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
        legend.position="none",
        panel.grid=element_blank()) + 
  scale_x_continuous(breaks=seq(1981,max.year),
                     labels=c("1981","","","","1985","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013")) + 
  #scale_color_grey() +
  scale_color_manual(values=c("red","blue"))

png("Manuscript/Figure_8_Length_at_age_all_test.png",width=6,height=6,units="in",res=300);test;dev.off()



#  This is for scaling each facet to have a total y-axis range of 325mm
dummy <- p %>% 
  filter(length>510) %>% 
  group_by(brood_year,agesex,stock) %>% 
  summarise(length=mean(length,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(agesex) %>% 
  summarise(mymax=max(length),
            mymin=min(length),
            mydiff=mymax-mymin,
            mymid=mymin+mydiff/2,
            newmin=mymid-162.5,
            newmax=mymid+162.5) %>% 
  ungroup %>% 
  dplyr::select(agesex,newmin,newmax) %>% 
  gather(key,length,-agesex) %>% 
  dplyr::select(-key) %>% 
  mutate(brood_year=max.year,
         Stock="Unuk")

#  Add Chickamin into the figure
#  Note that one point with a small mean is driving the apparent upward trend in size.
#  If we filter lengths to be greater than 510, this outlier effect is removed.
plotdat <- p %>% 
  filter(length>510) %>% 
  group_by(brood_year,agesex,stock) %>% 
  summarise(length=mean(length,na.rm=TRUE)) %>% ungroup %>%
  rename(Stock=stock)

pgam <- ggplot(plotdat,aes(brood_year,length,color=Stock)) + 
  geom_point(size=0.5) + 
  geom_blank(data=dummy) +
  facet_wrap(~agesex,drop = FALSE,scales="free_y",ncol=2,dir="v") + 
  #facet_wrap(~agesex,drop = FALSE,ncol=2,dir="v") + 
  stat_smooth(data=subset(plotdat,agesex!="Female; 1.2"),aes(color=Stock),se=FALSE,method="gam",linetype=1,size=0.75,alpha=0.75) + 
  stat_smooth(data=subset(plotdat,agesex!="Female; 1.2"),aes(color=Stock),se=FALSE,method="lm",linetype=2,size=0.5,alpha=0.75) + 
  ylab("Length (mm)") + 
  xlab("Brood Year") + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
        legend.position=c(0.85,0.56),
        panel.grid=element_blank()) + 
  scale_x_continuous(breaks=seq(1981,max.year),
                     labels=c("1981","","","","185","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013")) + 
  #scale_color_grey() +
  scale_color_manual(values=c("red","blue"))

png("Manuscript/Figure_8_Length_at_age_all_gam.png",width=6,height=6,units="in",res=300);pgam;dev.off()


ploess <- ggplot(plotdat,aes(brood_year,length,color=Stock)) + 
  geom_point(size=0.5) + 
  geom_blank(data=dummy) +
  facet_wrap(~agesex,drop = FALSE,scales="free_y",ncol=2,dir="v") + 
  #facet_wrap(~agesex,drop = FALSE,ncol=2,dir="v") + 
  stat_smooth(data=subset(plotdat,agesex!="Female; 1.2"),aes(color=Stock),se=FALSE,method="loess",linetype=1,size=0.75,alpha=0.75) + 
  stat_smooth(data=subset(plotdat,agesex!="Female; 1.2"),aes(color=Stock),se=FALSE,method="lm",linetype=2,size=0.5,alpha=0.75) + 
  ylab("Length (mm)") + 
  xlab("Brood Year") + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
        legend.position=c(0.85,0.56),
        panel.grid=element_blank()) + 
  scale_x_continuous(breaks=seq(1981,max.year),
                     labels=c("1981","","","","185","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013")) + 
  #scale_color_grey() +
  scale_color_manual(values=c("red","blue"))

png("Manuscript/Figure_8_Length_at_age_all_loess.png",width=6,height=6,units="in",res=300);ploess;dev.off()


#-----------------------------------------------------------------------------------------------
#  The following is Figure 8 without the Female age 1.2 group
#-----------------------------------------------------------------------------------------------

#  Define the maximum year in the data
max.year <- 2013

#  Create the age / sex combinations
p <- recoverdat %>% 
  filter(!is.na(sex) & fage%in%c("1.2","1.3","1.4")) %>% 
  mutate(sex=fct_recode(sex,"Male"="M",
                        "Female"="F"),
         agesex=paste(sex,fage,sep="; "),
         agesex=fct_expand(factor(agesex),"Female; 1.2"),
         agesex=fct_relevel(agesex,"Male; 1.2","Male; 1.3","Male; 1.4","Female; 1.2")) #%>% 
  #filter(agesex!="Female; 1.2")


#  This is for scaling each facet to have a total y-axis range of 325mm
dummy <- p %>% 
  filter(length>510) %>% 
  group_by(brood_year,agesex,stock) %>% 
  summarise(length=mean(length,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(agesex) %>% 
  summarise(mymax=max(length),
            mymin=min(length),
            mydiff=mymax-mymin,
            mymid=mymin+mydiff/2,
            newmin=mymid-162.5,
            newmax=mymid+162.5) %>% 
  ungroup %>% 
  dplyr::select(agesex,newmin,newmax) %>% 
  gather(key,length,-agesex) %>% 
  dplyr::select(-key) %>% 
  mutate(brood_year=max.year,
         Stock="Unuk")


#  Add Chickamin into the figure
#  Note that one point with a small mean is driving the apparent upward trend in size.
#  If we filter lengths to be greater than 510, this outlier effect is removed.
plotdat <- p %>% 
  filter(length>510) %>% 
  group_by(brood_year,agesex,stock) %>% 
  summarise(length=mean(length,na.rm=TRUE)) %>% ungroup %>%
  rename(Stock=stock)

ggplot(plotdat,aes(brood_year,length,color=Stock)) + 
  geom_point(size=0.5) + 
  geom_blank(data=dummy) +
  facet_wrap(~agesex,drop = FALSE,scales="free_y",ncol=2,dir="v") + 
  #facet_wrap(~agesex,drop = FALSE,ncol=2,dir="v") + 
  stat_smooth(data=subset(plotdat,agesex!="Female; 1.2"),aes(color=Stock),se=FALSE,method="loess",linetype=1,size=0.75,alpha=0.75) + 
  stat_smooth(data=subset(plotdat,agesex!="Female; 1.2"),aes(color=Stock),se=FALSE,method="lm",linetype=2,size=0.5,alpha=0.75) + 
  ylab("Length (mm)") + 
  xlab("Brood Year") + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
        legend.position=c(0.75,0.90),
        panel.grid=element_blank()) + 
  scale_x_continuous(breaks=seq(1981,max.year),
                     labels=c("1981","","","","1985","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013")) + 
  #scale_color_grey() +
  scale_color_manual(values=c("red","blue"))


#  The following craziness removes creates a blank upper-right panel.
g <- ggplotGrob(p1)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-2-2", "strip-t-2-1")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]
## move axis closer to panel
g$layout[g$layout$name == "axis-b-1-2", c("t", "b")] = c(9, 9)
#  If you don't use scales=free_y in the facet, you'll want this.
#g$layout[g$layout$name == "axis-l-2-1", c("l", "r")] = c(7, 7)
grid.newpage()
grid.draw(g)

png("Manuscript/Figure_8_Length_at_age_scales_free.png",width=6,height=6,units="in",res=300);grid.draw(g);dev.off()

#------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------
#  The following is the original Figure 8 without the Female age 1.2 group
#-----------------------------------------------------------------------------------------------

#  Define the maximum year in the data
max.year <- 2013

#  Create the age / sex combinations
p <- recoverdat %>% 
  filter(!is.na(sex) & fage%in%c("1.2","1.3","1.4")) %>% 
  mutate(sex=fct_recode(sex,"Male"="M",
                        "Female"="F"),
         agesex=paste(sex,fage,sep="; "),
         agesex=fct_expand(factor(agesex),"Female; 1.2"),
         agesex=fct_relevel(agesex,"Male; 1.2","Male; 1.3","Male; 1.4","Female; 1.2")) %>% 
filter(agesex!="Female; 1.2")


#  This is for scaling each facet to have a total y-axis range of 325mm
dummy <- p %>% 
  filter(length>510) %>% 
  group_by(brood_year,agesex,stock) %>% 
  summarise(length=mean(length,na.rm=TRUE)) %>% 
  ungroup %>% 
  group_by(agesex) %>% 
  summarise(mymax=max(length),
            mymin=min(length),
            mydiff=mymax-mymin,
            mymid=mymin+mydiff/2,
            newmin=mymid-162.5,
            newmax=mymid+162.5) %>% 
  ungroup %>% 
  dplyr::select(agesex,newmin,newmax) %>% 
  gather(key,length,-agesex) %>% 
  dplyr::select(-key) %>% 
  mutate(brood_year=max.year,
         Stock="Unuk")


#  Add Chickamin into the figure
#  Note that one point with a small mean is driving the apparent upward trend in size.
#  If we filter lengths to be greater than 510, this outlier effect is removed.
plotdat <- p %>% 
  filter(length>510) %>% 
  group_by(brood_year,agesex,stock) %>% 
  summarise(length=mean(length,na.rm=TRUE)) %>% ungroup %>%
  rename(Stock=stock)

ggplot(plotdat,aes(brood_year,length,color=Stock)) + 
  geom_point(size=0.5) + 
  geom_blank(data=dummy) +
  facet_wrap(~agesex,drop = FALSE,scales="free_y",ncol=2,dir="v") + 
  #facet_wrap(~agesex,drop = FALSE,ncol=2,dir="v") + 
  #stat_smooth(aes(color=Stock),se=FALSE,method="loess",linetype=1,size=0.75,alpha=0.75) + 
  stat_smooth(aes(color=Stock),se=FALSE,method="gam",linetype=1,size=0.75,alpha=0.75) + 
  stat_smooth(aes(color=Stock),se=FALSE,method="lm",linetype=2,size=0.5,alpha=0.75) + 
  ylab("Length (mm)") + 
  xlab("Brood Year") + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
        legend.position=c(0.75,0.90),
        panel.grid=element_blank()) + 
  scale_x_continuous(breaks=seq(1981,max.year),
                     labels=c("1981","","","","1985","","","","1989","","","","1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013")) + 
  #scale_color_grey() +
  scale_color_manual(values=c("red","blue"))


#  The following craziness removes creates a blank upper-right panel.
g <- ggplotGrob(p1)
# get the grobs that must be removed
rm_grobs <- g$layout$name %in% c("panel-2-2", "strip-t-2-1")
# remove grobs
g$grobs[rm_grobs] <- NULL
g$layout <- g$layout[!rm_grobs, ]
## move axis closer to panel
g$layout[g$layout$name == "axis-b-1-2", c("t", "b")] = c(9, 9)
#  If you don't use scales=free_y in the facet, you'll want this.
#g$layout[g$layout$name == "axis-l-2-1", c("l", "r")] = c(7, 7)
grid.newpage()
grid.draw(g)

png("Manuscript/Figure_8_Length_at_age_scales_free.png",width=6,height=6,units="in",res=300);grid.draw(g);dev.off()
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------


#summarise_linear_models_length
#  Create a table of linear model outputs
mdat <- p %>%
  filter(length>510) %>% 
  dplyr::select(agesex,brood_year,stock,length) %>% 
  data.frame
  
#  Make individual models for exploring residuals
#lm1 <- (lm(log(length)~brood_year,data=mdat %>%  filter(agesex=="Female; 1.3")))
#lm2 <- (lm(log(length)~brood_year,data=mdat %>%  filter(agesex=="Female; 1.4")))
#lm3 <- (lm(log(length)~brood_year,data=mdat %>%  filter(agesex=="Male; 1.2")))
#lm4 <- (lm(log(length)~brood_year,data=mdat %>%  filter(agesex=="Male; 1.3")))
#lm5 <- (lm(log(length)~brood_year,data=mdat %>%  filter(agesex=="Male; 1.4")))


p.coef <- mdat %>% 
  group_by(agesex,stock) %>% 
  do(tidy(lm(length~brood_year,data=.))) %>% 
  filter(term!="(Intercept)") %>% 
  arrange(stock,agesex) %>% 
  data.frame

p.sum <- mdat %>% 
  group_by(agesex,stock) %>% 
  do(glance(lm(length~brood_year,data=.))) %>% 
  data.frame %>% 
  dplyr::select(agesex,stock,adj.r.squared) %>% 
  arrange(stock,agesex) %>% 
  data.frame

lm.out <- p.sum %>% 
  inner_join(p.coef) %>% 
  dplyr::select(Stock=stock,agesex,-term,Slope=estimate,SE=std.error,-statistic,p.value,rsq=adj.r.squared) %>% 
  mutate_if(is.numeric,round,3) %>% 
  mutate(p.lm=ifelse(p.value>0,p.value,"<0.001")) %>% 
  dplyr::select(-p.value)

lm.out %>% 
  write_csv("Manuscript/Length_at_age_stock_linear_regressions.csv")
#-----------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------
#  Now GAMs of fish length
#-----------------------------------------------------------------------------------

gam.output <- mdat %>% 
  nest(-c(agesex,stock)) %>% 
  mutate(fit = map(data, ~mgcv::gam(length ~ s(brood_year), data = .)),
         results = map(fit, glance),
         results2 = map(fit,tidy),
         R.square = map_dbl(fit, ~ summary(.)$r.sq)) %>% 
  unnest(results,results2) %>%
  select(-c(data,fit,logLik,deviance,df.residual,term,ref.df,statistic)) %>% 
  mutate(p.value = ifelse(round(p.value,3)==0,"<0.001",round(p.value,3))) %>% 
  arrange(stock,agesex)

#  We want to compare with linear models. We will use linear glms for the comparison
glm.output <- mdat %>% 
  nest(-c(agesex,stock)) %>% 
  mutate(fit = map(data, ~ glm(length ~ brood_year, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>%
  select(stock,agesex,AIC_glm=AIC)

#  Combine the AICs and calculated a delta AIC value where a negative deltaAIC means the GAM did better
gam.output %>% 
  inner_join(glm.output) %>% 
  mutate(deltaAIC=AIC-AIC_glm,
         edf=round(edf,1)) %>% 
  dplyr::select(-c(df,AIC,BIC,AIC_glm)) %>% 
  write.csv("GAM_GLM_Compare.csv")
 

mdat %>% 
  nest(-c(agesex,stock)) %>% 
  mutate(fit = map(data, ~gamm(length ~ s(brood_year), data = .,correlation=corAR1(form=~brood_year))))

#  Examine autocorrelated GAMMs also
#  First Unuk
gamm1 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.2" & stock=="Unuk"),correlation=corAR1())
gamm1arma <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.2" & stock=="Unuk"),correlation=corARMA(p=1,q=0))
gamm2 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.3" & stock=="Unuk"),correlation=corARMA(p=1,q=0))
gamm3 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.4" & stock=="Unuk"),correlation=corARMA(p=1,q=0))
gamm4 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.2" & stock=="Unuk"),correlation=corARMA(p=1,q=0))
gamm5 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.3" & stock=="Unuk"),correlation=corARMA(p=1,q=0))
gamm6 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.4" & stock=="Unuk"),correlation=corARMA(p=1,q=0))
#  Now Chickamin
gamm7 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.2" & stock!="Unuk"),correlation=corARMA(p=1,q=0))
gamm8 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.3" & stock!="Unuk"),correlation=corARMA(p=1,q=0))
gamm9 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.4" & stock!="Unuk"),correlation=corARMA(p=1,q=0))
gamm10 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.2" & stock!="Unuk"),correlation=corARMA(p=1,q=0))
gamm11 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.3" & stock!="Unuk"),correlation=corARMA(p=1,q=0))
gamm12 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.4" & stock!="Unuk"),correlation=corARMA(p=1,q=0))



#old version backup
gamm2 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.3" & stock=="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam2,gamm2$lme)
gamm3 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.4" & stock=="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam3,gamm3$lme)
gamm4 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.2" & stock=="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam4,gamm4$lme)
gamm5 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.3" & stock=="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam4,gamm4$lme)
gamm6 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.4" & stock=="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam5,gamm5$lme)
#  Now Chickamin
gamm7 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.2" & stock!="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam6,gamm6$lme)
gamm8 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.3" & stock!="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam7,gamm7$lme)
gamm9 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Male; 1.4" & stock!="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam8,gamm8$lme)
gamm10 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.2" & stock!="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam9,gamm9$lme)
gamm11 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.3" & stock!="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam9,gamm9$lme)
gamm12 <- gamm(length~s(brood_year),data=mdat %>%  filter(agesex=="Female; 1.4" & stock!="Unuk"),correlation=corARMA(form=~brood_year,p=1,q=0));AIC(gam10,gamm10$lme)

gm1 <- summary(gamm1)
gm2 <- summary(gamm2)
gm3 <- summary(gamm3)
gm4 <- summary(gamm4)
gm5 <- summary(gamm5)
gm6 <- summary(gamm6)
gm7 <- summary(gamm7)
gm8 <- summary(gamm8)
gm9 <- summary(gamm9)
gm10 <- summary(gamm10)

# GAM beat out an autocorrelated GAMM based on AIC in all cases. Note that this is only for Unuk. 
# Chickamin had too many missing years to conduct GAMM with ARMA

gam.out <- mdat %>% 
  group_by(agesex) %>% 
  do(tidy(gam(length~s(brood_year),data=.))) %>% 
  inner_join(data.frame(agesex=unique(mdat$agesex),
                        dev.expl=c(g1$dev.expl,g2$dev.expl,g3$dev.expl,g4$dev.expl,g5$dev.expl,
                                   g6$dev.expl,g7$dev.expl,g8$dev.expl,g9$dev.expl,g10$dev.expl),
                        stock=c(rep("Unuk",5),rep("Chickamin",5)))) %>% 
  dplyr::select(-term,-ref.df,-statistic) %>% 
  mutate(dev.expl=100*dev.expl) %>% 
  mutate_if(is.numeric,round,3) %>% 
  arrange(stock,agesex) %>% 
  mutate(p.gam=ifelse(p.value>0,as.character(p.value),"<0.001")) %>% 
  dplyr::select(-p.value,Stock=stock)





# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------
#  Set-up age regressions
# ----------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------


p <- recoverdat %>% 
  filter(!is.na(sex) & fage%in%c("1.2","1.3","1.4")) %>% 
  mutate(sex=fct_recode(sex,"Male"="M",
                        "Female"="F")) %>% 
  group_by(sex,brood_year,stock) %>% 
  summarise(`1.2`=length(age[age==4])/n(),
            `1.3`=length(age[age==5])/n(),
            `1.4`=length(age[age==6])/n()) %>% 
  gather(age,value,-brood_year,-sex,-stock) %>% 
  data.frame %>% 
  right_join(expand.grid(brood_year=unique(recoverdat$brood_year),
                         sex=c("Male","Female"),
                         age=c("1.2","1.3","1.4"),
                         stock=c("Unuk","Chickamin"))) %>% 
  mutate(agesex=paste(sex,age,sep="; "),
         agesex=fct_expand(factor(agesex),"Female; 1.2"),
         agesex=fct_relevel(agesex,"Male; 1.2","Male; 1.3","Male; 1.4","Female; 1.2"),
         value=ifelse(value==0,0.00001,value),
         value=ifelse(value==1,0.99999,value))


## Stacked area plot for size at age by stock and sex

png(file="Manuscript/Figure_5_Stacked_Area_Plot.png",w=6.5,h=4.5,units="in",res=300)
p %>% 
  ggplot(aes(x=brood_year, y=value,fill=age)) + 
  geom_area(colour="black") + 
  theme_bw() + 
  facet_grid(stock~sex) + 
  scale_fill_viridis_d() +
  scale_x_continuous(breaks=seq(1981,2013),
                     labels=c("","1982","","","","1986","","","","1990","","","","1994","","","","1998","","","","2002","","","","2006","","","","2010","","",""),
                     expand=c(0.015,0.015)) +
  scale_y_continuous(expand=c(0.015,0.015)) +
  theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
        panel.grid = element_blank(),
        legend.position="top") + 
  guides(fill=guide_legend(title="Age")) + 
  ylab("Proportion of return") + 
  xlab("Brood year")
dev.off()



#----------------------------------------------------------------------
#beta_regression_age_at_return_Unuk


#  To find the best fit for each age and sex, I iterated through each of the possible link functions. Only 1.4 males showed
#  any difference among link functions for the Unuk. 
mybetafitfun <- function(mystock,myage,mysex){
  betamodel <- betareg(value ~ brood_year, data = tempdat %>% filter(age==myage & sex==mysex & stock==mystock))
  sapply(c("logit", "probit", "cloglog", "loglog"),function(x) AIC(update(betamodel, link = x)))
}

#  For the females, there are only two age classes of proportion data, which means that the age classes yield results that 
#  are simply inverses of each other. For the models we only fit the 1.3 females, understanding that the results will be equal
#  but with opposite sign for the 1.4 females.

bind_rows(
  mybetafitfun("Unuk","1.2","Male"),
  mybetafitfun("Unuk","1.3","Male"),
  mybetafitfun("Unuk","1.4","Male"),
  mybetafitfun("Unuk","1.3","Female"),
  mybetafitfun("CHICKAMIN R","1.2","Male"), #loglog
  mybetafitfun("CHICKAMIN R","1.3","Male"), #no difference
  mybetafitfun("CHICKAMIN R","1.4","Male"), #cloglog
  mybetafitfun("CHICKAMIN R","1.3","Female") #no difference
)

#  Fit and save each of the models with their specified link function in order to examine residual plots
u1 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.2" & sex=="Male" & stock=="Unuk"))
u2 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.3" & sex=="Male" & stock=="Unuk"))
u3 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.4" & sex=="Male" & stock=="Unuk"))
u4 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.3" & sex=="Female" & stock=="Unuk"))
c1 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.2" & sex=="Male" & stock=="CHICKAMIN R"),link="loglog")
c2 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.3" & sex=="Male" & stock=="CHICKAMIN R"))
c3 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.4" & sex=="Male" & stock=="CHICKAMIN R"),link="cloglog")
c4 <- betareg(value ~ brood_year, data = tempdat %>% filter(age=="1.3" & sex=="Female" & stock=="CHICKAMIN R"))

#  For inspecting residuals
par(mfrow=c(2,2));plot(u1)
par(mfrow=c(2,2));plot(u2)
par(mfrow=c(2,2));plot(u3)
par(mfrow=c(2,2));plot(u4)
par(mfrow=c(2,2));plot(c1)
par(mfrow=c(2,2));plot(c2)
par(mfrow=c(2,2));plot(c3)
par(mfrow=c(2,2));plot(c4)

#  If we do polynomial regression instead of univariate, the R^2 increases but the AICs select the univariate instead due to the 
#  extra parameter penalty. So I just stick with univariate brood_year model here.
mybetafun <- function(mystock,myage,mysex,mylink){
  betamodel <- betareg(value ~ brood_year, data = tempdat %>% filter(age==myage & sex==mysex & stock==mystock),link=mylink)
  mydat <- as.data.frame(coef(summary(betamodel)))[2,1:6]
  names(mydat) <- c("Estimate","SE","Z stat","P","Phi","Phi SE")
  return(bind_cols(data.frame(stock=mystock,
                              age=myage,
                              sex=mysex),
                   mydat,data.frame(pseudo.r.sq=summary(betamodel)$pseudo.r.squared)) %>% 
           mutate(Estimate=round(Estimate,3),
                  SE=round(SE,3),
                  `Z stat`=round(`Z stat`,2),
                  P=round(P,3),
                  P=ifelse(P>0,as.character(P),"<0.001"),
                  `Phi (SE)`=paste(round(Phi,1)," (",round(`Phi SE`,1),")",sep = ""),
                  pseudo.r.sq=round(pseudo.r.sq,2)) %>% 
           dplyr::select(-Phi,-`Phi SE`))
}

bind_rows(
  mybetafun("Unuk","1.2","Male","logit"),
  mybetafun("Unuk","1.3","Male","logit"),
  mybetafun("Unuk","1.4","Male","logit"),
  mybetafun("Unuk","1.3","Female","logit"),
  mybetafun("CHICKAMIN R","1.2","Male","loglog"),
  mybetafun("CHICKAMIN R","1.3","Male","logit"),
  mybetafun("CHICKAMIN R","1.4","Male","cloglog"),
  mybetafun("CHICKAMIN R","1.3","Female","logit")) %>% 
  arrange(stock) %>% 
  mutate(Stock=ifelse(stock=="Unuk","Unuk","Chickamin")) %>%
  dplyr::select(Stock,everything(),-stock) %>% 
  write_csv("Beta_age_regressions.csv")


#  Identify the best link function for the beta gams. 
#  Chickamin 4 ocean males are the only one for which a difference appears
mybetagamfitfun <- function(mystock,myage,mysex){
  mydat <- tempdat %>% filter(age==myage & sex==mysex & stock==mystock)
  AIC(gam(value~s(brood_year),family=betar(link="logit"), data = mydat),
      gam(value~s(brood_year),family=betar(link="probit"), data = mydat),
      gam(value~s(brood_year),family=betar(link="cloglog"), data = mydat),
      gam(value~s(brood_year),family=betar(link="cauchit"), data = mydat))
}

betaout <- bind_rows(
  mybetagamfitfun("Unuk","1.2","Male"),
  mybetagamfitfun("Unuk","1.3","Male"),
  mybetagamfitfun("Unuk","1.4","Male"),
  mybetagamfitfun("Unuk","1.3","Female"),
  mybetagamfitfun("CHICKAMIN R","1.2","Male"), 
  mybetagamfitfun("CHICKAMIN R","1.3","Male"), 
  mybetagamfitfun("CHICKAMIN R","1.4","Male"), 
  mybetagamfitfun("CHICKAMIN R","1.3","Female") 
)

#  When we compare the beta gam and the linear gam, the increased degrees of freedom
#  cancel out any improvements in fit and the linear models are equal to or better than 
#  the GAMs in all cases (based on AIC).
mybetagamfitfun <- function(mystock,myage,mysex,linkf){
  mydat <- tempdat %>% filter(age==myage & sex==mysex & stock==mystock)
  AIC(gam(value~s(brood_year),family=betar(link=linkf), data = mydat),
      gam(value~brood_year,family=betar(link=linkf), data = mydat))
}

mybetagamfitfun("Unuk","1.2","Male","logit")
mybetagamfitfun("Unuk","1.3","Male","logit")
mybetagamfitfun("Unuk","1.4","Male","logit")
mybetagamfitfun("Unuk","1.3","Female","logit")
mybetagamfitfun("CHICKAMIN R","1.2","Male","logit") 
mybetagamfitfun("CHICKAMIN R","1.3","Male","logit") 
mybetagamfitfun("CHICKAMIN R","1.4","Male","logit") 
mybetagamfitfun("CHICKAMIN R","1.3","Female","logit") 

mybetagamfitfun <- function(mystock,myage,mysex,linkf){
  mydat <- tempdat %>% filter(age==myage & sex==mysex & stock==mystock)
  (gam(value~(brood_year),family=betar(link=linkf), data = mydat))
}

mybetagamfitfun("Unuk","1.2","Male","logit")
mybetagamfitfun("Unuk","1.3","Male","logit")
mybetagamfitfun("Unuk","1.4","Male","logit")
mybetagamfitfun("Unuk","1.3","Female","logit")
mybetagamfitfun("CHICKAMIN R","1.2","Male","logit") 
mybetagamfitfun("CHICKAMIN R","1.3","Male","logit") 
mybetagamfitfun("CHICKAMIN R","1.4","Male","logit")
mybetagamfitfun("CHICKAMIN R","1.3","Female","logit") 








#---------------------------------------------------------------------------------------
#  Stacked area plot for age compositions - figure not used
#---------------------------------------------------------------------------------------
p <- recoverdat %>% 
  filter(!is.na(sex) & fage%in%c("1.2","1.3","1.4")) %>% 
  mutate(sex=fct_recode(sex,"Male"="M",
                        "Female"="F")) %>% 
  group_by(sex,brood_year,stock) %>% 
  summarise(`1.2`=length(age[age==4])/n(),
            `1.3`=length(age[age==5])/n(),
            `1.4`=length(age[age==6])/n()) %>% 
  gather(age,value,-brood_year,-sex,-stock) %>% 
  data.frame %>% 
  right_join(expand.grid(brood_year=unique(recoverdat$brood_year),
                         sex=c("Male","Female"),
                         age=c("1.2","1.3","1.4"),
                         stock=c("Unuk","Chickamin"))) %>% 
  mutate(agesex=paste(sex,age,sep="; "),
         agesex=fct_expand(factor(agesex),"Female; 1.2"),
         agesex=fct_relevel(agesex,"Male; 1.2","Male; 1.3","Male; 1.4","Female; 1.2"),
         value=ifelse(value==0,0.00001,value),
         value=ifelse(value==1,0.99999,value))


## Stacked area plot for size at age by stock and sex

png(file="Manuscript/Figure_5_Stacked_Area_Plot.png",w=6.5,h=4.5,units="in",res=300)
p %>% 
  ggplot(aes(x=brood_year, y=value,fill=age)) + 
  geom_area(colour="black") + 
  theme_bw() + 
  facet_grid(stock~sex) + 
  scale_fill_viridis_d() +
  scale_x_continuous(breaks=seq(1981,2013),
                     labels=c("","1982","","","","1986","","","","1990","","","","1994","","","","1998","","","","2002","","","","2006","","","","2010","","",""),
                     expand=c(0.015,0.015)) +
  scale_y_continuous(expand=c(0.015,0.015)) +
  theme(axis.text.x = element_text(angle = 90,size=8,vjust=0.5),
        panel.grid = element_blank(),
        legend.position="top") + 
  guides(fill=guide_legend(title="Age")) + 
  ylab("Proportion of return") + 
  xlab("Brood year")
dev.off()
#---------------------------------------------------------------------------------------



my.mod <- function(mystock,myage){
  print(summary(betareg(survival ~ brood_year, data = surv %>% 
                          filter(fage==myage & stock==mystock) %>% 
                          mutate(survival=survival/100))))
}

my.mod("Unuk","1.2")
my.mod("Unuk","1.3")
my.mod("Unuk","1.4")
my.mod("CHICKAMIN R","1.2")
my.mod("CHICKAMIN R","1.3")
my.mod("CHICKAMIN R","1.4")

