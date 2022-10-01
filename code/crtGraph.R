rm(list=ls())
library(latex2exp)
setwd("~/Documents/Dropbox/Research/datingRecession/fourInd")
source('~/Documents/Dropbox/Research/datingRecession/fourInd/code/header.R')

##################################################################################
############ create spegattite graph #############################################
##################################################################################
############################################################
##########  create spegattie graph  ########################
############################################################
for(i in 1:length(dfm)){
  names(dfm[[i]])<-c('date','vec')
  dfm[[i]] <- (dfm[[i]]%>%dplyr::filter(date>=as.Date("1976-01-01")))
}
p<-ggplot()
for(i in 1:length(dfm)){
  p<-p+geom_line(data=dfm[[i]],aes(x=date,y=vec))
}

p1 <- p+
  scale_y_continuous(breaks = c(0),labels = c("0"))+
  #geom_rect(data=dfm[[440]],aes(xmin=as.Date("1969-12-01","%Y-%m-%d"),
  #                              xmax=as.Date("1970-11-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  #geom_rect(data=dfm[[440]],aes(xmin=as.Date("1973-11-01","%Y-%m-%d"),
  #                              xmax=as.Date("1975-03-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("1980-01-01","%Y-%m-%d"),
                                xmax=as.Date("1980-07-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("1981-07-01","%Y-%m-%d"),
                                xmax=as.Date("1982-11-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("1990-07-01","%Y-%m-%d"),
                                xmax=as.Date("1991-03-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("2001-03-01","%Y-%m-%d"),
                                xmax=as.Date("2001-11-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("2007-12-01","%Y-%m-%d"),
                                xmax=as.Date("2009-06-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  labs(x="Peaks and Troughs Identified by NBER",y="Common Factor")+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")


p2 <- p+  
  scale_y_continuous(breaks = c(0),labels = c("0"))+
  #geom_rect(data=dfm[[440]],aes(xmin=as.Date("1970-01-01","%Y-%m-%d"),
  #                              xmax=as.Date("1971-01-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  #geom_rect(data=dfm[[440]],aes(xmin=as.Date("1974-02-01","%Y-%m-%d"),
  #                              xmax=as.Date("1975-06-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("1979-10-01","%Y-%m-%d"),
                                xmax=as.Date("1980-09-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("1981-10-01","%Y-%m-%d"),
                                xmax=as.Date("1983-01-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("1990-10-01","%Y-%m-%d"),
                                xmax=as.Date("1991-06-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("2001-03-01","%Y-%m-%d"),
                                xmax=as.Date("2002-04-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(data=dfm[[440]],aes(xmin=as.Date("2008-05-01","%Y-%m-%d"),
                                xmax=as.Date("2009-11-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  labs(x="Peaks and Troughs Identified by Bsquid",y="Common Factor")+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")
  
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
grid.newpage()
grid.draw(g)
ggsave(filename="~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/cmnfct.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)
#####################################################################################
#####################################################################################
#####################################################################################
  load("~/Documents/Dropbox/Research/datingRecession/fourInd/output/peak2008.Rdata")
  df <- drop_na(opt_peak2008[[62]])
  p1 <- ggplot(data=df)+geom_line(aes(x=date,y=vec,linetype="Jul 2008 Vintage"),size=0.5)+
    geom_vline(xintercept=as.numeric(df$date[24]),linetype=1)+
    geom_hline(aes(yintercept=0))+
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
    theme(legend.position = c(0.15,0.4),legend.text = element_text(colour = "black",size=12))+
    labs(x="mon/year",y="Common Factor")+
    theme(legend.title=element_blank(),axis.title = element_text(size=12))
  p2 <- ggplot(data=df)+geom_line(aes(x=date,y=pi,linetype="probability of structural break"),size=0.5)+
    geom_line(aes(x=date,y=c1,linetype="optimal stopping time"),size=0.5)+
    geom_vline(xintercept=as.numeric(df$date[24]),linetype=1)+
    scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
    labs(x="mon/year",y="probability")+
    theme(legend.position = c(0.15,0.4),legend.text = element_text(colour = "black",size=12))+
    theme(legend.title=element_blank(),axis.title = element_text(size=12))
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g <- rbind(g1,g2,size="first")
  g$widths <- unit.pmax(g1$widths,g2$widths)
  grid.newpage()
  grid.draw(g)
  ggsave(filename="~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/peak2008.png",
         plot=grid.draw(g),width = 10,height = 5.15, dpi=300)
#######################################################################################
load("~/Documents/Dropbox/Research/datingRecession/fourInd/output/trough2008.Rdata")
df <- drop_na(opt_trough2008[[17]])
p1 <- ggplot(data=df)+geom_line(aes(x=date,y=vec,linetype="Dec 2009 Vintage"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[17]),linetype=1)+
  geom_hline(aes(yintercept=0))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  theme(legend.position = c(0.15,0.2),legend.text = element_text(colour = "black",size=12))+
  labs(x="mon/year",y="Conmon Factor")+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
p2 <- ggplot(data=df)+geom_line(aes(x=date,y=pi,linetype="probability of structural break"),size=0.5)+
  geom_line(aes(x=date,y=c3,linetype="optimal stopping time"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[17]),linetype=1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  labs(x="mon/year",y="probability")+
  theme(legend.position = c(0.20,0.3),legend.text = element_text(colour = "black",size=12))+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
grid.newpage()
grid.draw(g)
ggsave(filename="~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/trough2008.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)
##################################################################################
################################ CFNAI ###########################################
##################################################################################
dt <- read.csv("../cfnai/data/cfnai-realtime-2001.csv", header = TRUE)
dt.ma3 <- dt[,seq(1,ncol(dt),by=2)]
dt.ma3[,1]<-as.Date(dt.ma3[,1],format="%m/%d/%Y")
cfnai122011 <- (dt.ma3%>%dplyr::filter(d>=as.Date("1976-01-01")))
#################################################################################
######################### over all CFNAI ########################################
#################################################################################
p<-ggplot(data=cfnai122011)+geom_line(aes(x=d,y=CF3122011))
p1 <- p+
  scale_y_continuous(breaks = c(0),labels = c("0"))+
  geom_rect(aes(xmin=as.Date("1980-01-01","%Y-%m-%d"),
                xmax=as.Date("1980-07-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("1981-07-01","%Y-%m-%d"),
                xmax=as.Date("1982-11-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("1990-07-01","%Y-%m-%d"),
                xmax=as.Date("1991-03-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("2001-03-01","%Y-%m-%d"),
                xmax=as.Date("2001-11-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("2007-12-01","%Y-%m-%d"),
                xmax=as.Date("2009-06-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.06)+
  labs(x="Peaks and Troughs Identified by NBER",y="CFNAI MA3")+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")
p2 <- p+  
  scale_y_continuous(breaks = c(0),labels = c("0"))+
  geom_rect(aes(xmin=as.Date("1980-05-01","%Y-%m-%d"),
                xmax=as.Date("1980-12-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("1981-11-01","%Y-%m-%d"),
                xmax=as.Date("1983-01-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("1990-11-01","%Y-%m-%d"),
                xmax=as.Date("1991-06-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("2001-06-01","%Y-%m-%d"),
                xmax=as.Date("2002-01-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.01)+
  geom_rect(aes(xmin=as.Date("2008-04-01","%Y-%m-%d"),
                xmax=as.Date("2009-09-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.06)+
  labs(x="Peaks and Troughs Identified by the Model",y="CFNAI MA3")+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
grid.newpage()
grid.draw(g)
ggsave(filename = "~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/cfnai_all.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)
################### 2008 real time ##############################################
crtDfLst <- function(df){
  pk <- list()
  for(i in 2:length(df)){
    d <- df[,c(1,i)]
    colnames(d) <- c('date','vec')
    d <- (d%>%dplyr::filter(date>=as.Date("2004-01-01")))
    pk[[i-1]] <- (d%>%drop_na())
  }
  return(pk)
}
cfnai.lst <- crtDfLst(dt.ma3) 
cfnai2008 <- cfnai.lst[56:110]
p<-ggplot()
for(i in 1:length(cfnai2008)){
  p<-p+geom_line(data=cfnai2008[[i]],aes(x=date,y=vec))
}
p1 <- p+  
  scale_y_continuous(breaks = c(0),labels = c("0"))+
  geom_rect(data=cfnai2008[[31]],aes(xmin=as.Date("2007-12-01","%Y-%m-%d"),
                                     xmax=as.Date("2009-06-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.02)+
  labs(x="Peaks and Troughs Identified by NBER",y="Real Time CFNAI MA3")+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")
p2 <- p+  
  scale_y_continuous(breaks = c(0),labels = c("0"))+
  geom_rect(data=cfnai2008[[31]],aes(xmin=as.Date("2008-04-01","%Y-%m-%d"),
                xmax=as.Date("2009-09-01","%Y-%m-%d"),ymin=-Inf,ymax=Inf),alpha=.02)+
  labs(x="Peaks and Troughs Identified by Bsquid",y="Real Time CFNAI MA3")+
  scale_x_date(date_breaks = "1 year", date_labels = "%y")
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
grid.newpage()
grid.draw(g)
ggsave(filename = "~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/cfnai_2008.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)
################## dating 2008 Recession with Real time CFNAI ######################
load("~/Dropbox/Research/datingRecession/cfnai/output/peak2008.Rdata")
df <- p2008[[62]][53:72,]
p1 <- ggplot(data=df)+geom_line(aes(x=date,y=vec,linetype="May 2008 Vintage"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[20]),linetype=1)+
  geom_hline(aes(yintercept=0))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  theme(legend.position = c(0.15,0.3),legend.text = element_text(colour = "black",size=12))+
  labs(x="mon/year",y="CFNAI MA3")+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
p2 <- ggplot(data=df)+geom_line(aes(x=date,y=pi,linetype="probability of structural break"),size=0.5)+
  geom_line(aes(x=date,y=c1,linetype="optimal stopping time"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[20]),linetype=1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  labs(x="mon/year",y="probability")+
  theme(legend.position = c(0.15,0.3),legend.text = element_text(colour = "black",size=12))+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
grid.newpage()
grid.draw(g)
ggsave(filename="~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/peak2008_cfnai.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)

load("~/Documents/Dropbox/Research/datingRecession/cfnai/output/trough2008.Rdata")
df <- drop_na(t2008[[11]])
p1 <- ggplot(data=df)+geom_line(aes(x=date,y=vec,linetype="Oct 2009 Vintage"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[11]),linetype=1)+
  geom_hline(aes(yintercept=0))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  theme(legend.position = c(0.15,0.45),legend.text = element_text(colour = "black",size=12))+
  labs(x="mon/year",y="CFNAI MA3")+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
p2 <- ggplot(data=df)+geom_line(aes(x=date,y=pi,linetype="probability of structural break"),size=0.5)+
  geom_line(aes(x=date,y=c3,linetype="optimal stopping time"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[11]),linetype=1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  labs(x="mon/year",y="probability")+
  theme(legend.position = c(0.15,0.4),legend.text = element_text(colour = "black",size=12))+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
grid.newpage()
grid.draw(g)
ggsave(filename="~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/trough2008_cfnai.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)

  ##### use 2008 trough to demonstrate different control factors ##########
df <- drop_na(t2008[[20]])
p1 <- ggplot(data=df)+geom_line(aes(x=date,y=vec,linetype="Oct 2009 Vintage"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[10]),linetype=1)+
  geom_hline(aes(yintercept=0))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  theme(legend.position = c(0.15,0.4),legend.text = element_text(colour = "black",size=12))+
  labs(x="mon/year",y="CFNAI MA3")+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
p2 <- ggplot(data=df)+
  geom_line(aes(x=date,y=c1,linetype="c=0.01"),size=0.5)+
  geom_line(aes(x=date,y=c2,linetype="c=0.02"),size=0.5)+
  geom_line(aes(x=date,y=c3,linetype="c=0.03"),size=0.5)+
  geom_line(aes(x=date,y=pi,linetype="probability of structural break"),size=0.5)+
  geom_vline(xintercept=as.numeric(df$date[10]),linetype=1)+
  geom_vline(xintercept=as.numeric(df$date[11]),linetype=1)+
  geom_vline(xintercept=as.numeric(df$date[19]),linetype=1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")+
  labs(x="mon/year",y="probability")+
  theme(legend.position = c(0.70,0.4),legend.text = element_text(colour = "black",size=12))+
  theme(legend.title=element_blank(),axis.title = element_text(size=12))
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1,g2,size="first")
g$widths <- unit.pmax(g1$widths,g2$widths)
grid.newpage()
grid.draw(g)
ggsave(filename="~/Documents/Dropbox/Research/sheredWithXuguang/datingRecession/IJOFRevision/Graph/trough2008_cfnai_cf.png",
       plot=grid.draw(g),width = 10,height = 5.15, dpi=300)
  