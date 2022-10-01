# prepare data, define functions 
options(scipen = 999,digits = 4)
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
crtDfTs <- function(vec,stry,strm,freq){
  v.ts <- ts(as.data.frame(vec),start = c(stry, strm),frequency=freq)
  v.df <- as.data.frame.ts(v.ts)
  v.df$date <- as.Date.ts(v.ts)
  return(v.df)
}
# usage
#packages <- c("tidyverse","astsa","xts","zoo","forecast","parallel")
packages <- c("tidyverse","xts","zoo","parallel","grid","gridExtra","gtable","rlist")
ipak(packages)
setwd("~/Dropbox/Research/datingRecession/fourInd")
df <- crtDfTs(seq(420),1976,1,12)

df <- (df%>%mutate(NBER=ifelse(date>=as.Date('1980-01-01')&date<=as.Date('1980-07-01'),1,
                               ifelse(date>=as.Date('1981-07-01')&date<=as.Date('1982-11-01'),1,
                                      ifelse(date>=as.Date('1990-07-01')&date<=as.Date('1991-03-01'),1,
                                             ifelse(date>=as.Date('2001-03-01')&date<=as.Date('2001-11-01'),1,
                                                    ifelse(date>=as.Date('2007-12-01')&date<=as.Date('2009-06-01'),1,0))))))
       )
df <- (df%>%mutate(c12=ifelse(date>=as.Date('1979-10-01')&date<=as.Date('1980-09-01'),1,
                               ifelse(date>=as.Date('1981-10-01')&date<=as.Date('1983-01-01'),1,
                                      ifelse(date>=as.Date('1990-10-01')&date<=as.Date('1991-01-01'),1,
                                             ifelse(date>=as.Date('2001-03-01')&date<=as.Date('2002-04-01'),1,
                                                    ifelse(date>=as.Date('2008-05-01')&date<=as.Date('2009-11-01'),1,0))))))
)

  # P: the number of real positive cases in the data
  # N: the number of real negative cases in the data
  # TP: true positive
  # TN: true negative
  # FP: false positive
  # FN: false negative
df<-(df%>%mutate(P=ifelse(NBER==1,1,0))
       %>%mutate(N=ifelse(NBER==0,1,0))
       %>%mutate(TP=ifelse(NBER==1 & c12 ==1,1,0))
       %>%mutate(TN=ifelse(NBER==0 & c12 ==0,1,0))
       %>%mutate(FP=ifelse(NBER==0 & c12 ==1,1,0))
       %>%mutate(FN=ifelse(NBER==1 & c12 ==0,1,0))
     )
# true positive rate
TPR <- sum(df$TP)/sum(df$P)
TNR <- sum(df$TN)/sum(df$N)
c1 <- c(0,0)
c2 <- c(1,1)
R <- rbind(c1,c2)
R <- rbind(R,c(TPR,TNR))
R_df <- as.data.frame(R)
p <- ggplot(R_df, aes(V1, V2)) +  geom_point()
p <- p + stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+  labs(x="TPR",y="TNR")
ggsave(filename="~/Dropbox/Research/shared/datingRecession/Graph/ROC.png",plot=p,width = 10,height = 5.15, dpi=300)


# loess method: local regression fitting
p + geom_smooth(method = "loess")

df <- (df%>%mutate(c33=ifelse(date>=as.Date('1979-08-01')&date<=as.Date('1980-09-01'),1,
                              ifelse(date>=as.Date('1981-10-01')&date<=as.Date('1983-01-01'),1,
                                     ifelse(date>=as.Date('1990-10-01')&date<=as.Date('1991-06-01'),1,
                                            ifelse(date>=as.Date('2000-12-01')&date<=as.Date('2002-06-01'),1,
                                                   ifelse(date>=as.Date('2008-04-01')&date<=as.Date('2009-11-01'),1,0))))))
)

# P: the number of real positive cases in the data
# N: the number of real negative cases in the data
# TP: true positive
# TN: true negative
# FP: false positive
# FN: false negative
df<-(df%>%mutate(P=ifelse(NBER==1,1,0))
     %>%mutate(N=ifelse(NBER==0,1,0))
     %>%mutate(TP=ifelse(NBER==1 & c33 ==1,1,0))
     %>%mutate(TN=ifelse(NBER==0 & c33 ==0,1,0))
     %>%mutate(FP=ifelse(NBER==0 & c33 ==1,1,0))
     %>%mutate(FN=ifelse(NBER==1 & c33 ==0,1,0))
)
# true positive rate
TPR <- sum(df$TP)/sum(df$P)
TNR <- sum(df$TN)/sum(df$N)
R <- rbind(R,c(TPR,TNR))
R_df <- as.data.frame(R)
plot(R_df$V1,R_df$V2)
