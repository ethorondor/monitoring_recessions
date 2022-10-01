# prepare data, define functions 

options(scipen = 999,digits = 4)
set.seed(1245)
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
#packages <- c("tidyverse","astsa","xts","zoo","forecast","parallel")
packages <- c("tidyverse","xts","zoo","parallel","grid","gridExtra","gtable")
ipak(packages)
##########################################################################
#### source packages and define functions ################################
##########################################################################
source("./code/BSPT.R")
crtBSquid <- function(ts,oth){
  bsquid <- new("BSquid",
                ts = ts,
                nSim = oth$nSim,
                # the prior for prob of each state (0.9,0.1)
                pi0 = c(0.95,0.05),
                pi12 = 0.03,
                model=list( paramNames = c("p11","mu1","mu2","phi","sigma"),
                            param = oth$param,
                            output = c(0,1,2,3,4,5),
                            model.null.erdis = "dnorm(y-(1-model$param[[4]]$value)*model$param[[2]]$value-model$param[[4]]$value*y_1,0,model$param[[5]]$value)" ,
                            model.alt.erdis = "dnorm(y-(1-model$param[[4]]$value)*model$param[[3]]$value-model$param[[4]]$value*y_1,0,model$param[[5]]$value)",
                            model.null = "(1-model$param[[4]]$value)*model$param[[2]]$value+model$param[[4]]$value*y_1+rnorm(1000,mean=0,sd=model$param[[5]]$value)",
                            model.alt = "(1-model$param[[4]]$value)*model$param[[3]]$value+model$param[[4]]$value*y_1+rnorm(1000,mean=0,sd=model$param[[5]]$value)"
                ),
                cnFctr=list(0.01,0.02,0.03),
                trainN=oth$trN
  )
  return(bsquid)
}
mntbrk <- function(paramLst,df,trN,nSim){
  oth <- list('param'=paramLst,
              'trN'=trN,
              'nSim'=nSim)
  lst <- lapply(df,crtBSquid,oth)
  opt <- mclapply(lst, BSquid, mc.cores = getOption("mc.cores", 12L))
  return(opt)
}
mntbrk.ML <- function(paramLst,df,trN,nSim){
  oth <- list('param'=paramLst,
              'trN'=trN,
              'nSim'=nSim)
  lst <- lapply(df,crtBSquid,oth)
  opt <- mclapply(lst, BSquid.ML, mc.cores = getOption("mc.cores", 10L))
  return(opt)
}
deMean <- function(df,trN){
  pk <- list()
  j = 1
  for(i in trN:nrow(df)){
    pk[[j]] = data.frame(date = df[1:i,1],vec=(df[1:i,2]))
    j = j + 1
  }
  return(pk)
}

deMean2 <- function(df){
  pk = list()
  for(i in 2:length(df)){
    v <- df[!is.na(df[,i]),i]
    pk[[i-1]] <- data.frame(date=df[1:length(v),1],vec=(v))
  }
}
crtDfLst <- function(df){
  pk <- list()
  for(i in 2:length(df)){
    d <- df[,c(1,i)]
    pk[[i-1]] <- (d%>%drop_na())
  }
  return(pk)
}
crtLst <- function(l,bgnDate){
  return(l%>%filter(date>=as.Date(bgnDate)))
}


crtMntDf <- function(df,strvn,endvn,bgnDate){
  lst <- df[strvn:endvn]
  l <- lapply(lst,crtLst,bgnDate=bgnDate)
  return(l)
}
##########################################################################
############## read in data ##############################################
##########################################################################
dfm.csv <- read.csv('./data/kalman_DFM.csv')
df <- dfm.csv[,c(-1,-2)]
df['date'] <- as.Date(df[,1],format="%m/%d/%Y")

df_1 = df
df_2 <- data.frame(matrix(ncol=ncol(df_1),nrow=nrow(df_1)))
colnames(df_2) = colnames(df_1)
df_2[,1] = df_1['date']
for(i in 2:length(df_1)){
  df_2[,i] <- c(0,0,rollmean(df_1[,i],3))
}

##################################################################
######## define parameters #######################################
##################################################################

  
paramLst_peak <- list(p11=list(prior = c(0.00,1.00), value = 0.95),
                      mu1=list(prior = c(-0.20,0.80), value = 0.00),
                      mu2=list(prior = c(-2.00,-0.80), value = 0.00),
                      phi=list(prior = c(0.10,0.40), value = 0.00),
                      sigma=list(prior = c(0.50,0.80), value = 0.00)
)

paramLst_trough <- list(p11=list(prior = c(0.00,1.00), value = 0.95),
                        mu1=list(prior = c(-2.00,-0.80), value = 0.00),
                        mu2=list(prior = c(-0.20,0.80), value = 0.00),
                        phi=list(prior = c(0.10,0.40), value = 0.00),
                        sigma=list(prior = c(0.50,0.80), value = 0.00)
)
trN = 7 
nSim = 10000
dfm <- crtDfLst(df_2)