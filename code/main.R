  setwd("~/Documents/Dropbox/Research/datingRecession/fourInd")
  source('./code/header.R')
  ##################################################################################
  ### create demeaned data for pre 1976 data
  ##################################################################################
  
  # create pre 1980 data
  vintage = c('date',colnames(df_2[2]))
  df <- (df_2 %>% select(vintage)
               %>%filter(date<=as.Date('1976-01-01')))
  dfm1973 <- deMean(df,trN)
  ########################################################################
  ######### peak 1969 ####################################################
  ########################################################################
  peak1969 <- crtMntDf(dfm1973,4,35,'1967-04-01')
  opt_peak1969 <- mntbrk(paramLst_peak,peak1969,trN=6,nSim)
  save(opt_peak1969,file="./output/peak1969.Rdata")
  #i=0
  #i=i+1;View(opt_peak1969[[i]])
  # peak 1970-01
  # 1969 trough
  trough1969 <- crtMntDf(dfm1973,36,47,'1970-01-01')
  opt_trough1969 <- mntbrk(paramLst_trough,trough1969,trN=6,nSim)
  save(opt_trough1969,file="./output/trough1969.Rdata")
  # trough 1971-02
  
  #peak 1973
  peak1973 <- crtMntDf(dfm1973,48,80,'1971-01-01')
  opt_peak1973 <- mntbrk(paramLst_peak,peak1973,trN=6,nSim)
  save(opt_peak1973,file="./output/peak1973.Rdata")
  #i=0
  #i=i+1;View(opt_peak1973[[i]])
  # 1974-02
  
  #trough 1973
  trough1973 <- crtMntDf(dfm1973,86,100,'1974-03-01')
  opt_trough1973 <- mntbrk(paramLst_trough,trough1973,trN=6,nSim)
  save(opt_trough1973,file="./output/trough1973.Rdata")
  # 1975-07
  #################################################################################################
  ################ peak1980 #######################################################################
  #################################################################################################
  # vintage 1-45 1976-12 to 1980-08
  peak1980 <- crtMntDf(dfm,1,45,'1975-07-01')
  opt_peak1980 <- mntbrk(paramLst_peak,peak1980,trN=10,nSim)
  save(opt_peak1980,file="./output/peak1980.Rdata")
  # peak 1979-09 vintage 1979-10
  ################################################
  ################ peak 1982 #####################
  ################################################
  peak1982 <- crtMntDf(dfm,59,63,'1980-10-01')
  opt_peak1982 <- mntbrk(paramLst_peak,peak1982,trN=10,nSim)
  save(opt_peak1982,file="./output/peak1982.Rdata")
  # peak 1981-09 vintage 1981-10
  ###############################################
  ################ peak 1990 ####################
  ###############################################
  peak1990 <- crtMntDf(dfm,86,170,'1983-01-01')
  opt_peak1990 <- mntbrk(paramLst_peak,peak1990,trN=10,nSim)
  save(opt_peak1990,file="./output/peak1990.Rdata")
  # peak 1990-10 vintage 1990-12
  ###############################################
  ################ peak 2001 ####################
  ###############################################
  peak2001 <- crtMntDf(dfm,189,300,'1991-08-01')
  opt_peak2001 <- mntbrk(paramLst_peak,peak2001,trN=10,nSim)
  save(opt_peak2001,file="./output/peak2001.Rdata")
  ######################################################
  ######### peak 2008 ##################################
  ######################################################
  peak2008 <- crtMntDf(dfm,319,386,'2002-06-01')
  opt_peak2008 <- mntbrk(paramLst_peak,peak2008,trN=10,nSim)
  save(opt_peak2008,file="./output/peak2008.Rdata")
  
  #################################################################################################
  ################ trough 1980 ####################################################################
  #################################################################################################
  trough1980 <- crtMntDf(dfm,41,50,'1979-10-01')
  opt_trough1980 <- mntbrk(paramLst_trough,trough1980,trN=6,nSim)
  save(opt_trough1980,file="./output/trough1980.Rdata")
  # trough 1980-10 vintage 1980-11
  ################################################
  ################ trough 1982 ###################
  ################################################
  trough1982 <- crtMntDf(dfm,66,80,'1981-11-01')
  opt_trough1982 <- mntbrk(paramLst_trough,trough1982,trN=6,nSim)
  save(opt_trough1982,file="./output/trough1982.Rdata")
  # trough 1983-03 vintage 1983-04
  ###############################################
  ################ trough 1990 ##################
  ###############################################
  trough1990 <- crtMntDf(dfm,173,185,'1990-10-01')
  opt_trough1990 <- mntbrk(paramLst_trough,trough1990,trN=6,nSim)
  save(opt_trough1990,file="./output/trough1990.Rdata")
  # trough 1991-08 vintage 1991-09
  ######################################################
  ######### trough 2001 ################################
  ######################################################
  trough2001 <- crtMntDf(dfm,296,325,'2001-01-01')
  opt_trough2001 <- mntbrk(paramLst_trough,trough2001,trN=6,nSim)
  save(opt_trough2001,file="./output/trough2001.Rdata")
  ######################################################
  ########### trough2008 ###############################
  ######################################################
  trough2008 <- crtMntDf(dfm,381,410,'2008-02-01')
  opt_trough2008 <- mntbrk(paramLst_trough,trough2008,trN=6,nSim)
  save(opt_trough2008,file="./output/trough2008.Rdata")