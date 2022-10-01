options(scipen = 999, digits = 4)
setwd("/home/elrond/Documents/Dropbox/Research/datingRecession/fourInd/output")
require(tidyverse)
load("./peak1969.Rdata")
load("./peak1973.Rdata")
load("./peak1980.Rdata")
load("./peak1982.Rdata")
load("./peak1990.Rdata")
load("./peak2001.Rdata")
load("./peak2008.Rdata")

load("./trough1969.Rdata")
load("./trough1973.Rdata")
load("./trough1980.Rdata")
load("./trough1982.Rdata")
load("./trough1990.Rdata")
load("./trough2001.Rdata")
load("./trough2008.Rdata")

df <- opt_trough1982
i <- 0
i <- i + 1; View(drop_na(df[[i]]))
  