library(tidyverse)
library(zoo)
library(patchwork)
library(lubridate)
library(padr)

rm(list=ls())

## set smoothing windowsize and sample window size for t tests
windowsize <- 15
windowsize_ttest <- 30

## load in daily CI (song) metrics
ci_daily <- read.csv("data_processed/song_daily.csv")
ci_daily$date <- as.POSIXct(ci_daily$date, tz = "UTC")

## fill in missing dates w/ NA
ci_daily <- pad(ci_daily)

## run t tests to identify significant drops in CInight:CIday (behavioral transition)
dci_sig1 <- data.frame(as.numeric(rep(1,length(ci_daily$date))))
colnames(dci_sig1) <- c("pv")
dci_sig2 <- data.frame(as.numeric(rep(1,length(ci_daily$date))))
colnames(dci_sig2) <- c("pv")
for (d in (1):(length(ci_daily$date)-windowsize_ttest)) {

  i1 <- d-windowsize_ttest
  i2 <- d-1
  j1 <- d+1
  j2 <- d+windowsize_ttest
  
  ## only proceed if indices are positive
  if (i1 > 0) {
    s1 <- ci_daily$blue_ci_ratio[i1:i2]
    s2 <- ci_daily$blue_ci_ratio[j1:j2]
    
    ## only run the t-test if there is sufficient CI coverage during the current test time period
    if ((sum(is.na(s1)) < 10) && (sum(is.na(s2)) < 20)) {
      res1 <- t.test(s1,s2,alternative = "greater", var.equal = FALSE, na.omit =TRUE)
      res2 <- t.test(s1,s2,alternative = "less", var.equal = FALSE, na.omit =TRUE)
      dci_sig1$pv[d] <- res1$p.value
      dci_sig2$pv[d] <- res2$p.value
    }
    
    else {
      dci_sig1$pv[d] <- NA
      dci_sig2$pv[d] <- NA
    }
  }
  else {
    dci_sig1$pv[d] <- NA
    dci_sig2$pv[d] <- NA
  }
}

## identify periods of significant CI night:day decrease (transition to migration)
ci_daily$pv1 <- dci_sig1$pv
migration_transition <- ci_daily %>%
  filter(pv1<0.05)

##### save
save(ci_daily, file = "data_processed/ci_daily.Rdata")
save(migration_transition, file = "data_processed/migration_transition.Rdata")
