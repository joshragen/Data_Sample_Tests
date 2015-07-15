#This was code written to split into high and low sections, which turned out to be easier than I was expecting.

#dato <- dat$testresults_absolute_monocytes[order(dat$testresults_absolute_monocytes, na.last = NA)]
#sorts in ascending order

#source("fix_values.R")
#fix_values("cbc_results_units.csv")

fix_values <- function(file, test = 34){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)
      l <- ncol(dat)
      #dat<-read.csv("cbc_results_units.csv", stringsAsFactors = FALSE)
      
      datf <- dat[-which(is.na(dat[[test]])),]
      #n <- nrow(datf)
      #bound <- quantile(datf[[test]], c(0.19))[[1]]
      bound <- 3
      #low_dat <- data.frame()
      #high_dat <- data.frame()
      #sep_dat <- function(b){
      #is_low <- c()
      #for(b in 1:n){
      #      if(datf[[test]][b] < bound){
      #            #low_dat <- rbind(low_dat, datf[b,])
      #            is_low <- c(is_low, T)
      #      }else{
      #            #high_dat <- rbind(high_dat, datf[b,])
      #            is_low <- c(is_low, F)
      #      }
      #}
      #low_dat <- datf[which(is_low == T),]
      #high_dat <- datf[which(is_low == F),]
      
      high_dat <- split(datf, (datf[[test]] < bound))[[1]]
      low_dat <- split(datf, (datf[[test]] < bound))[[2]]
      
      #datl<-(split(low_dat[[test]], low_dat$registry))
      #boxplot(datl[c(1:6,9)])
      
}

#write.csv(high_dat, "high_cbc_results_ab_monocytes.csv")
#write.csv(low_dat, "low_cbc_results_ab_monocytes.csv")