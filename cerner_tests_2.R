
setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-data.table(read.csv("cerner_output_info.csv", stringsAsFactors = FALSE))

dat$Date <- as.Date(dat$Test.Date, "%m/%d/%Y")
dat_rno <- (split(dat, dat$RNO.Hash))
rno_dim <- sapply(dat_rno, function(x) dim(x))[1,]
dat_key <- (split(dat, dat$Test.Key))
names(dat_key)[68] <- "cd4-cd8_ratio"
names(dat_key)[70] <- "cd4_percent"
names(dat_key)[72] <- "cd8_percent"

cerner_graph <- function(g, dense = T, qq = T, box = F){
      for(t in 1:length(dat_key)){
            if(nrow(dat_key[[t]]) <= 50){
                  #print(paste(t,"skip", nrow(dat_key[[t]])))
                  next
            }
            datk <- dat_key[[t]]
            datk$Test.Result <- as.numeric(datk$Test.Result)
            datk <- datk[-which(is.na(datk$Test.Result)),]
            if(length(datk$Test.Result) <= 30){
                  #print(paste(t, "row.skip", length(datk$Test.Result)))
                  next
            }
            for(i in 1:4){
                  out <- abs((datk$Test.Result)-mean(datk$Test.Result, na.rm = TRUE)) >
                        (6)*sd(datk$Test.Result, na.rm = TRUE)
                  if(T %in% out){
                        datk<-datk[(-which(out == T)),]
                        rownames(out)<-NULL
                  }else{
                        datk <- datk
                  }
                  if(all(is.na(datk$Test.Result))){
                        #print(paste(t, "na.skip"))
                        next
                  }
            }
            #fixes outliers by removing them so graphs are cleaner
            #library(lubridate)
            dates <- split(datk, year(datk$Date))
            
            #QQNORM
            if(qq){
                  jpeg(paste0(t, "tqqplot_", names(dat_key)[t], ".jpg"), 1000,600)
                  qqnorm(datk$Test.Result)
                  qqline(datk$Test.Result)
                  legn <- c()
                  legc <- c()
                  for(l in 1:length(dates)){
                        if(length(dates[[l]]$Test.Result) < 15){
                              next
                        }
                        spoints <- qqnorm(dates[[l]]$Test.Result, plot.it = F)
                        points(spoints, col = c(l+1))
                        qqline((dates[[l]]$Test.Result), col = c(l+1))
                        legn <- c(legn, paste(names(dates[l]), length(dates[[l]]$Test.Result)))
                        legc <- c(legc, c(l+1))
                  }
                  if(length(legn) == 0){
                        dev.off()
                        message("Printed to ", paste0(t, "tqqplot_", names(dat_key)[t], ".jpg"))
                  }else{
                        legend("topleft", legend = legn, col = legc, lty = 1)
                        #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
                        dev.off()
                        message("Printed to ", paste0(t, "tqqplot_", names(dat_key)[t], ".jpg"))
                  }
            }
            
            #DENSITY
            if(dense){
                  jpeg(paste0(t, "tdensity_", names(dat_key)[t], ".jpg"), 1000,600)
                  d<-density(datk$Test.Result, na.rm = T)
                  plot(d)
                  polygon(d, col="red")
                  legn <- c()
                  legc <- c()
                  for(l in 1:length(dates)){
                        if(length(dates[[l]]$Test.Result) < 15){
                              next
                        }
                        lines(density(dates[[l]]$Test.Result), col = c(l+1))
                        legn <- c(legn, paste(names(dates[l]), length(dates[[l]]$Test.Result)))
                        legc <- c(legc, c(l+1))
                  }
                  if(length(legn) == 0){
                        dev.off()
                        message("Printed to ", paste0(t, "tdensity_", names(dat_key)[t], ".jpg"))
                  }else{
                        legend("topright", legend = legn, col = legc, lty = 1)
                        #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
                        dev.off()
                        message("Printed to ", paste0(t, "tdensity_", names(dat_key)[t], ".jpg"))
                  } 
            }
            
            #BOXPLOT
            if(box){
                  jpeg(paste0(t, "boxplot_",names(dat_key)[t], "_dates.jpg"), 1000,800)
                  datesf <- split(datk$Test.Result, year(datk$Date))
                  #datl<-(split(datfix[[j]], datfix$Registry))
                  for(q in 1:length(dates)){
                        if(length(datesf[[q]]) < 15){
                              datesf[[q]] <- NA
                              #print("T")
                        }
                  }
                  if(all(is.na(datesf))){
                        dev.off()
                        next
                  }
                  b <- boxplot(datesf, plot = 0)
                  par(mar = c(9,4,1,1))
                  boxplot(datesf, names = paste0(b$names, "(n=", b$n, ")"), las = 2)
                  dev.off()
                  message("Printed to ", paste0(t, "boxplot_",names(dat_key)[t], "_dates.jpg"))
            }
      }
      print(g)
}


#This code is for dealing with graphing absolute_lymphocites (t = 4) in a clean way due to different units
{
#datk_names <- split(datk, datk$Test.Name)
#datk_names[[2]]$Test.Result <- datk_names[[2]]$Test.Result * 1000
#datk_names[[4]]$Test.Result <- datk_names[[4]]$Test.Result * 1000
#
#for(n in 1:2){
#      for(i in 1:2){
#            out <- abs((datk_names[[n]]$Test.Result)-mean(datk_names[[n]]$Test.Result, na.rm = TRUE)) >
#                  (6)*sd(datk_names[[n]]$Test.Result, na.rm = TRUE)
#            if(T %in% out){
#                  datk_names[[n]]<-datk_names[[n]][(-which(out == T)),]
#                  rownames(out)<-NULL
#            }else{
#                  datk_names[[n]] <- datk_names[[n]]
#            }
#            if(all(is.na(datk_names[[n]]$Test.Result))){
#                  print(paste(t, "na.skip"))
#                  next
#            }
#      }
#      dates <- split(datk_names[[n]], year(datk_names[[n]]$Date))
#      jpeg(paste0(t, "_", n, "_tqqplot_", names(datk_names)[n], ".jpg"), 1000,600)
#      qqnorm(datk_names[[n]]$Test.Result)
#      qqline(datk_names[[n]]$Test.Result)
#      legn <- c()
#      legc <- c()
#      for(l in 1:length(dates)){
#            if(length(dates[[l]]$Test.Result) < 30){
#                  next
#            }
#            spoints <- qqnorm(dates[[l]]$Test.Result, plot.it = F)
#            points(spoints, col = c(l+1))
#            qqline((dates[[l]]$Test.Result), col = c(l+1))
#            legn <- c(legn, paste(names(dates[l]), length(dates[[l]]$Test.Result)))
#            legc <- c(legc, c(l+1))
#      }
#      if(length(legn) == 0){
#            dev.off()
#      }else{
#            legend("topleft", legend = legn, col = legc, lty = 1)
#            legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
#            dev.off()
#      }
#      jpeg(paste0(t, "_", n, "_tdensity_", names(datk_names)[n], ".jpg"), 1000,600)
#      d<-density(datk_names[[n]]$Test.Result, na.rm = T)
#      plot(d)
#      polygon(d, col="red")
#      legn <- c()
#      legc <- c()
#      for(l in 1:length(dates)){
#            if(length(dates[[l]]$Test.Result) < 30){
#                  next
#            }
#            lines(density(dates[[l]]$Test.Result), col = c(l+1))
#            legn <- c(legn, paste(names(dates[l]), length(dates[[l]]$Test.Result)))
#            legc <- c(legc, c(l+1))
#      }
#      if(length(legn) == 0){
#            dev.off()
#            print(t)
#      }else{
#            legend("topright", legend = legn, col = legc, lty = 1)
#            legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
#            dev.off()
#            print(t)  
#      }
#      #dev.off()
#}
#datkf <- rbindlist(datk_names)
#for(i in 1:4){
#      out <- abs((datkf$Test.Result)-mean(datkf$Test.Result, na.rm = TRUE)) >
#            (6)*sd(datkf$Test.Result, na.rm = TRUE)
#      if(T %in% out){
#            datkf<-datkf[(-which(out == T)),]
#            rownames(out)<-NULL
#      }else{
#            datkf <- datkf
#      }
#      if(all(is.na(datkf$Test.Result))){
#            print(paste(t, "na.skip"))
#            next
#      }
#}
#datk<-datkf
}