
setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-data.table(read.csv("cerner_output_info.csv", stringsAsFactors = FALSE))

dat$Date <- as.Date(dat$Test.Date, "%m/%d/%Y")
dat_rno <- (split(dat, dat$RNO.Hash))
rno_dim <- sapply(dat_rno, function(x) dim(x))[1,]
dat_key <- (split(dat, dat$Test.Key))
dat_reg <- split(dat, dat$Registry)
sapply(dat_reg, function(x) dim(x))[1,]
sapply(dat_reg, function(x) length(unique(x$RNO.Hash)))
names(dat_key)[68] <- "cd4-cd8_ratio"
names(dat_key)[70] <- "cd4_percent"
names(dat_key)[72] <- "cd8_percent"

cerner_graph <- function(g, dense = T, qq = T, box = T){
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
            reg <- split(datk, datk$Registry)
            
            #QQNORM
            if(qq){
                  jpeg(paste0(t, "rqqplot_", names(dat_key)[t], "_reg.jpg"), 1000,600)
                  qqnorm(datk$Test.Result)
                  qqline(datk$Test.Result)
                  legn <- c()
                  legc <- c()
                  for(l in 1:length(reg)){
                        if(length(reg[[l]]$Test.Result) < 15){
                              next
                        }
                        spoints <- qqnorm(reg[[l]]$Test.Result, plot.it = F)
                        points(spoints, col = c(l+1))
                        qqline((reg[[l]]$Test.Result), col = c(l+1))
                        legn <- c(legn, paste(names(reg[l]), length(reg[[l]]$Test.Result)))
                        legc <- c(legc, c(l+1))
                  }
                  if(length(legn) == 0){
                        dev.off()
                        message("Printed to ", paste0(t, "rqqplot_", names(dat_key)[t], "_reg.jpg"))
                  }else{
                        legend("topleft", legend = legn, col = legc, lty = 1)
                        #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
                        dev.off()
                        message("Printed to ", paste0(t, "rqqplot_", names(dat_key)[t], "_reg.jpg"))
                  }
            }
            
            #DENSITY
            if(dense){
                  jpeg(paste0(t, "rdensity_", names(dat_key)[t], ".jpg"), 1000,600)
                  d<-density(datk$Test.Result, na.rm = T)
                  plot(d)
                  polygon(d, col="light blue")
                  legn <- c()
                  legc <- c()
                  for(l in 1:length(reg)){
                        if(length(reg[[l]]$Test.Result) < 15){
                              next
                        }
                        lines(density(reg[[l]]$Test.Result), col = c(l+1))
                        legn <- c(legn, paste(names(reg[l]), length(reg[[l]]$Test.Result)))
                        legc <- c(legc, c(l+1))
                  }
                  if(length(legn) == 0){
                        dev.off()
                        message("Printed to ", paste0(t, "rdensity_", names(dat_key)[t], ".jpg"))
                  }else{
                        legend("topright", legend = legn, col = legc, lty = 1)
                        #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
                        dev.off()
                        message("Printed to ", paste0(t, "rdensity_", names(dat_key)[t], ".jpg"))
                  }
            }
            
            #BOXPLOT
            if(box){
                  jpeg(paste0("boxplot_", t, "_", names(dat_key)[t], "_reg.jpg"), 1000,800)
                  regf <- split(datk$Test.Result, datk$Registry)
                  #datl<-(split(datfix[[j]], datfix$Registry))
                  for(q in 1:length(reg)){
                        if(length(regf[[q]]) < 15){
                              regf[[q]] <- NA
                              #print("T")
                        }
                  }
                  if(all(is.na(regf))){
                        dev.off()
                        next
                  }
                  b <- boxplot(regf, plot = 0)
                  par(mar = c(9,4,1,1))
                  boxplot(regf, names = paste0(b$names, "(n=", b$n, ")"), las = 2)
                  dev.off()
                  message("Printed to ", paste0("boxplot_", t, "_", names(dat_key)[t], "_reg.jpg"))
            }
      }
      print(g)
}

datf <- dat[,-c(1,2,3,5,6,8,9,10,11,12,13), with = F]
datf_key <- (split(datf, datf$Test.Key))
names(datf_key)[68] <- "cd4-cd8_ratio"
names(datf_key)[70] <- "cd4_percent"
names(datf_key)[72] <- "cd8_percent"
#as.numeric(datf_key[[1]][,Test.Result])
char.nam <- names(which(lapply(datf_key, function(x) all(is.na(as.numeric(x[,Test.Result]))))==T))
key_mix <- datf_key[which(!(names(datf_key) %in% char.nam))]

datnum <- c()
datchar <- c()
datper <- c()
datcharlength <- c()
datlength <- c()
for(t in 1:length(key_mix)){
      testcol <- key_mix[[t]][,Test.Result]
      unval <- unique(testcol)
      if(any(is.na(unval))){
            unval <- unval[-which(is.na(unval))]
      }
      
      if(!any(is.na(as.numeric(unval)))){
            datnum <- c(datnum, names(key_mix[t]))
            next
      }else{
            datchar <- c(datchar, names(key_mix[t]))
      }
      charval <- unval[which(is.na(as.numeric(unval)))]
      
      testchar <- testcol[which(testcol %in% charval)]
      bplott <- table(testchar)
      
      if(any(is.na(testcol))){
           testcol <- testcol[-which(is.na(testcol))] 
      }
      
      tlength <- length(which(testcol %in% charval))
      per <- paste0(round(tlength/length(testcol), digits = 3), "%")
      datper <- c(datper, paste0(round(length(testchar)/length(testcol), digits = 5), "%"))
      datcharlength <- c(datcharlength, length(testchar))
      datlength <- c(datlength, length(testcol))
      
      #jpeg(paste0(t, "_mixed_barplot_cerner_", names(key_mix[t]), ".jpg"), 1200,800)
      #par(mar = c(13,4,1,1))
      #bplt <- barplot(bplott, col = c(2:30), legend = c(paste(rownames(bplott), "n =", bplott), paste("# of nums =", tlength), per), las = 2)
      ##text(x= bplt, y= bplott + (bplott/8), labels=as.character(bplott), xpd = T)
      #dev.off()
}

char_key <- datf_key[which(lapply(datf_key, function(x) all(is.na(as.numeric(x[,Test.Result]))))==T)]
for(b in 1:length(char_key)){
      if(length(unique(char_key[[b]][,Test.Result])) < 2){
            next
      }
      jpeg(paste0(b, "_barplot_", names(char_key[b]), ".jpg"), 1200,800)
      par(mar = c(13,4,1,1))
      barplot(table(char_key[[b]][,Test.Result]), col = c(2:500), 
            legend = c(paste(rownames(table(char_key[[b]][,Test.Result])),
            "n =", table(char_key[[b]][,Test.Result]))), las = 2)
      dev.off()
}

for(i in 1:length(datf_key)){
      if(class(datf_key[[i]][,Test.Result]) == "character"){
            print(paste(class(datf_key[[i]][,Test.Result]), i))
      }
}

#This code is for dealing with graphing absolute_lymphocites (t = 4) in a clean way due to different units
{
datk_names <- split(datk, datk$Test.Name)
datk_names[[2]]$Test.Result <- datk_names[[2]]$Test.Result * 1000
datk_names[[4]]$Test.Result <- datk_names[[4]]$Test.Result * 1000

for(n in 1:4){
      for(i in 1:2){
            out <- abs((datk_names[[n]]$Test.Result)-mean(datk_names[[n]]$Test.Result, na.rm = TRUE)) >
                  (6)*sd(datk_names[[n]]$Test.Result, na.rm = TRUE)
            if(T %in% out){
                  datk_names[[n]]<-datk_names[[n]][(-which(out == T)),]
                  rownames(out)<-NULL
            }else{
                  datk_names[[n]] <- datk_names[[n]]
            }
            if(all(is.na(datk_names[[n]]$Test.Result))){
                  print(paste(t, "na.skip"))
                  next
            }
      }
      reg <- split(datk_names[[n]], datk_names[[n]]$Registry)
      jpeg(paste0(t, "_", n, "_tqqplot_", names(datk_names)[n], ".jpg"), 1000,600)
      qqnorm(datk_names[[n]]$Test.Result)
      qqline(datk_names[[n]]$Test.Result)
      legn <- c()
      legc <- c()
      for(l in 1:length(reg)){
            if(length(reg[[l]]$Test.Result) < 30){
                  next
            }
            spoints <- qqnorm(reg[[l]]$Test.Result, plot.it = F)
            points(spoints, col = c(l+1))
            qqline((reg[[l]]$Test.Result), col = c(l+1))
            legn <- c(legn, paste(names(reg[l]), length(reg[[l]]$Test.Result)))
            legc <- c(legc, c(l+1))
      }
      if(length(legn) == 0){
            dev.off()
      }else{
            legend("topleft", legend = legn, col = legc, lty = 1)
            #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
            dev.off()
      }
      jpeg(paste0(t, "_", n, "_tdensity_", names(datk_names)[n], ".jpg"), 1000,600)
      d<-density(datk_names[[n]]$Test.Result, na.rm = T)
      plot(d)
      polygon(d, col="light blue")
      legn <- c()
      legc <- c()
      for(l in 1:length(reg)){
            if(length(reg[[l]]$Test.Result) < 30){
                  next
            }
            lines(density(reg[[l]]$Test.Result), col = c(l+1))
            legn <- c(legn, paste(names(reg[l]), length(reg[[l]]$Test.Result)))
            legc <- c(legc, c(l+1))
      }
      if(length(legn) == 0){
            dev.off()
            print(t)
      }else{
            legend("topright", legend = legn, col = legc, lty = 1)
            #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
            dev.off()
            print(t)  
      }
      #dev.off()
}
datkf <- rbindlist(datk_names)
for(i in 1:4){
      out <- abs((datkf$Test.Result)-mean(datkf$Test.Result, na.rm = TRUE)) >
            (6)*sd(datkf$Test.Result, na.rm = TRUE)
      if(T %in% out){
            datkf<-datkf[(-which(out == T)),]
            rownames(out)<-NULL
      }else{
            datkf <- datkf
      }
      if(all(is.na(datkf$Test.Result))){
            print(paste(t, "na.skip"))
            next
      }
}
datk<-datkf
}