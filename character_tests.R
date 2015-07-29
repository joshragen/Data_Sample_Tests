
setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-read.csv("cbc_results_tests.csv", stringsAsFactors = FALSE, na.strings = c("NA", "", "null"))

#NOTE, this code can require the csource object from the check_source2.R file, depending on what's being graphed
datv<-dat[c(1:7,which(grepl(".Value", names(dat))))]
reg<-split(datv, datv$Registry)
for(t in 1:336){
      if(dense){
            gap <- as.numeric(datv[[t]])
            gap <- gap[-which(is.na(gap))]
            if(length(gap) < 30){
                  next
            }
            for(i in 1:4){
                  out <- abs((gap)-mean(gap, na.rm = TRUE)) >
                        (6)*sd(gap, na.rm = TRUE)
                  if(T %in% out){
                        gap <- gap[(-which(out == T))]
                        rownames(out)<-NULL
                  }
            }
            jpeg(paste0(t, "_reg_density_", names(datv)[t], ".jpg"), 1000,600)
            d<-density(gap, na.rm = T)
            plot(d)
            polygon(d, col="light blue")
            legn <- c()
            legc <- c()
            for(i in 1){
                  for(c in 1:length(csource)){
                        if(nrow(csource[[c]]) < 30){
                              #print(nrow(csource[[c]]))
                              #print(c)
                              next
                        }
                        #print("here")
                        test <- as.numeric(csource[[c]][[t]])
                        test <- test[-which(is.na(test))]
                        if(length(test) < 25){
                              next
                        }
                        lines(density(test), col = c(c))
                        #print("done")
                        legn <- c(legn, paste(names(csource[c]), length(test)))
                        legc <- c(legc, c)
                  }
            }
            #This code prints lines for sources of the data points, requires csource with only ".Values"
            for(c in 1:length(reg)){
                  test <- as.numeric(reg[[c]][[t]])
                  test <- test[-which(is.na(test))]
                  if(length(test) < 15){
                        next
                  }
                  lines(density(test), col = c)
                  legn <- c(legn, paste(names(reg[c]), length(test)))
                  legc <- c(legc, c)
            }
            #This prints the lines by registry instead
            if(length(legn) == 0){
                  dev.off()
                  #message("Printed to ", paste0(t, "sdensity_", names(datv)[t], ".jpg"))
            }else{
                  legend("topright", legend = legn, col = legc, lty = 1)
                  #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
                  dev.off()
                  #message("Printed to ", paste0(t, "sdensity_", names(datv)[t], ".jpg"))
            }
      }
      if(box){
            btest <- c()
            cnam <- c()
            #for(c in 1:length(csource)){
            #                  if(nrow(csource[[c]]) < 30){
            #                        #print(nrow(csource[[c]]))
            #                        next
            #                  }
            #                  test <- as.numeric(csource[[c]][[t]])
            #                  test <- test[-which(is.na(test))]
            #                  
            #                  if(length(test) < 15){
            #                        next
            #                  }
            #                  for(i in 1:4){
            #                        out <- abs((test)-mean(test, na.rm = TRUE)) >
            #                              (6)*sd(test, na.rm = TRUE)
            #                        if(T %in% out){
            #                              test <- test[(-which(out == T))]
            #                              rownames(out)<-NULL}
            #                  }
            #                  #names(test) <- names(csource[c])
            #                  btest <- c(btest, list(test))
            #                  cnam <- c(cnam, names(csource[c]))
            #}
            #For printing out by csource
            for(c in 1:length(reg)){
                  test <- as.numeric(reg[[c]][[t]])
                  test <- test[-which(is.na(test))]
                  if(length(test) < 15){
                        next
                  }
                  for(i in 1:4){
                        out <- abs((test)-mean(test, na.rm = TRUE)) >
                              (6)*sd(test, na.rm = TRUE)
                        if(T %in% out){
                              test <- test[(-which(out == T))]
                              rownames(out)<-NULL}
                  }
                  btest <- c(btest, list(test))
                  cnam <- c(cnam, names(reg[c]))
            }
            
            names(btest) <- cnam
            if(length(btest) < 2){
                  next
            }
            jpeg(paste0(t, "_source_boxplot_", names(datv)[t], "_reg.jpg"), 1000,800)
            b <- boxplot(btest, plot = 0)
            par(mar = c(12,4,1,1))
            boxplot(btest, names = paste0(b$names, "(n=", b$n, ")"), las = 2)
            dev.off()
      }
}
#This large loop either makes boxplots or density curves.

apply(datv, 2, function(x) which(class(x) == "character"))
for(i in 1:ncol(datv)){
      if(class(datv[[i]]) == "character"){
            print(paste(class(datv[[i]]), i))
      }
}
#Eventually figure out a way to seperate values which are categorical and numeric in the same column
datvn <- data.frame(apply(datv, 2, function(x) as.numeric(x)))
which(apply(datvn, 2, function(x) all(is.na(x))))
datvc<-datv[which(apply(datvn, 2, function(x) all(is.na(x))))]
#write.csv(datvc, file = "Character_Values.csv")

charnam <- names(which(apply(datvn, 2, function(x) all(is.na(x)))))
datvf <- datv[which(!(colnames(datv) %in% charnam))]
#datvf is all value columns that are not entirely character vectors
datnum <- c()
datchar <- c()
for(t in 1:ncol(datvf)){
      testcol <- datvf[[t]]
      unval <- unique(testcol)
      unval <- unval[-which(is.na(unval))]
      
      if(!any(is.na(as.numeric(unval)))){
            #datnum <- c(datnum, names(datvf[t]))
            next
      }else{
            #datchar <- c(datchar, names(datvf[t]))
      }
      charval <- unval[which(is.na(as.numeric(unval)))]
      
      testchar <- testcol[which(testcol %in% charval)]
      bplott <- table(testchar)
      
      testcolf <- testcol[-which(is.na(testcol))]
      tlength <- length(which(!(testcolf %in% charval)))
      per <- paste0(round(tlength/length(testcolf), digits = 3), "%")
      
      jpeg(paste0(t, "_mixed_barplot_", names(datvf)[t], ".jpg"), 1200,800)
      par(mar = c(13,4,1,1))
      bplt <- barplot(bplott, col = c(2:30), legend = c(paste(rownames(bplott), "n =", bplott), paste("# of nums =", tlength), per), las = 2)
      #text(x= bplt, y= bplott + (bplott/8), labels=as.character(bplott), xpd = T)
      dev.off()
}

for(b in 1:ncol(datvc)){
      if(length(unique(datvc[[b]])) < 3){
            next
      }
      jpeg(paste0(b, "_barplot_", names(datvc)[b], ".jpg"), 1200,800)
      par(mar = c(13,4,1,1))
      barplot(table(datvc[[b]]), col = c(2:500),legend = c(paste(rownames(table(datvc[[b]])), "n =", table(datvc[[b]]))), las = 2)
      dev.off()
}
#makes barplot of the graphs
