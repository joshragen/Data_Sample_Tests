
setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-read.csv("cbc_results_tests.csv", stringsAsFactors = FALSE, na.strings = c("NA", "", "null"))
#source("check_source.R")

dat<-dat[-which(is.na(dat$BMI)),]
dat<-dat[-which(is.na(dat$Height..cms.)),]

check_source <- function(source, file = "cbc_results_source.csv", rm = T, corr = F, 
                         dense = F, box = F, file_name = paste0("cbc_results_", s[source], "_source.csv")){
      
      #dat<-read.csv(file, stringsAsFactors = FALSE, na.strings = c("NA", ""))
      
      s <- c("Quest Diagnostics", "VMMC", "VM")
      datf <- dat[which(apply(dat,1, function(x) any(grepl(s[source], x)))),]
      if(source == 1){
            for(a in 1:5){
                  for(q in 1:nrow(datf)){
                        if(any(grepl("VM", datf[q,]))){
                              datf <- datf[-q,]
                        }
                  }
            }
      }else if(source == 2){
            for(a in 1:5){
                  for(q in 1:nrow(datf)){
                        if(any(grepl("Quest", datf[q,]))){
                              datf <- datf[-q,]
                        }
                  }
            }
            #q <- 1
            #datfx <- datf
            #while(q <= nrow(datfx)){
            #      if(any(grepl("VM", datfx[q,]))){
            #            datfx <- datfx[-q,]
            #      }
            #      q <- q + 1
            #      print(q)
            #}
            #This code could be a better solution, make work if you have the time
      }else if(source == 3){
            datf <- datf[-which(apply(datf,1, function(x) any(grepl("VMMC", x)))),]
      }
      if(rm){
            for(i in 1:ncol(datf)){
                  if(class(datf[[i]]) == "character"){
                        next
                  }else if(all(is.na(datf[[i]]))){
                        next
                  }
                  out <- abs((datf[i])-mean(datf[[i]], na.rm = TRUE)) >
                        (6)*sd(datf[[i]], na.rm = TRUE)
                  if(T %in% out){
                        datf<-datf[(-which(out == T)),]
                        rownames(out)<-NULL
                  }
            }
      }
      if(corr){
            num_col <- c()
            for(i in 1:ncol(datf)){
                  if(all(is.na(datf[[i]]))){
                        next
                  }else if(is.numeric(datf[[i]])){
                        num_col <- append(num_col, i)
                  }else{
                        next
                  }
            }
            ndat <- datf[num_col]
            corrdat <- as.data.frame(cor(ndat[seq(ncol(ndat))], use = "pairwise.complete.obs"))
            write.csv(corrdat, file_name)
      }
      #ndat <- apply(dat,2, function(x) split(x,is.numeric(x)))
      
      #datg <- split(datf, datf())
      if(dense){
            for(k in 1:ncol(datf)){
                  g<-datf[[k]]
                  if(class(g) == "character"){
                        next
                  }else if(all(is.na(g))){
                        next
                  }
                  datfix <- datf[-which(is.na(datf[[k]])),]
                  datg <- split(datfix, datfix$Gender)
                  if(length(datfix[[k]]) < 30){
                        next
                  }
                  jpeg(paste0("density_",names(datf[k]),"_", source, "_", s[source], "_gender_rm", ".jpg"), 1000,600)
                  d<-density(g, na.rm = T)
                  plot(d)
                  polygon(d, col="red")
                  legn <- c()
                  legc <- c()
                  for(t in 1:length(datg)){
                        #gen <- datg[[t]][[k]][-which(is.na(datg[[t]][[k]]))]
                        if(length(datg[[t]][[k]]) < 10){
                              next
                        }
                        lines(density(datg[[t]][[k]], na.rm = T), col = c(t+2))
                        legn <- c(legn, paste(names(datg[t]), length(datg[[t]][[k]])))
                        legc <- c(legc, c(t+2))
                  }
                  if(length(legn) == 0){
                        dev.off()
                        print(k)
                  }else{
                        legend("topright", legend = legn, col = legc, lty = 1)
                        #legend("topright", legend = legn, col = legc, lty = 1, cex = 0.75, y.intersp = 0.5, bty = "n")
                        dev.off()
                        print(k)  
                  }
                  #dev.off()
            }
      }
      if(box){
            for(j in 1:ncol(datf)){
                  if(class(datf[[j]]) == "character"){
                        next
                  }
                  datfix <- datf[-which(is.na(datf[[j]])),]
                  if(length(datfix[[j]]) < 30){
                        next
                  }
                  jpeg(paste0("boxplot_",names(dat[j]),"_", source, "_", s[source], "_registry_rm.jpg"), 1000,800)
                  datl<-(split(datfix[[j]], datfix$Registry))
                  
                  #jpeg(paste0("boxplot_age_quan_",names(dat[j]),"_", source, "_", s[source], "_rm.jpg"), 1000,800)
                  #dage <- as.numeric(sub("null", NA, datfix$Age.at.Draw))
                  #datl <- split(datfix[[j]], cut(dage, quantile(dage, na.rm = T),include.lowest = T))
                  #This code will make boxplots of age quantiles instead of registry if un-commented
                  
                  if(any(lengths(datl) < 28)){
                        #datl <- datl[-which(lengths(datl) < 28)]
                        datl[which(lengths(datl) < 4)] <- NA
                  }
                  
                  #if(source == 1){
                  #      datl <- datl[-c(7)]
                  # }
                  
                  if(all(is.na(datl))){
                        dev.off()
                        next
                  }
                  b <- boxplot(datl, plot = 0)
                  par(mar = c(9,4,1,1))
                  boxplot(datl, names = paste0(b$names, "(n=", b$n, ")"), las = 2)
                  dev.off()
            }
      }
      if((rm == F) & (box == F) & (corr == F)){
            write.csv(datf, file_name)
      }
}

#for(i in 1:nrow(datf)){
#      if(is.na(datf$absolute_neutrophils_value[i])){
#            next
#      }
#      if(datf$absolute_neutrophils_value[i] < 20){
#            datf$absolute_neutrophils_value[i] <- datf$absolute_neutrophils_value[i] * 1000
#      }
#}
#This is because abs_neutrophils_data has many unusual values, so it corrected that data to look more normal

datts<-dat[which(grepl(".Source", names(dat)))]
dattv<-as.vector(as.matrix(datts))
dattv<-dattv[-which(is.na(dattv))]
sources <- unique(dattv)
ord<-c()
for(i in 1:length(unique(dattv))){
      #ord<-c(ord, paste(length(which(dattv == unique(dattv)[i])), unique(dattv)[i]))
      ord<-c(ord, length(which(dattv == unique(dattv)[i])))
}
names(ord) <- sources
     
rnam <- c()
for(u in 1:length(unique(dattv))){
      nam <- unique(dattv)[u]
      nam_len <- c()
      dattf <- datts[,!which(!grepl(nam, datts)), with = F]
      for(r in 1:nrow(dattf)){
            if(nam %chin% as.character(dattf[r])){
                  nam_len <- c(nam_len, r)
            }
      }
      rnam <- c(rnam, list(nam_len))
      print(nam)
}
#This code takes a long time to run, saved in "rows_of_sources.csv", code to extract below
rnam<-read.csv("rows_of_sources.csv", stringsAsFactors = FALSE, na.strings = c("NA", "", "null"))
rnam<-rbind(rnam,NA)
#if this isn't here, the lapply breaks at 20 because there aren't any NA
rlist<-as.list(rnam)
lst<- lapply(rlist, function(x) x[-which(is.na(x))])
names(lst) <- sources

csource <- c()
for(d in 1:length(lst)){
      sdat <- dat[lst[[d]],]
      for(c in seq(10,ncol(sdat), 3)){
            if(sources[d] %chin% as.character(sdat[,c])){
                  if(any(is.na(sdat[c]))){
                        sdat[which(is.na(sdat[c])),c(c-2,c-1,c)] <- NA
                  }else{
                        sdat[which(sdat[c] != sources[d]), c(c-2,c-1,c)] <- NA  
                  }
            }else{
                  sdat[c(c-2,c-1,c)] <- NA
            }
      }
      csource <- c(csource, list(sdat))
      print(sources[d])
}
names(csource) <- sources
#Pick up with examining csource, which is all rows sorted by the name of the source
fix_rows <- function(r) {
      count <- max_row - nrow(r)
      if (count > 0) {
            matrow <- matrix(NA, count, ncol(r))
            colnames(matrow) <- colnames(r)
            rbind(r, matrow)
      } else {
            r
      }
}
csource <- c()
for(d in 1:length(lst)){
      sdat <- dat[lst[[d]],]
      datf <- matrix()
      for(c in 1:ncol(sdat)){
            if(sources[d] %chin% as.character(sdat[,c])){
                  #crow <- c(as.numeric(rownames(sdat)[which(sdat[c] == sources[d])]))
                  crow <- which(sdat[c] == sources[d])
                  co <-data.frame(row = crow, col = c)
                  rvalue <- sdat[as.matrix(data.frame(row = co$row, col = co$col-2))]
                  runit <- sdat[as.matrix(data.frame(row = co$row, col = co$col-1))]
                  rsource <- sdat[as.matrix(co)]
                  #coord <- rbind(coord, co)
                  coord <- data.frame(cbind(rvalue,runit,rsource))
                  names(coord)<-c(names(sdat)[c-2], names(sdat)[c-1], names(sdat)[c])
                  list_data_frames <- list(datf, coord)
                  max_row <- max(unlist(lapply(list_data_frames, nrow), use.names = F))
                  
                  dat_list <- lapply(list_data_frames, fix_rows)
                  datf<-data.frame(do.call(cbind,dat_list))
            }
      }
      datf <- datf[-1]
      csource <- c(csource, list(datf))
      print(sources[d])
}
#This code is considerably slower and makes the list with just the 3 columns for each
#test with the correct source, instead of replacing all other columns with NA