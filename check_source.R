
setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-read.csv("cbc_results_source.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
#source("check_source.R")

check_source <- function(source, file = "cbc_results_source.csv", rm = T, corr = F, 
                         dense = F, box = F, file_name = paste0("cbc_results_", s[source], "_source.csv")){
      
      dat<-read.csv(file, stringsAsFactors = FALSE, na.strings = c("NA", ""))
      
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
      if(dense){
            for(k in 1:ncol(datf)){
                  g<-datf[[k]]
                  if(class(g) == "character"){
                        next
                  }else if(all(is.na(g))){
                        next
                  }
                  jpeg(paste0("density_",names(datf[k]),"_", source, "_", s[source], "_rm", ".jpg"), 1000,600)
                  d<-density(g, na.rm = T)
                  plot(d)
                  polygon(d, col="red")
                  dev.off()
            }
      }
      if(box){
            for(j in 4:ncol(datf)){
                  
                  if(class(datf[[j]]) == "character"){
                        next
                  }
                  datfix <- datf[-which(is.na(datf[[j]])),]
                  jpeg(paste0("boxplot_",names(dat[j]),"_", source, "_", s[source], "_rm.jpg"), 1000,800)
                  datl<-(split(datfix[[j]], datfix$Registry))
                  
                  #jpeg(paste0("boxplot_age_quan_",names(dat[j]),"_", source, "_", s[source], "_rm.jpg"), 1000,800)
                  #dage <- as.numeric(sub("null", NA, datfix$Age.at.Draw))
                  #datl <- split(datfix[[j]], cut(dage, quantile(dage, na.rm = T),include.lowest = T))
                  #This code will make boxplots of age quantiles instead of registry if un-commented
                  
                  if(any(lengths(datl) < 28)){
                        datl[which(lengths(datl) < 28)] <- NA
                  }
                  
                  if(source == 1){
                        datl <- datl[-c(7)]
                  }
                  
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
