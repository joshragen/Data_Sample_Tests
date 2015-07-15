
setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-read.csv("cbc_results_source.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))
#source("check_source.R")

check_source <- function(source, file = "cbc_results_source.csv", rm = T, dense = F, box = T, file_name = "cbc_results_source.csv"){
      
      dat<-read.csv(file, stringsAsFactors = FALSE, na.strings = c("NA", ""))
      
      s <- c("Quest Diagnostics", "VMMC", "VM")
      datf <- dat[which(apply(dat,1, function(x) any(grepl(s[source], x)))),]
      if(source == 3){
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
            for(j in 1:ncol(datf)){
                  
                  if(class(datf[[j]]) == "character"){
                        next
                  }
                  
                  datfix <- datf[-which(is.na(datf[[j]])),]
                  jpeg(paste0("boxplot_",names(dat[j]),"_", source, "_", s[source], "_rm.jpg"), 1000,800)
                  datl<-(split(datfix[[j]], datfix$Registry))
                  
                  if(any(lengths(datl) < 28)){
                        datl[which(lengths(datl) < 28)] <- NA
                  }
                  
                  #if(source == 1){
                  #      datl <- datl[-c(7)]
                  #}
                  
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
      
      if((rm = F) & (box = F)){
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