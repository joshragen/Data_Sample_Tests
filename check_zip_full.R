
#source("check_zip_final.R")
#check_postal_codes("CSZ.csv")
#check_postal_codes("CSZ.csv", registry = "postal_codes.csv", file_name = "new_corrected_postal_codes.csv")
check_postal_codes <- function(file, registry = "full_postal_codes.csv", file_name = "corrected_postal_codes.csv"){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      reg <- read.csv(registry, stringsAsFactors = FALSE, colClasses = "character")
      #reg <- read.csv("full_postal_codes.csv", stringsAsFactors = FALSE, colClasses = "character")
      #dat <- read.csv("CSZ.csv", stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      
      message("Starting... (0 of 5)")
      
      datc<-dat
      datc$state<-toupper(dat$state)
      datc$city<-toupper(dat$city)
      regc<-reg
      regc$city<-toupper(reg$city)
      dzip <- datc$zip
      reg_zip <- regc$zip
      
      fix_zip <- function(r){
            if(nchar(r) == 10){
                  r <- substr(r, 1,nchar(r)-5)
            }
            if(!any(r == reg_zip)){
                  r <- NA
            }
            r
      }
      
      #Look at having it check whether or not state and city are correct, if they are,
      #then check if state is, then zip. This way, it has to run less through the 
      #check_zip function, causing it to run faster overall
      
      dzip_fix<-sapply(dzip, fix_zip, USE.NAMES = FALSE)
      datf<-replace(datc,3,dzip_fix)
      message("Postal Codes Fixed (1 of 5)")
      
#      ziprow <- function(z){
#            if(is.na(z)){
#                  data.frame(city = "No Data",state = "No Data",zip = "No Data")
#            }else{
#                  regc[which(z == reg_zip),]
#            }
#      }
#      
#      zip_list <- lapply(dzip_fix,ziprow)
#      
#      message("Zip Code Registry List Created (2 of 5)")
#      
      #1
#      check_zip <- function(n){
#            reg_row <- which(duplicated(rbind(datf[n,], regc))) - nrow(datf[n,])
#            if(all(any(datf[n,] == regc[reg_row,])) == TRUE){
#                  corrow <- datf[n,]
#                  print(corrow)
#            }else if(all(datf[n,] == zip_list[[n]]) == FALSE){
#                  corrow <- zip_list[[n]]
#            }
#            corrow
#      }
      #2
#      check_zip <- function(n){
#            reg_row <- which(duplicated(rbind(datf[n,], regc))) - nrow(datf[n,])
#            if(identical(c(datf[n,]), c(regc[reg_row,]))){
#                  corrow <- datf[n,]
#
#            }else if(!identical(c(datf[n,]), c(regc[reg_row,]))){
#                  corrow <- regc[which(datf$zip[n] == reg_zip),][1,]
#
#            }
#      }

      #3      
#      check_zip <- function(n){
#            
#            reg_row <- regc[which(dzip_fix[n] == reg_zip),]
#            if(nrow(reg_row) > 1){
#                  if(any(datf$city[n] == reg_row$city)){
#                        reg_row <- reg_row[which(datf$city[n] == reg_row$city),]
#                  }else{
#                        reg_row <- reg_row[1,]
#                  }
#            }else if(is.na(dzip_fix[n])){
#                  reg_row <- matrix(ncol = 3)
#            }
#            
#            if(identical(c(datf[n,]), c(reg_row))){
#                  corrow <- reg_row
#            }else if(!identical(c(datf[n,]), c(reg_row))){
#                  corrow <- reg_row
#            }
#            
#            corrow
#      }
      #4
      check_zip <- function(n){
            
            reg_row <- reg[which(dzip_fix[n] == reg_zip),]
            reg_rowC <- reg_row
            reg_rowC$city <- toupper(reg_rowC$city)
            reg_rowC$state <- toupper(reg_rowC$state)
            
            #reg_rowc <- data.frame(toupper(reg_row$city), toupper(reg_row$state), reg_row$zip)
            if(nrow(reg_row) > 1){
                  if(any(datf$city[n] == reg_rowC$city)){
                        reg_row <- reg_row[which(datf$city[n] == reg_rowC$city),]
                  }else{
                        reg_row <- reg_row[1,]
                  }
            }else if(is.na(dzip_fix[n])){
                  reg_row <- matrix(ncol = 3)
            }
            reg_row
      }
      
      
      #identical(c(datf[n,]), c(regc[31345,]))
      #Filter(function(x) x > 0, which(duplicated(rbind(datf[n,], regc))) - nrow(datf[n,]))
      #which(duplicated(rbind(datf[n,], regc))[-seq_len(nrow(datf[n,]))])
      
      cordat_mat<-sapply(1:nrow(dat), check_zip)
      message("Ordered According to Zip Code (3 of 5)")
      cordat_list<-data.frame(t(as.data.frame(cordat_mat)))
      
      cordat<-data.frame(lapply(cordat_list,as.character), stringsAsFactors = FALSE)
      
      conf <- c()

      confidence <- function(p){
            lvl <- length(which(toupper(cordat[p,]) == datf[p,]))
            conf <- c(conf, lvl)
      }
      confidence_level<-sapply(1:nrow(cordat), confidence)
      cordat<-cbind(cordat,confidence_level)
      message("Confidence Level Added (4 of 5)")
      #Confidence level is measure of how different the new data is from the original,
      #if it's 1, then it's worth looking at whats going on
      
      
      for(l in 1:nrow(dat)){
            if(cordat$confidence_level[l] == 1){
                  if(any((datf$city[l] == regc$city) & (datf$state[l] == regc$state))){
                        cor_reg <- reg[which((datf$city[l] == regc$city) &
                                          (datf$state[l] == regc$state)),]
                        if(any(cordat$zip[l] != cor_reg$zip)){
                              cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                                    "; City and State don't match zip")
                        }
                  }
            }else if(((is.na(datf$zip[l])) & (datf$city[l] != "") & (datf$state[l] != ""))){
                  if(!any((datf$city[l] == regc$city) & (datf$state[l] == regc$state))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                              "; City and State combination not in registry, either in other country or typo present")
                  }
            }else if((datf$city[l] == "") & (datf$state[l] == "")){
                  cordat$confidence_level[l] <- paste(cordat$confidence_level[l], "; No Data Present")
            }
            if(cordat$confidence_level[l] == 0){
                  if(any((datf$city[l] == regc$city) | (datf$state[l] == regc$state)) & (!any(dzip[l] == reg_zip))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], "; Zip Code Doesn't Match Registry")
                  }
            }
      }
      
      message("Marked Incorrect Cities and States(5 of 5)")
      
      final_table <- cbind(Original_Values = "", dat, Corrected_Values ="",cordat)
      
      write.csv(final_table, file_name)
      message("Done")
}