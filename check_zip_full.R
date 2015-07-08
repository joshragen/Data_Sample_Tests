
#source("check_zip_full.R")
#check_postal_codes("CSZ.csv")
#check_postal_codes("CSZ.csv", registry = "postal_codes.csv", file_name = "new_corrected_postal_codes.csv")
check_postal_codes <- function(file, registry = "full_postal_codes.csv", file_name = "corrected_postal_codes.csv"){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      reg <- read.csv(registry, stringsAsFactors = FALSE, colClasses = "character")
      #reg <- read.csv("full_postal_codes.csv", stringsAsFactors = FALSE, colClasses = "character")
      #dat <- read.csv("CSZ.csv", stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      
      message("Starting... (0 of 4)")
      
      datC<-dat
      datC$state<-toupper(dat$state)
      datC$city<-toupper(dat$city)
      regC<-reg
      regC$city<-toupper(reg$city)
      dzip <- datC$zip
      reg_zip <- reg$zip
      
      fix_zip <- function(r){
            if(nchar(dzip[r]) == 10){
                  dzip[r] <- substr(dzip[r], 1,nchar(r)-5)
            }
            if(!any(dzip[r] == reg_zip)){
                  dzip[r] <- NA
            }
            dzip[r]
      }
      
      dzip_fix<-sapply(seq_along(dzip), fix_zip, USE.NAMES = FALSE)
      datf<-replace(datC, 3,dzip_fix)
      message("Postal Codes Fixed (1 of 4)")
      
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
      
      cordat_mat<-sapply(1:nrow(dat), check_zip)
      message("Ordered According to Zip Code (2 of 4)")
      cordat_list<-data.frame(t(as.data.frame(cordat_mat)))
      
      cordat<-data.frame(lapply(cordat_list,as.character), stringsAsFactors = FALSE)
      
      conf <- c()
      confidence <- function(p){
            lvl <- length(which(toupper(cordat[p,]) == datf[p,]))
            conf <- c(conf, lvl)
      }
      confidence_level<-sapply(1:nrow(cordat), confidence)
      cordat<-cbind(cordat,confidence_level)
      message("Confidence Level Added (3 of 4)")
      #Confidence level is measure of how different the new data is from the original,
      #if it's 0 or 1, then it's worth looking at whats going on
      
      
      for(l in 1:nrow(dat)){
            if(cordat$confidence_level[l] == 1){
                  if(any((datf$city[l] == regC$city) & (datf$state[l] == regC$state))){
                        cor_reg <- reg[which((datf$city[l] == regC$city) &
                                          (datf$state[l] == regC$state)),]
                        if(any(cordat$zip[l] != cor_reg$zip)){
                              cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                                    "; City and State don't match zip")
                        }
                  }
            }else if(((is.na(datf$zip[l])) & (datf$city[l] != "") & (datf$state[l] != ""))){
                  if(!any((datf$city[l] == regC$city) & (datf$state[l] == regC$state))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                              "; City and State combination not in registry, either in other country or typo present")
                  }
            }else if((datf$city[l] == "") & (datf$state[l] == "")){
                  cordat[l,] <- c("","","",paste(cordat$confidence_level[l], "; No Data Present"))
            }
            if(cordat$confidence_level[l] == 0){
                  if(any((datf$city[l] == regC$city) | (datf$state[l] == regC$state)) & (!any(dzip[l] == reg_zip))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], "; Zip Code Doesn't Match Registry")
                  }
            }
      }
      
      message("Marked Incorrect Cities and States (4 of 4)")
      
      final_table <- cbind(Original_Values = "", dat, Corrected_Values ="",cordat)
      
      write.csv(final_table, file_name)
      message("Done")
}