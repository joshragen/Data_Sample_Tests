
#source("check_zip_final.R")
#check_postal_codes("CSZ.csv")
check_postal_codes <- function(file, registry = "postal_codes.csv"){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      reg <- read.csv(registry, stringsAsFactors = FALSE, colClasses = "character")
      
      message("Starting... (0 of 5)")
      
      dat$state<-toupper(dat$state)
      dat$city<-toupper(dat$city)
      reg$city<-toupper(reg$city)
      dzip <- dat$zip
      reg_zip <- reg$zip
      
      fix_zip <- function(r){
            if(nchar(r) == 10){
                  r <- substr(r, 1,nchar(r)-5)
            }
            if(!any(r == reg_zip)){
                  r <- NA
            }
            r
      }
      
      dzip_fix<-sapply(dzip, fix_zip, USE.NAMES = FALSE)
      datf<-replace(dat,3,dzip_fix)
      message("Postal Codes Fixed (1 of 5)")
      
      ziprow <- function(z){
            if(is.na(z)){
                  data.frame(city = "No Data",state = "No Data",zip = "No Data")
            }else{
                  reg[which(z == reg_zip),]
            }
      }
      
      zip_list <- lapply(dzip_fix,ziprow)

      message("Zip Code Registry List Created (2 of 5)")
      
      check_zip <- function(n){
            if(all(datf[n,] == zip_list[[n]]) == TRUE){
                  corrow <- datf[n,]
            }else if(all(datf[n,] == zip_list[[n]]) == FALSE){
                  corrow <- zip_list[[n]]
            }
            corrow
      }
      
      cordat_mat<-sapply(1:nrow(dat), check_zip)
      message("Ordered According to Zip Code (3 of 5)")
      cordat_list<-data.frame(t(as.data.frame(cordat_mat)))
      
      cordat<-data.frame(lapply(cordat_list,as.character), stringsAsFactors = FALSE)
      
      conf <- c()

      confidence <- function(p){
            lvl <- length(which(cordat[p,] == datf[p,]))
            conf <- c(conf, lvl)
      }
      confidence_level<-sapply(1:nrow(cordat), confidence)
      cordat<-cbind(cordat,confidence_level)
      message("Confidence Level Added (4 of 5)")
      #Confidence level is measure of how different the new data is from the original,
      #if it's 1, then it's worth looking at whats going on
      
      
      for(l in 1:nrow(dat)){
            if(cordat$confidence_level[l] == 1){
                  if(any((datf[l,1] == reg[,1]) & (datf[l,2] == reg[,2]))){
                        cor_reg <- reg[which((datf$city[l] == reg$city) &
                                          (datf$state[l] == reg$state)),]
                        if(any(cordat$zip[l] != cor_reg$zip)){
                              cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                                    "; City and State don't match zip")
                        }
                  }
            }else if(((is.na(datf$zip[l])) & (datf$city[l] != "") & (datf$state[l] != ""))){
                  if(!any((datf$city[l] == reg$city) & (datf$state[l] == reg$state))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                              "; City and State combination not in registry, either in other country or typo present")
                  }
            }else if((datf$city[l] == "") & (datf$state[l] == "")){
                  cordat$confidence_level[l] <- paste(cordat$confidence_level[l], "; No Data Present")
            }
      }
      
      message("Marked Incorrect Cities and States(5 of 5)")
      
      final_table <- cbind(Original_Values = "", dat, Corrected_Values ="",cordat)
      
      write.csv(final_table, "Corrected_Postal_Codes.csv")
      message("Done")
}