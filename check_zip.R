
#source("check_zip.R")
#check_postal_codes("CSZ.csv")
check_postal_codes <- function(file, registry = "postal_codes.csv"){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      #set stringsAsFactors = FALSE so it doesn't print as a string
      reg <- read.csv(registry, stringsAsFactors = FALSE, colClasses = "character")
      #row.names(dat)<-as.character(1:nrow(dat))
      #reg <- read.csv("postal_codes.csv", stringsAsFactors = FALSE, colClasses = "character")
      #dat <- read.csv("CSZ.csv", stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      
      dat$state<-toupper(dat$state)
      
      dzip <- dat$zip
      
      reg_zip <- reg$zip
      
      
      #      fix_zip <- function(r){
      #            if(nchar(dzip[r]) == 10){
      #                  dzip[r] <- substr(dzip[r], 1,nchar(dzip[r])-5)
      #            }
      #           if(dat$state[r] %in% reg$state){
      #                  reg_zip_st <<- reg[which(reg$state == dat$state[r]),]
      #            }
      #            if(dat$state[r] %in% reg$state & !any(dzip[r] == reg_zip_st)){
      #                 dzip[r] <- NA
      #            }else if(!any(dzip[r] == reg_zip)){
      #                        dzip[r] <- NA
      #            }
      #           dzip[r]
      #      }
      #      dzip_fix<-sapply(1:nrow(dat), fix_zip, USE.NAMES = FALSE)
      
      #I tried to get it to just search through one state here to speed it up,
      #but it still goes slowly
      
      fix_zip <- function(r){
            
            if(nchar(r) == 10){
                  r <- substr(r, 1,nchar(r)-5)
            }
            if(!any(r == reg_zip)){
                  r <- NA
            }
            r
      }
      #using the %in% operator is another option, but it seems to take slightly more time
      
      #change to return NAs for other countries and missing values
#      dumb<-lapply(1:nrow(dat), fix_zip)
      
      dzip_fix<-sapply(dzip, fix_zip, USE.NAMES = FALSE)
      #dat$zip <- dzip_fix
      datf<-replace(dat,3,dzip_fix)
      #dumb is not a real variable, it's there for lapply to print to,
      #but it actually has no real purpose
      message("Postal Codes Fixed")
      
      #zip_int <- as.integer(dat$zip)
      
      #dzip <- dat$zip
      
      ziprow <- function(z){
            if(is.na(z)){
                  data.frame(city = "No Data",state = "No Data",zip = "No Data")
            }else{
                  reg[which(z == reg_zip),]
            }
      }
      
      #find the row which the zip code in dat$zip is in
     
      
      
      zip_list <- lapply(dzip_fix,ziprow)
      #NOTE: limiting this to first 100 for testing purposes
      #identical(dat[1,], zip_list)
      #this will check if theyre identical
      message("Zip Code Registry List Created")
      
      
      #cordat<-data.frame()
      
      check_zip <- function(n){
            if(all(datf[n,] == zip_list[[n]]) == TRUE){
                  corrow <- dat[n,]
            }else if(all(datf[n,] == zip_list[[n]]) == FALSE){
                  corrow <- zip_list[[n]]
            }
            #this checks the cities and states for if they're correct
            #print(corrow)
            corrow
      }
      
      
      cordat_mat<-sapply(1:nrow(dat), check_zip)
      message("Ordered According to Zip Code")
      cordat_list<-data.frame(t(as.data.frame(cordat_mat)))
      
      cordat<-data.frame(lapply(cordat_list,as.character), stringsAsFactors = FALSE)
      
      conf <- c()
#      confidence <- function(p){
#            lvl <- length(which(cordat[p,] == dat[p,]))
#            conf <- c(conf, lvl)
#            #print(conf)
#      }
      confidence <- function(p){
            lvl <- length(which(cordat[p,] == datf[p,]))
            conf <- c(conf, lvl)
            #print(conf)
      }
      confidence_level<-sapply(1:nrow(cordat), confidence)
      cordat<-cbind(cordat,confidence_level)
      message("Confidence Level Added")
      
      #check cities and states if confidence level is less than 3 and greater than 0
      
#      check_city <- function(l){
#            if((dat[l,1] == cordat[l,1]) & (dat[l,3] != cordat[l,3]) & (cordat[[l,1]] != "1")){
#                  print(l)
#           }
#      }
      
      check_conf <- function(l){
            if(cordat$confidence_level[l] == 1){
                  print(l)
            }
      }
      
      
      
      write.csv(cordat, "corrected_postal_codes.csv")
}


#There are 35 cases where the zip code typed isn't a real zip code, and is
#intended to be in the United States

#There are 47 cases where the confidence level is equal to 1, 1477 cases
#where it is equal to 2, 14241 where it's 3, and 4989 where it's 0(in which
#all but one are values that don't exist)

#When the confidence level takes 10 digit zip codes into account,
#there are only 15 cases. These are when the zip code doesn't match
#up with either the city or state. Most of these are mistypes of city
#names, but 3 of them (15456, 15747, and 17249) are in other countries
#with zip codes the same as possible codes in the US, so they aren't
#filtered out earlier


#start with just finding the function based off of the zipcode, assuming that
#it's correct, then move on to checking the zipcode based on city or state
#possible make a confidence score (0 is all correct, 1 is 1 change...)

#ignore foreign countries(including Canada, UK, Norway, Australia, India,
#Puerto Rico, Guam, and others)

#print in columns to the right of it correct data in CSZ
