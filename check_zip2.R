
#source("check_zip2.R")
#check_postal_codes("CSZ.csv")
check_postal_codes <- function(file, registry = "postal_codes.csv"){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      #set stringsAsFactors = FALSE so it doesn't print as a string
      reg <- read.csv(registry, stringsAsFactors = FALSE, colClasses = "character")
      #reg <- read.csv("postal_codes.csv", stringsAsFactors = FALSE, colClasses = "character")
      #dat <- read.csv("CSZ.csv", stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      
      message("Starting...(0 of 5)")
      
      dat$state<-toupper(dat$state)
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
      #using the %in% operator is another option, but it seems to take slightly more time
      
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
                  corrow <- dat[n,]
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
            #NOTE, making this be dat will cause it to not incorporate cases
            #where there is a 9 digit zip code, and puts that into the confidence
            #interval as well, datf will ignore this
            conf <- c(conf, lvl)
      }
      confidence_level<-sapply(1:nrow(cordat), confidence)
      cordat<-cbind(cordat,confidence_level)
      message("Confidence Level Added (4 of 5)")
      
      
      for(l in 1:nrow(dat)){
            if(cordat$confidence_level[l] == 1){
                  if(any((datf[l,1] == reg[,1]) & (datf[l,2] == reg[,2]))){
                        cor_reg <- reg[which((datf$city[l] == reg$city) &
                                          (datf$state[l] == reg$state)),]
                        if(any(cordat$zip[l] != cor_reg$zip)){
                              cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                                    "; City and State don't match zip, originally",
                                    datf$city[l], "and", datf$state[l])
                        }
                  }
            }else if(((is.na(datf$zip[l])) & (datf$city[l] != ""))){
                  if(!any((datf$city[l] == reg$city) & (datf$state[l] == reg$state))){
                        cordat$confidence_level[l] <- paste(cordat$confidence_level[l], 
                              "; City and State combination not in registry, either in other country or typo present; originally",
                              dat$city[l], ",", dat$state[l], ",", dat$zip[l])
                  }
            }else if(datf$city[l] == ""){
                  cordat$confidence_level[l] <- paste(cordat$confidence_level[l], "; No Data Present")
            }
      }
      
      message("Marked Incorrect Cities and States(5 of 5)")
      
      write.csv(cordat, "corrected_postal_codes.csv")
}


#source("check_zip2.R")
#fineName <- "Rprof_test.txt"
#Rprof(fineName)
#cpc <- check_postal_codes("CSZ.csv")
#Rprof(NULL)
#summaryRprof(fineName)

#NOTE: run this ^^^^^^^ to check the time that parts of this function are taking

#There are 35 cases where the zip code typed isn't a real zip code, and is
#intended to be in the United States

#There are 47 cases where the confidence level is equal to 1, 1477 cases
#where it is equal to 2, 14241 where it's 3, and 4989 where it's 0(in which
#all but one are values that don't exist)

#When the confidence level takes 9 digit zip codes into account,
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
