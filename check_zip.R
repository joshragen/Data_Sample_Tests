

check_postal_codes <- function(file, registry = "postal_codes.csv"){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      #set stringsAsFactors = FALSE so it doesn't print as a string
      reg <- read.csv(registry, stringsAsFactors = FALSE)
      #row.names(dat)<-as.character(1:nrow(dat))
      #reg <- read.csv("postal_codes.csv", stringsAsFactors = FALSE)
      #dat <- read.csv("CSZ.csv", stringsAsFactors = FALSE)[,c("city", "state", "zip")]
      
      dzip <- dat$zip
      
      fix_zip <- function(r){
            if(nchar(dzip[r]) == 10){
                  dat$zip[r] <<- substr(dzip[r], 1,nchar(dzip[r])-5)
            }else if(nchar(dzip[r]) > 5){
                  dat$zip[r] <<- substr(dzip[r], 0,nchar(dzip[r])-nchar(dzip[r]))
                  #NOTE: this currently deletes the entire zip code there,
                  #replacing it with an empty value so the rest of the code
                  #skips over it, change later
            }else if(nchar(dzip[r]) == 0){
                  dat$zip[r] <<- "No Data"
            }
            integer(0)
      }
      #change to return NAs for other countries and missing values
      dumb<-lapply(1:nrow(dat), fix_zip)
      #dumb is not a real variable, it's there for lapply to print to,
      #but it actually has no real purpose
      message("Postal Codes Fixed")
      
      
      ziprow <- function(z){
            if(is.na(z)){
                  data.frame(city = "No Data",state = "No Data",zip = "No Data")
            }else if(!any(z == reg_zip)){
                  data.frame(city = "Not in Reg",state = "Not in Reg",zip = "Not in Reg")
            }else{
                  reg[which(z == reg_zip),]
            }
      }
      
      #find the row which the zip code in dat$zip is in
     
      zip_int <- as.integer(dat$zip)
      reg_zip <- reg$zip
      
      zip_list <- lapply(zip_int,ziprow)
      #NOTE: limiting this to first 100 for testing purposes
      #identical(dat[1,], zip_list)
      #this will check if theyre identical
      message("Zip Code Registry List Created")
      
      
      cordat<-data.frame()
      
      check_zip <- function(n){
            if(all(dat[n,] == zip_list[[n]]) == TRUE){
                  corrow <- dat[n,]
            }else if(all(dat[n,] == zip_list[[n]]) == FALSE){
                  corrow <- zip_list[[n]]
            }
            #this checks the cities and states for if they're correct
            corrow
            #<<- rbind(cordat, corrow)
            #integer(0)
      }
      
      
      cordat_mat<-sapply(1:nrow(dat), check_zip)
      message("Ordered According to Zip Code")
      cordat_list<-data.frame(t(as.data.frame(cordat_mat)))
      
      cordat<-data.frame(lapply(cordat_list,as.character), stringsAsFactors = FALSE)
      write.csv(cordat, "corrected_postal_codes.csv")
}


#(which(dat[2,] == zip_list[[2]])== c(1,2,3)) == TRUE
#row.names(zip_list[[2]])<-as.numeric(row.names(dat[2,]))



#start with just finding the function based off of the zipcode, assuming that
#it's correct, then move on to checking the zipcode based on city or state
#possible make a confidence score (0 is all correct, 1 is 1 change...)

#ignore foreign countries(including Canada, UK, Norway, Australia, India,
#Puerto Rico, Guam, and others)

#print in columns to the right of it correct data in CSZ
