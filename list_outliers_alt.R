#'file' is name of csv file to use
#'test' is the column of the tests which the outliers are drawn from
#'reg' is the registry of results to return data from if sort = T
#'quan' is the quantile of age groups to return if split_age = T
#'file_name' is the name of the file which the final outliers are printed to
#'z' is the number of sd's from the mean to test (default = 3)
#'rm' is whether or not to remove outliers more than 2 times z standard deviations away,
#under the assumption that outliers this far removed are typos that are interfering with
#detecting real outliers in a reasonable range
#'sort' is whether or not to sort the results by registry, in which case 'reg' needs
#to have a valid registry from the data typed into it
#'split_age' is whether or not to divide the results by quantile of age groups

#     These are the different possible levels for registry
#     "Allergy", "Control", "Diabetes", "IBD", "IDRegistry", "Neurology",
#     "OutsideSamples", "PulmonaryDiseaseRegistry", "RDRegistry", "Transplant"


#setwd("C:/Users/jragen/Desktop/Sample_Tests")
#source("list_outliers_alt.R")
#list_outliers_alt("cbc_results.csv")

list_outliers_alt <- function(file, test = 4:l, reg = "", quan = "",
                              file_name = "outliers.csv",z = 3,
                              rm = F, sort = F, split_age = F){
      dat <- read.csv(file, stringsAsFactors = FALSE)
      l <- ncol(dat)
      #dat<-read.csv("cbc_results.csv", stringsAsFactors = FALSE)
      #z<-3
      outliers <- matrix(ncol = 2)
      #colnames(outliers) <- c("EVENT", "POINTS")
      
      if(split_age){
            if(quan %in% 1:4){
                  dage <- dat$ageAtDraw
                  dat <- split(dat, cut(dage, quantile(dage, na.rm = T),include.lowest = T))[[quan]]
                  file_name <- paste0("outliers_age_quantile_", quan,".csv")
            }else if(quan == ""){
                  stop("No Data Quantile Specified")
            }else if(!(quan %in% 1:4)){
                  stop("Quantile Not Valid")
            }
      }
      if(sort){
            if(reg %in% dat$registry){
                  dat <- (split(dat, dat$registry))[[reg]]
                  file_name <- paste0("outliers_", reg,".csv")
            }else if(reg == ""){
                  stop("No Registry Specified")
            }else if(!(reg %in% dat$registry)){
                  stop("Registry not Present in Data")
            }
      }
      for(i in test) {
            outlier_col<-NULL
            colmean <- c("None", "None")
            colint <- c("None", "None")
            skip <- F
            if(is.character(dat[[i]])){
                  outlier_col <- c("Non-Numerical Data", "")
                  skip <- T
            }else if(is.na(mean(dat[[i]], na.rm = TRUE))){
                  outlier_col <- c("No Data Present", "")
            }else{
                  if(rm){
                        file_name<-"outliers_rm.csv"
                        out <- abs((dat[i])-mean(dat[[i]], na.rm = TRUE)) >
                              (z * 2)*sd(dat[[i]], na.rm = TRUE)
                        if(T %in% out){
                              datf<-dat[(-which(out == T)),]
                              rownames(out)<-NULL
                        }else{
                              datf <- dat
                        }
                        
                        out <- abs((datf[i])-mean(datf[[i]], na.rm = TRUE)) >
                              z*sd(datf[[i]], na.rm = TRUE)

                        outlier_col <- data.frame(EVENT = dat$event[which(out == TRUE)],
                              POINTS = datf[[i]][which(out == TRUE)],stringsAsFactors=FALSE)
                        colmean <- c("Mean of Column", mean(datf[[i]], na.rm = TRUE))
                        colint <- c("Confidence Interval", 
                              paste(c(mean(datf[[i]], na.rm = TRUE) + z*sd(datf[[i]], na.rm = TRUE)),
                              c(mean(datf[[i]], na.rm = TRUE) - z*sd(datf[[i]], na.rm = TRUE)), sep = ", "))
                  }else{
                        out <- abs((dat[i])-mean(dat[[i]], na.rm = TRUE)) >
                        z*sd(dat[[i]], na.rm = TRUE)
                        
                        outlier_col <- data.frame(EVENT = dat$event[which(out == TRUE)],
                              POINTS = dat[[i]][which(out == TRUE)],stringsAsFactors=FALSE)
                        colmean <- c("Mean of Column", mean(dat[[i]], na.rm = TRUE))
                        colint <- c("Confidence Interval", 
                              paste(c(mean(dat[[i]], na.rm = TRUE) + z*sd(dat[[i]], na.rm = TRUE)), ",",
                              c(mean(dat[[i]], na.rm = TRUE) - z*sd(dat[[i]], na.rm = TRUE))))
                  }
            }
            if(skip){next}
            outlier_tab <- rbind(colmean, colint, outlier_col)
            colnames(outlier_tab) <-c("event_name", colnames(dat[i]))
            list_data_frames <- list(outliers, outlier_tab)
            max_row <- max(unlist(lapply(list_data_frames, nrow), use.names = F))
            
            fix_rows <- function(r) {
                  count <- max_row - nrow(r)
                  if (count > 0) {
                        matrow <- matrix("", count, ncol(r))
                        colnames(matrow) <- colnames(r)
                        rbind(r, matrow)
                  } else {
                        r
                  }
            }
            dat_list <- lapply(list_data_frames, fix_rows)
            outliers<-data.frame(do.call(cbind,dat_list))
      }
      outliers <- outliers[-c(1,2)]
      if(length(test) < 2){print(outliers)}
      write.csv(outliers, file_name)
      message(paste("Outliers printed to", file_name))
}