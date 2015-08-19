#'file' is name of csv file to use
#'z' is the number of sd's from the mean to test (default = 3)
#'test' is the column of the tests which the outliers are drawn from
#'file_name' is the name of the file which the final outliers are printed to

#source("list_outliers2.R")
#list_outliers("cbc_results.csv")

list_outliers <- function(file, test = 4:ncol(dat), file_name = "outliers.csv", z = 3){
      
      dat <- read.csv(file, stringsAsFactors = FALSE)
      #dat<-read.csv("cbc_results.csv", stringsAsFactors = FALSE)
      #z<-3
      
      outliers <- matrix(ncol = 2)
      #colnames(outliers) <- c("EVENT", "POINTS")
      
      for(i in test) {
            outlier_col<-NULL
            if(is.character(dat[[i]])){
                  outlier_col <- c("Non-Numerical Data", "")
                  #namerow <- c("Name of Data", colnames(dat[i]))
                  colmean <- c("None", "None")
                  colint <- c("None", "None")
            }else if(is.na(mean(dat[[i]], na.rm = TRUE))){
                  outlier_col <- c("No Data Present", "")
                  #namerow <- c("Name of Data", colnames(dat[i]))
                  colmean <- c("None", "None")
                  colint <- c("None", "None")
            }else {
                  out <- abs((dat[i])-mean(dat[[i]], na.rm = TRUE)) >
                  z*sd(dat[[i]], na.rm = TRUE)
                  
                  outlier_col <- data.frame(dat$event[which(out == TRUE)],
                  dat[[i]][which(out == TRUE)],stringsAsFactors=FALSE)
                  #namerow <- c("Name of Test", colnames(dat[i]))
                  colmean <- c("Mean of Column", mean(dat[[i]], na.rm = TRUE))
                  colint <- c("Confidence Interval", 
                        paste(c(mean(dat[[i]], na.rm = TRUE) + z*sd(dat[[i]], na.rm = TRUE)),
                        c(mean(dat[[i]], na.rm = TRUE) - z*sd(dat[[i]], na.rm = TRUE)), sep = ", "))
            }
            
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
            
            outliers<-do.call(cbind,dat_list)
      }
      
      outliers <- outliers[-c(1,2)]
      if(length(test) < 2){print(outliers)}
      write.csv(outliers, file_name)
      message(paste("Outliers printed to", file_name))
}