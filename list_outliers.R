#'file' is name of csv file to use
#'z' is the number of sd's from the mean to test (default = 3)

list_outliers <- function(file, z = 3){
      
      dat <- read.csv(file)
      
      outliers <- data.frame(ROWS = c(0), POINTS = c(0))
      #sets the outliers data frame with column names before the loop
      
      for(test in 1:ncol(dat)) {
            outlier_col<-NULL
            if(is.nan(mean(dat[[test]], na.rm = TRUE) == TRUE)){
                  outlier_col <- c("No Data Present", "")
                  #if there is no data, or it's not numerical, then
                  #"No Data Present" is put into the data frame
                  
            } else if(is.na(mean(dat[[test]], na.rm = TRUE))){
                  outlier_col <- c("Non-Numerical Data", "")
                  
            } else {
                  out <- abs((dat[test])-mean(dat[[test]], na.rm = TRUE)) >
                  z*sd(dat[[test]], na.rm = TRUE)
                  #this finds the outliers outside of z standard deviations
                  #and puts them into a logical vector
                  
                  outlier_col <- data.frame(ROWS = which(out == TRUE),
                  POINTS = dat[[test]][which(out == TRUE)])
                  #this goes through that logical vector and puts the row number
                  #and values of the outliers into a dataframe
            }
            #the if/else statement checks if there is numerical data in
            #the column, then find outliers if there is
            
            
            namerow <- c("Name of Test", colnames(dat[test]))
            colmean<- c("Mean of Column", mean(dat[[test]], na.rm = TRUE))
            #This both prints the name of the outlier, and the mean of the columns
            #so that they are displayed above the list of outliers
            
            outliers <- rbind(outliers, namerow, colmean, outlier_col)
            #combines outliers data frame for every loop though this function
            #adding previous array first, then names of the tests they
            #are in, then the mean and and data
      }
      
      write.csv(outliers, "outliers.csv")
      #this function prints out the table when it's done
      #to a csv file
}
#NOTE:At the end of the function, warnings display for every column
#which didn't have numeric values in it, ignore them