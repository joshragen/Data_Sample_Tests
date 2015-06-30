list_outliers <- function(file, test, z = 3){
      
      dat <- read.csv(file)
      
      out <- abs((dat[[test]])-mean(dat[[test]], na.rm = TRUE)) >
            z*sd(dat[[test]],na.rm = TRUE)
      #this finds the outliers outside of z standard deviations
      #and puts them into a logical vector
      
      outliers <- data.frame(row = which(out == TRUE),
                             value = dat[[test]][which(out == TRUE)])
      #this goes through that logical vector and puts the row number
      #and values of the outliers into a dataframe
      
      print(mean(dat[[test]], na.rm = TRUE))
      print(outliers)
      #these print the mean, and then the data frame with
      #the outliers and rows in them
}