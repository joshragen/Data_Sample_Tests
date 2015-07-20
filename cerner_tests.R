
setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-read.csv("cerner_output.csv", stringsAsFactors = FALSE)
#library(data.table)
#This package could be useful

dat$Date <- as.Date(dat$Date, "%m/%d/%Y")
dat_rno <- (split(dat, dat$RNO))
tdate <- dat[, c(list(Internal.Key), list(RNO), list(Date))]

#dat_key <- (split(dat, dat$Internal.Key)) 
#dat_ex_date <- (split(dat, dat$Existing.Date.Particpant.Record))

#sapply(dat_rno, function(x) dim(x))[1,]
#^^prints length for each patient
check_dates <- function(pat){
      
      tdat <- dat_rno[[505]]
      
      difftime(tdat$Date[1],tdat$Date[5])
      #will find difference in days between visits
      
      date_sd <- sapply(dat_rno, function(x) sd(x$Date))
      mean(dat_sd, na.rm = T)
      #This gives the mean of the standard deviations is 534.4002
      
      #sd_vis <- sapply(dat_rno, function(x) sd(diff(unique(x$Date[order(x$Date)]))))
      diff_date <- function(r){
            ord_date <- unique(r$Date[order(r$Date)])
            #diffs <- sd(diff(ord_date))
            if(length(ord_date) < 8){
                  diffs <- NA
            }else{
                  diffs <- mean(diff(ord_date))
            }
            diffs
      }
      sd_vis <- sapply(dat_rno, diff_date)
      
      vis_lengths <- sapply(dat_rno, function(x) length(unique(r$Date[order(r$Date)])))
      #Code beneath uses data.table package
      #use duplicated to find identical entries
      
      dat_rnot<-dat_rno
      dup_date <- c()
      for(p in seq(dat_rno)){
            #ord_date <- unique(dat_rno[[p]]$Date[order(dat_rno[[p]]$Date)])
            #dat_rno[[p]] <- dat_rno[[p]][order(dat_rno[[p]]$Date)]
            #if(length(ord_date) <= 1){
            #      next
            #}
            #print(ord_date)
            tdate <- dat_rno[[p]]$Date
            #tdated<-tdate
            #for(l in 1:length(tdated)){
            #      #c <- dat_rno[[p]]$Date[l]
            #      #if(any(tdated[l] != ord_date)){next}
            #      o <- 1
            #      while(o <= (length(ord_date)-1)){
            #            if((tdated[l] == ord_date[o]) & (diff(ord_date)[[o]] <= 14)){
            #                  dat_rno[[p]]$Date[l] <- ord_date[o + 1]
            #                  ord_date <- unique(tdated)[order(unique(tdated))]
            #                  s = T
            #            }else{s=F}
            #            o <- o + 1
            #            if(s){break}
            #      }
            #      #print(l)
            #}
            #never run this again, it takes half an hour
            #tdate <- dat_rno[[p]][, list(Date)]
            #dup_date <- c()
            
            mat_date <- data.table(cbind(dat_rno[[p]]$Internal.Key, tdate))
            mat_datef <- split(mat_date, mat_date$tdate)
            mat_dup <- c()
            for(t in 1:length(mat_datef)){
                  if(any(duplicated(mat_datef[[t]]))){
                        dated <- length(which(duplicated(mat_datef[[t]])))
                  }else{
                        dated <- NA
                  }
                  mat_dup <- c(mat_dup, dated)
            }
            mat_dupf <- mat_dup[-which(is.na(mat_dup))]
            dup_date <- c(dup_date, mat_dupf)
            #print(p)
      }
      #sapply(dat_rno, function(x) length(unique(x$Date)))
      #this code checks how many unique date there are
      #datd <- lapply(dat$Date, function(x) as.Date(x, "%m/%d/%Y"))
      #datdl <- as.data.frame(datd)
      #for(i in 1:nrow(dat)){
      #      datf$Date[i] <- as.Date(dat$Date[i], "%m/%d/%Y")
      #}
}

dat_rnof <- rbindlist(dat_rno)
write.csv(dat_rnof, "cerner_tests_dates_2_weeks.csv")

dat_key <- split(dat_null, dat_null$Internal.Key)
null_dat <- dat_key[[60]]
dim(null_dat)
unique(null_dat$Test.Name)
null_test <- split(null_dat, null_dat$Test.Name)
null_names <- sapply(null_test, function(x) dim(x))[1,]
#sapply(dat_rno, function(x) dim(x))[1,]

dim_list <- c()
      for(d in 1:length(dat_rno)){
           dat_ever <- (split(dat_rno[[d]], dat_rno[[d]]$X.Is.Ever..Test))
           if(is.null(dat_ever[2][[1]])){
                 dat_ever[2][[1]] <- data.frame()
           }
           dim_list <- c(dim_list, dat_ever[2])
           #just makes a list of the second element of the list (TRUE)
           #dim_list[c(FALSE,TRUE)] prints every other element, could be useful
      }
      #sub(NULL, replacement = NA)
      e_dim <- lapply(dim_list, function(x) dim(x)[1])
      #unlist turns the list to a vector, useful for checking lengths
      is_ever_dim <- data.frame(rbind(names(dat_rno),unlist(e_dim, use.names = F)))
      #prints every other element(here the Ts)
      
#datf<-read.csv("cerner_output_full.csv", stringsAsFactors = FALSE)
datf <- data.table(datf)
datn <- datf[which(datf$Nearest.BRI.Date != "")]
datn$Date <- as.Date(datn$Date, "%Y-%m-%d")
datn$Nearest.BRI.Date <- as.Date(datn$Nearest.BRI.Date, "%Y-%m-%d")

      #dateh is a numeric vector of the differences between columns in datf divided by 30 (for months)
hist(abs(dateh), breaks = seq.int(0,82, by = 4), main = "Nearest Appointment to Last Visit",
     xlab = "Months", col = "turquoise", xlim = c(0,70))
abline(v = 60, col = "red")