
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
            datd <- dat_rno[[p]]$Date
            #ord_date <- unique(datd[order(datd)])
            #dat_rno[[p]] <- dat_rno[[p]][order(datd)]
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
      #this code checks how many unique dates there are
      #datd <- lapply(dat$Date, function(x) as.Date(x, "%m/%d/%Y"))
      #datdl <- as.data.frame(datd)
      #for(i in 1:nrow(dat)){
      #      datf$Date[i] <- as.Date(dat$Date[i], "%m/%d/%Y")
      #}
}

dense<-function(g){
      if(class(g) == "character"){
            next
      }
      jpeg(paste0("density_abs_",i, names(int.key)[i], ".jpg"), 1000,600)
      d<-density(g, na.rm = T)
      plot(d)
      polygon(d, col="red")
      dev.off()
      #code to make Kernal density plot data instead of histogram
}
int.key <- split(dat_key$percent_lymphocytes, dat_key$percent_lymphocytes$Test.Name)
for(i in 1:length(int.key)){
      dense(as.numeric(int.key[[i]]$Result))
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

#run 37 times
#mean_dense <- function(t){
for(t in 1:length(dat_key)){
      if(nrow(dat_key[[t]]) <= 1000){
            print(t)
            next
      }
      datk <- dat_key[[t]]
      datk_rno <- split(datk, datk$RNO)
      for(h in 1:length(datk_rno)){
            if(nrow(datk_rno[[h]]) < 10){
                  datk_rno[[h]] <- NA
            }
      }
      datk_rno <- datk_rno[-which(is.na(datk_rno))]
      dalek <- rbindlist(datk_rno)
      dalek$Result <- as.numeric(dalek$Result)
      if(all(is.na(dalek$Result))){
            print(t)
            next
      }
      dalek.aov <- aov(Result ~ RNO, data = dalek)
      dmeans <- as.numeric(model.tables(dalek.aov,"means")[[1]][[2]])
      jpeg(paste0("density_", t, "_qqplot_", names(dat_key)[t], ".jpg"), 1000,600)
      qqnorm(dmeans)
      qqline(dmeans)
      dev.off()
      jpeg(paste0("density_",t, "_", names(dat_key)[t], ".jpg"), 1000,600)
      d<-density(dmeans, na.rm = T)
      plot(d)
      polygon(d, col="red")
      dev.off()
      print(t)
}
#prints density and qqplots of the means of each patient for each test

for(t in 1:length(dat_key)){
      if(nrow(dat_key[[t]]) <= 30){
            print("skip")
            next
      }
      datk <- dat_key[[t]]
      datk$Result <- as.numeric(datk$Result)
      datk <- datk[-which(is.na(datk$Result)),]
      if(length(datk$Result) <= 30){
            print("row.skip")
            next
      }
      out <- abs((datk[8])-mean(datk$Result, na.rm = TRUE)) >
            (5)*sd(datk$Result, na.rm = TRUE)
      if(T %in% out){
            datk<-datk[(-which(out == T)),]
            rownames(out)<-NULL
      }else{
            datk <- datk
      }
      if(all(is.na(datk$Result))){
            print("na.skip")
            next
      }
      
      jpeg(paste0("fdensity_", t, "_qqplot_", names(dat_key)[t], ".jpg"), 1000,600)
      qqnorm(datk$Result)
      qqline(datk$Result)
      dev.off()
      jpeg(paste0("fdensity_",t, "_", names(dat_key)[t], ".jpg"), 1000,600)
      d<-density(datk$Result, na.rm = T)
      plot(d)
      polygon(d, col="red")
      dev.off()
      print(t)
}
#prints overall tests for normality for all data per individual tests

daleks <- split(datk, datk$Test.Name)
for(p in 1:4){
      jpeg(paste0("p_", p, "_qqplot_", names(daleks)[p], ".jpg"), 1000,600)
      qqnorm(daleks[[p]]$Result)
      qqline(daleks[[p]]$Result)
      dev.off()
      jpeg(paste0("p_", p, "_density_", names(daleks)[p], ".jpg"), 1000,600)
      d<-density(daleks[[p]]$Result, na.rm = T)
      plot(d)
      polygon(d, col="red")
      dev.off()
}
#Prints by different names of original test

dat_key <- (split(dat, dat$Internal.Key)) 
hema <- dat_key$hematocrit
hema_rno <- (split(hema, hema$RNO))
hemaf <- hema_rno
for(h in 1:length(hemaf)){
      if(nrow(hemaf[[h]]) < 10){
            hemaf[[h]] <- NA
      }
}

library(data.table)
hemaf <- hemaf[-which(is.na(hemaf))]
#sapply(hemaf, function(x) dim(x))[1,]
hemad <- rbindlist(hemaf)
hemad<-hemad[-which(nchar(hemad$Result) >= 3),]
hemad$Result <- as.numeric(hemad$Result)
hemad.aov <- aov(Result ~ RNO, data = hemad)

summary(hemad.aov)
print(model.tables(hemad.aov,"means"),digits=5)
hmeans <- as.numeric(model.tables(hemad.aov,"means")[[1]][[2]])
#taking the density of this showed it was normal enough
#same with platelet count

library(lubridate)
dathy <- split(hema, year(hema$Date))
for(i in 1:length(dathy)){
      dathy[[i]][[8]] <- as.numeric(dathy[[i]][[8]])
      if(any(is.na(dathy[[i]][[8]]))){
            dathy[[i]] <- dathy[[i]][-which(is.na(dathy[[i]][[8]])),]
      }else{next}
}
for(i in 1:length(dathy)){
      dense(dathy[[i]][[8]])
}
#Note, I have modified how this function prints in the console when I ran it.

#sapply(daty, function(x) dim(x))[1,]

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

lymph <- dat_key$absolute_lymphocytes
lymphs <- split(lymph, lymph$Test.Name)
sam.lym <- data.table()
for(i in 1:4){
      sam.lm <- rbind(sam.lyp, lymphs[[i]][sample(nrow(lymphs[[i]]), 5),])
}