#This file is just where I've been dumping various bits of code to go through these
#spreadsheets and either sort or analyze them. There's no particular organization
#or order in it, and if the file were to be ran, it would most likely crash.

#Some basic plotting functions for histrograms and density curves
histo<-function(g){
      ts<-g
      hts<-hist(ts,breaks = 10)
      xfit<-seq(min(ts, na.rm = T), max(ts, na.rm = T), length = 40)
      yfit<-dnorm(xfit,mean=mean(ts,na.rm = T),sd=sd(ts,na.rm = T))
      yfit <- yfit*diff(hts$mids[1:2])*length(ts) 
      lines(xfit, yfit, col="blue", lwd=2)
      #code to make histogram with line of normal curve
}

dense<-function(g){
      if(class(g) == "character"){
            next
      }
      jpeg(paste0("density_low_","_",names(g), "_rm.jpg"), 1000,600)
      d<-density(g, na.rm = T)
      plot(d)
      polygon(d, col="red")
      dev.off()
      #code to make Kernal density plot data instead of histogram
}

#The section of code beneath this prints density graphs for the high or low plots.

setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-read.csv("cbc_results_units.csv", stringsAsFactors = FALSE)
z = 3

test <- 34

datf <- dat[-which(is.na(dat[[test]])),]
bound <- 3

high_dat <- split(datf, (datf[[test]] < bound))[[1]]
low_dat <- split(datf, (datf[[test]] < bound))[[2]]

for(i in 1:53){
      if(class(high_dat[[i]]) == "character"){
            next
      }else if(all(is.na(high_dat[[i]]))){
            next
      }
      out <- abs((high_dat[i])-mean(high_dat[[i]], na.rm = TRUE)) >
            (6)*sd(high_dat[[i]], na.rm = TRUE)
      if(T %in% out){
            high_dat<-high_dat[(-which(out == T)),]
            rownames(out)<-NULL
      }
}

for(i in 1:ncol(high_dat)){
      g<-high_dat[[i]]
      if(class(g) == "character"){
            next
      }else if(all(is.na(g))){
            next
      }
      jpeg(paste0("density_low_",names(high_dat[i]), "_rmh", ".jpg"), 1000,600)
      d<-density(g, na.rm = T)
      plot(d)
      polygon(d, col="red")
}

#^^^^^^ End of this section

#abline(lm(...), col = "red")
#this adds a line of linear regression to the dat, which first needs to be found with the lm function
#lm can be lm(dat$testresults_hemoglobin ~ dat$testresults_hematocrit)

#I didn't finish the code beneath this.
graph_data <- function(file, test = 4:l, file_name = "", type, rm = F){
      dat <- read.csv(file, stringsAsFactors = FALSE)
      l <- ncol(dat)
      #dat<-read.csv("cbc_results.csv", stringsAsFactors = FALSE)
      
      if(rm){
      for(i in 4:28){

            out <- abs((dat[i])-mean(dat[[i]], na.rm = TRUE)) >
                  (z * 2)*sd(dat[[i]], na.rm = TRUE)
            if(T %in% out){
                  dat<-dat[(-which(out == T)),]
                  rownames(out)<-NULL
            }
      }
      }
      if(type == "density"){
            
      }
}

#this just made boxplots for the general data.
for(i in c(4:12,18:28)){
      datl<-(split(dat[[i]], dat$registry))
      boxplot(datl)
      jpeg(paste0("boxplot_", i,"_",names(dat[i]), "_rm.jpg"), 1200,800)
}

for(l in 4:28){
      print(cor(dat[[l]], dat$ageAtDraw, use = "pairwise.complete.obs"))
}
#this code checks for correlations with age
cor.test(dat$testresults_hematocrit, dat$testresults_hemoglobin, use = "pairwise.complete.obs")
with(dat, cor(testresults_white_blood_cell_count, testresults_absolute_neutrophils, use = "pairwise.complete.obs"))
#this code also checks correlations between entries, and replacing the cor with plot plots them

corrdat<-as.data.frame(cor(dat[4:28], use = "pairwise.complete.obs"))
#^^^^ This creates a table with the correlations of all entries in the table together

find_cor_rows<-function(n){
      for(i in n){
            #if(any(corrdat[,i] > 0.75, na.rm = T)){
            rown <- which(corrdat[[i]] > 0.75)
            rown <- rown[-which(rown == i)]
            if(length(rown) > 0){
                  print(paste0("[", rown, ",", i, "]"))
            }
      }
}
plot_cor <- function(rown,col){
      plot
}
#I don't remember what this code was for ^^^^^^^.

setwd("C:/Users/jragen/Desktop/Sample_Tests")
dat<-read.csv("cbc_results_units.csv", stringsAsFactors = FALSE)
z = 3
for(i in 1:53){
      if(class(dat[[i]]) == "character"){
            next
      }else if(all(is.na(dat[[i]]))){
            next
      }
            out <- abs((dat[i])-mean(dat[[i]], na.rm = TRUE)) >
                  (z * 3)*sd(dat[[i]], na.rm = TRUE)
            if(T %in% out){
                  dat<-dat[(-which(out == T)),]
                  rownames(out)<-NULL
                  }
}

test <- 23

datf <- dat[-which(is.na(dat[[test]])),]
bound <- 3

high_dat <- split(datf, (datf[[test]] < bound))[[1]]
low_dat <- split(datf, (datf[[test]] < bound))[[2]]
for(i in c(4:12,18:28)){
      jpeg(paste0("boxplot_", i,"_",names(dat[i]), "_rm.jpg"), 1000,600)
      datl<-(split(dat[[i]], dat$registry))
      boxplot(datl[c(1:6,9)])
}

#to not show outliers in boxplots, use range and outline = F to change it
#This whole section above this could be copy and pasted to get boxplots of data.

dage <- dat$ageAtDraw
datq<-split(dat, cut(dage, quantile(dage, na.rm = T),include.lowest = T))



datq<-split(dat$testresults_absolute_monocytes, cut(dat$testresults_absolute_monocytes,
      quantile(dat$testresults_absolute_monocytes, probs = seq(0,1,0.16666666), na.rm = T),include.lowest = T))
#prcomp(~ ., data = dat[c(4:12,18:28)], na.action = na.omit, scale = T)
#this does PCA on data (not really, i didn't really get it to work)


for (i in c(46:51, 55:63)){
      alt<-as.name(paste0(names(dat)[[i]], ".1"))
      colm <- which(names(dat) == alt)
      if(all(dat[[i]] == dat[[colm]], na.rm = T)){
            print("TRUE")
      }
}
#this was to check if the columns were identical to their counterparts with a .1 on them
#they were, so I removed them to make space

#code to take random samples which was later rbind'ed together to make a sample


zip <- zip[-which(is.na(zip))]
locs <- data.frame()
for(z in 1:length(zip)){
      if(any(zip[z] == zipcode$zip)){
            row <- which(zip[z] == zipcode$zip)
            coord <- c(zipcode$latitude[row], zipcode$longitude[row])
      }else{
            next
      }
      locs <- rbind(locs, coord)
}
names(locs) <- c("latitude", "longitude")
locs$latitude <- jitter(locs$latitude, amount = 0.3)
locs$longitude <- jitter(locs$longitude, amount = 0.3)

map <- get_googlemap('usa', markers = locss[1:50,], zoom = 3, scale = 1)
#only gets first 50 points, looks bad

map <- get_map('usa', zoom = 3, scale = 10)
p <- ggmap(map) + geom_point(data = locs, aes(longitude, latitude), col = 'red')

#this makes a map of the us with all of the general areas of the zip codes plotted onto it