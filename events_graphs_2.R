
setwd("C:/Users/jragen/Desktop/Sample_Tests")
library(data.table)
coll<-data.table(read.csv("collection_events.csv", stringsAsFactors = FALSE))
#coll<-coll[-c(1:93)]
#removes the rows with dates that don't fit into the format or are obviously wrong.
coll$date <- as.Date(coll$date, "%m/%d/%Y")

graph_coll <- function(g, color = T, mon = F, year = F, regis = F, zip = F, age = F, gender = F){
      if(color){
            cols <- c("black", "gray20", "gray40","gray60","darkblue", "blue","blueviolet", 
                      "lightblue", "green4", "green", "palegreen", "gold4", "gold", "red4",
                      "red", "orchid", "royalblue4", "powderblue", "orangered4", "orangered",
                      "olivedrab4", "violet", "yellow", "firebrick1", "turquoise", "chocolate4",
                      "chocolate", "cyan4", "cyan", "chartreuse4", "chartreuse", "blueviolet",
                      "brown4", "brown", "aquamarine4", "aquamarine", "azure4", "azure",
                      "bisque4", "beige", "dodgerblue4", "dodgerblue", "darkorchid4", "darkorchid",
                      "darkred", "darkslategray", "darkslategray1", "indianred4", "indianred",
                      "khaki", "lightseagreen", "mediumspringgreen","orange4", "orange", "yellow4",
                      "yellow", "steelblue4", "steelblue", "purple4", "purple", "olivedrab1")
      }
      
      if(mon | year){
            collf <- coll[-c(1:93)]
            cold <- split(collf, year(coll$date))
            coldm <- lapply(cold, function(x) split(x, month(x$date)))
            yearlen <- lengths(coldm)
            myear <- c()
            for(i in 1:length(coldm)){
                  myear <- c(myear, tail(gl(i, yearlen[i]), yearlen[i]))
            }
            
            coldf<-unlist(coldm, recursive=F)
            mdat <- sapply(coldf, function(x) nrow(x))
            mdaty <- sapply(cold, function(x) nrow(x))
            if(mon){
                  jpeg(paste0("collection_events_plot_months.jpg"), 1500,850)
                  barplot(mdat, main = "Events by Month", xlab = "Month",
                          ylab = "# of Events", col = cols[myear])
                  #legend("topleft", legend = names(coldm), fill = cols, cex = 0.5, y.intersp = 0.5)
                  legend("topleft", legend = paste0(names(coldm), ": n = ", mdaty),
                         fill = cols, cex = 1.25, y.intersp = 1.25)
                  dev.off()
                  #This section of code creates a bar graph by month
            }
            if(year){
                  #mdaty <- sapply(cold, function(x) nrow(x))
                  jpeg(paste0("collection_events_plot_year.jpg"), 1500,850)
                  barplot(mdaty, main = "Events by Year", xlab = "Year",
                          ylab = "# of Events", col = cols)
                  legend("topleft", legend = paste0(names(mdaty), ": n = ", mdaty),
                         fill = cols, cex = 1.25, y.intersp = 1.25)
                  dev.off()
                  #This creats the graph by overall year
            }
      }
      
      if(regis){
            colreg <- split(coll, coll$registry)
            reglen <- sort(sapply(colreg, function(x) nrow(x)))
            
            jpeg(paste0("collection_events_plot_registry.jpg"), 1500,850)
            par(mar = c(12,4,1,1))
            barplot(reglen, col = cols, main = "Events by Registry",
                    xlab = "Registry", ylab = "# of Events", las = 2)
            legend("topleft", legend = paste0(names(reglen), ": n = ", reglen),
                   fill = cols, cex = 1.5, y.intersp = 1.5)
            dev.off()
      }
      
      if(zip){
            reg <- read.csv("full_postal_codes.csv", stringsAsFactors = FALSE, colClasses = "character")
            #czip <- coll$zip
            crnozip <- split(coll$zip, coll$RNO.Hash)
            czip <- as.vector(sapply(crnozip, function(x) unique(x)))
            rzip <- reg$zip
            #cstate <- coll$state
            #rstate <- unique(reg$state)
            
            #fix_zip <- function(r){
            state <- c()
            for(r in 1:length(czip)){
                  zp <- czip[r]
                  if(nchar(zp) == 10){
                        zp <- substr(zp, 1,nchar(zp)-5)
                  }
                  if(nchar(zp) == 0){
                        fin <- "None"
                  }else if(!any(zp == rzip)){
                        #if(any(cstate[r] == rstate)){
                        #      fin <- cstate[r]
                        #}else{
                        #      fin <- "No Match"
                        #}
                        #zp <- NA
                        fin <- "No Match"
                  }else{
                        fin <- reg[which(zp == rzip),]$state[1]
                  }
                  state <- c(state, fin)
                  print(r)
            }
            #slow code
            #state <- sapply(1:nrow(coll), fix_zip, USE.NAMES = FALSE)
            statlen <- sort(table(state))
            jpeg(paste0("collection_events_plot_state.jpg"), 1500,850)
            barplot(statlen, ylim = c(0,500), main = "Events by Patients Home State",
                    xlab = "State", ylab = "# of Patients", col = cols, las = 2)
            legend("topleft", legend = paste0(names(statlen), ": n = ", statlen),
                   fill = cols, ncol = 2, cex = 1.25, y.intersp = 1.25)
            dev.off()
      }
      
      if(age){
            jpeg(paste0("collection_events_histo_age.jpg"), 1500,850)
            hist(coll$age, breaks = seq.int(0,100, by = 2), main = "Distribution of Age",
                 xlab = "Age", col = cols, xlim = c(0,100))
            axis(1, at = seq.int(0,100, by = 5))
            dev.off()
      }
      
      if(gender){
            #genreg <- split(coll, coll$gender)
            #genlen <- sort(sapply(genreg, function(x) nrow(x)))
            crnogen <- split(coll$gender, coll$RNO.Hash)
            cgen <- as.vector(sapply(crnogen, function(x) unique(x)[1]))
            genlent <- sort(table(cgen))
            names(genlent)[4] <- "empty"
            
            jpeg(paste0("collection_events_rno_plot_gender.jpg"), 1500,850)
            barplot(genlent, col = c("firebrick", "orangered", "gray10", "azure", "deepskyblue3", "hotpink"), 
                    main = "Events by Gender", xlab = " Gender", ylab = "# of Events")
            legend("topleft", legend = paste0(names(genlent), ": n = ", genlent),
                   fill = c("firebrick", "orangered", "gray10", "azure", "deepskyblue3", "hotpink"), 
                   cex = 1.5, y.intersp = 1.5)
            dev.off()
      }
}


sam <- data.table(read.csv("sample_events_f.csv", stringsAsFactors = FALSE))
sam$Draw.Date <- as.Date(sam$Draw.Date, "%m/%d/%Y")
library(RColorBrewer)

graph_sam <- function(s, color = T, mon = F, year = F, diff = T, regis = F, sample = F, location = F){
      if(color){
                  cols <- c("black", "gray20", "gray40","gray60","darkblue", "blue","blueviolet", 
                            "lightblue", "green4", "green", "palegreen", "gold4", "gold", "red4",
                            "red", "orchid", "royalblue4", "powderblue", "orangered4", "orangered",
                            "olivedrab4", "violet", "yellow", "firebrick1", "turquoise", "chocolate4",
                            "chocolate", "cyan4", "cyan", "chartreuse4", "chartreuse", "blueviolet",
                            "brown4", "brown", "aquamarine4", "aquamarine", "azure4", "azure",
                            "bisque4", "beige", "dodgerblue4", "dodgerblue", "darkorchid4", "darkorchid",
                            "darkred", "darkslategray", "darkslategray1", "indianred4", "indianred",
                            "khaki", "lightseagreen", "mediumspringgreen","orange4", "orange", "yellow4",
                            "yellow", "steelblue4", "steelblue", "purple4", "purple", "olivedrab1")
      }
      
      if(mon | year){
            samd <- split(sam, year(sam$Draw.Date))
            samd <- samd[-1]
            samdm <- lapply(samd, function(x) split(x, month(x$Draw.Date)))
            yearlen <- lengths(samdm)
            myear <- c()
            for(i in 1:length(samdm)){
                  myear <- c(myear, tail(gl(i, yearlen[i]), yearlen[i]))
            }
            
            samdf<-unlist(samdm, recursive=F)
            mdat <- sapply(samdf, function(x) nrow(x))
            mdaty <- sapply(samd, function(x) nrow(x))
            if(mon){
                  jpeg(paste0("sample_events_plot_months.jpg"), 1500,850)
                  barplot(mdat, main = "Samples by Month", xlab = "Month",
                          ylab = "# of Samples", col = cols[myear])
                  #legend("topleft", legend = names(samdm), fill = cols, cex = 0.5, y.intersp = 0.5)
                  legend("topleft", legend = paste0(names(samdm), ": n = ", mdaty),
                         fill = cols, cex = 1.25, y.intersp = 1.25)
                  dev.off()
                  #This section of code creates a bar graph by month
            }
            if(year){
                  #mdaty <- sapply(samd, function(x) nrow(x))
                  jpeg(paste0("sample_events_plot_year.jpg"), 1500,850)
                  barplot(mdaty, main = "Samples by Year", xlab = "Year",
                          ylab = "# of Samples", col = cols)
                  legend("topleft", legend = paste0(names(mdaty), ": n = ", mdaty),
                         fill = cols, cex = 1.25, y.intersp = 1.25)
                  dev.off()
                  #This creats the graph by overall year
            }
      }
      if(diff){
            samy <- sam[-which(year(sam$Draw.Date) == 1900)]
            today <- Sys.Date()
            fiveyear <- as.Date(c("2000-01-01", "2005-01-01", "2010-01-01"))
            yearlines <- as.numeric(difftime(today, fiveyear))
            axisyear <- seq(1995, 2015)
            
            sameh <- year(samy$Draw.Date)
            #sameh <- as.numeric(difftime(today, samy$Draw.Date))
            #sameh <- round(sameh/365)
            jpeg(paste0("sample_events_differences_histogram.jpg"), 1500,850)
            #hist(sameh, main = "Year of Sample Draw", xlab = "Year", col = terrain.colors(length(axisyear)))
            hist(-sameh, right = F, xaxt = 'n', main = "Year of Sample Draw", xlab = "Year", col = terrain.colors(length(axisyear)))
            axis(1, at = seq(-1995, -2015, by = -5), labels = c(1995, 2000, 2005, 2010, 2015))
            #axis(1, at = seq.int(0,7500, by = 250))
            #abline(v = yearlines, col = "red")
            #text(yearlines+100, 17000, c("2000", "2005", "2010"))
            dev.off()
            
            
            
            #jpeg(paste0("sample_events_differences_histogram.jpg"), 1500,850)
            #dens <- density(sameh, na.rm = T)
            #plot(dens$x, length(sameh) * dens$y, type = "l")
            #polygon(d, col=terrain.colors(21))
            
            #jpeg(paste0("sample_events_differences_line.jpg"), 1500,850)
            #plot(axisyear, table(sameh), type = "o", yaxt = 'n')
            #axis(2, at = seq(0, 20000, by = 5000), labels = seq(0, 20000, by = 5000))
            #dev.off()
            if(regis){
                  sameh.reg <- split(year(samy$Draw.Date), samy$Registry)
                  sameh.reg <- sameh.reg[order(lengths(sameh.reg), decreasing = T)]
                  loccol <- rainbow(length(sameh.reg))
                  #jpeg(paste0("sample_events_differences_reg_boxplot.jpg"), 1500,850)
                  ##b <- boxplot(sameh.reg, plot = 0)
                  #par(mar = c(6,4,4,1))
                  #boxplot(sameh.reg, col = cols[4:61], main = "Differences by Registry",
                  #        xlab = "Registry", ylab = "Days from Today")
                  #abline(h = yearlines, col = "red")
                  #text(length(sameh.reg)+0.75, yearlines+100, c("2000", "2005", "2010"))
                  #legend("topleft", legend = paste0(names(sameh.reg), ": n = ", lengths(sameh.reg)),
                  #       fill = cols[4:61])
                  ##axis(2, at = seq(0, 7300, by = 365), labels = axisyear)
                  #dev.off()
                  
                  jpeg(paste0("sample_events_differences_registry_lines.jpg"), 1500,850)
                  plot(axisyear, table(sameh), type = "o", yaxt = 'n', main = "Sample Draw by Registry",
                       xlab = "Registry", ylab = "Frequency", lwd = 5)
                  axis(2, at = seq(0, 20000, by = 5000), labels = seq(0, 20000, by = 5000))
                  for(i in 1:length(sameh.reg)){
                        lines(names(table(sameh.reg[i])), table(sameh.reg[i]), type = "o", col = loccol[i], lwd = 5)
                  }
                  legend("topleft", legend = c(paste0("Sample Type: n = ", length(sameh)), paste0(names(sameh.reg), ": n = ", lengths(sameh.reg))),
                         fill = c("black", loccol[1:i]), cex = 1.5)
                  dev.off()
            }
            #By Registry
            
            if(type){
                  sameh.sample <- split(year(samy$Draw.Date), samy$Sample.Type)
                  sameh.sample <- sameh.sample[order(lengths(sameh.sample), decreasing = T)]
                  loccol <- rainbow(length(sameh.sample))
                  #jpeg(paste0("sample_events_differences_sample_type_boxplot.jpg"), 1500,850)
                  ##b <- boxplot(sameh.sample, plot = 0)
                  #par(mar = c(6,4,4,1))
                  #boxplot(sameh.sample, col = cols[20:61], main = "Differences by Sample Type",
                  #        xlab = "Type", ylab = "Days from Today")
                  #abline(h = yearlines, col = "red")
                  #text(length(sameh.sample)+0.75, yearlines+100, c("2000", "2005", "2010"))
                  #legend("topleft", legend = paste0(names(sameh.sample), ": n = ", lengths(sameh.sample)),
                  #       fill = cols[20:61])
                  #dev.off()
                  
                  jpeg(paste0("sample_events_differences_sample_type_lines.jpg"), 1500,850)
                  plot(axisyear, table(sameh), type = "o", yaxt = 'n', main = "Sample Draw by Type",
                       xlab = "Sample Type", ylab = "Frequency", lwd = 5)
                  axis(2, at = seq(0, 20000, by = 5000), labels = seq(0, 20000, by = 5000))
                  for(i in 1:length(sameh.sample)){
                        lines(names(table(sameh.sample[i])), table(sameh.sample[i]), type = "o", col = loccol[i], lwd = 5)
                  }
                  legend("topleft", legend = c(paste0("Sample Type: n = ", length(sameh)), paste0(names(sameh.sample), ": n = ", lengths(sameh.sample))),
                         fill = c("black", loccol[1:i]), cex = 1.5)
                  dev.off()
            }
            
            #By Sample Type
            
            
            if(location){
                  sameh.loc <- split(year(samy$Draw.Date), samy$Location)
                  sameh.loc <- sameh.loc[order(lengths(sameh.loc), decreasing = T)]
                  loccol <- rainbow(length(sameh.loc))
                  #jpeg(paste0("sample_events_differences_location_boxplot.jpg"), 1500,850)
                  ##b <- boxplot(sameh.loc, plot = 0)
                  #par(mar = c(6,4,4,1))
                  #boxplot(sameh.loc, col = cols[40:61], main = "Differences by Sample Location",
                  #        xlab = "Location", ylab = "Year")
                  #abline(h = yearlines, col = "red")
                  #text(length(sameh.loc)+0.75, yearlines+100, c("2000", "2005", "2010"))
                  #legend("bottomleft", legend = paste0(names(sameh.loc), ": n = ", lengths(sameh.loc)),
                  #       fill = cols[40:61])
                  #dev.off()
                  
                  jpeg(paste0("sample_events_differences_location_lines.jpg"), 1500,850)
                  plot(axisyear, table(sameh), type = "o", yaxt = 'n', main = "Sample Draw by Location",
                       xlab = "Location", ylab = "Frequency", lwd = 5)
                  axis(2, at = seq(0, 20000, by = 5000), labels = seq(0, 20000, by = 5000))
                  for(i in 1:length(sameh.loc)){
                        lines(names(table(sameh.loc[i])), table(sameh.loc[i]), type = "o", col = loccol[i], lwd = 5)
                  }
                  legend("topleft", legend = c(paste0("Sample Type: n = ", length(sameh)), paste0(names(sameh.loc), ": n = ", lengths(sameh.loc))),
                         fill = c("black", loccol[1:i]), cex = 1.5)
                  dev.off()
            }
            #By Location
      }
      #make boxplots of the difference in dates to today
      
      if(regis){
            samreg <- split(sam, sam$Registry)
            reglen <- sort(sapply(samreg, function(x) nrow(x)))
            
            jpeg(paste0("sample_events_plot_registry.jpg"), 1500,850)
            par(mar = c(12,4,1,1))
            barplot(reglen, col = cols, main = "Samples by Registry",
                    xlab = "Registry", ylab = "# of Samples", las = 2)
            legend("topleft", legend = paste0(names(reglen), ": n = ", reglen),
                   fill = cols, cex = 1.5, y.intersp = 1.5)
            dev.off()
      }
      
      if(sample){
            samsam <- split(sam, sam$Sample.Type)
            samlen <- sort(sapply(samsam, function(x) nrow(x)))
            
            jpeg(paste0("sample_events_plot_sample_type.jpg"), 1500,850)
            par(mar = c(8,4,1,1))
            barplot(samlen, col = cols, main = "Samples by Type",
                    xlab = "Type", ylab = "# of Samples", las = 2)
            legend("topleft", legend = paste0(names(samlen), ": n = ", samlen),
                   fill = cols, cex = 1.5, y.intersp = 1.5)
            dev.off()
      }
      
      if(location){
            samloc <- split(sam, sam$Location)
            loclen <- sort(sapply(samloc, function(x) nrow(x)))
            
            jpeg(paste0("sample_events_plot_location.jpg"), 1500,850)
            par(mar = c(8,4,1,1))
            barplot(loclen, col = cols, main = "Samples by Location",
                    xlab = "Location", ylab = "# of Samples", las = 2)
            legend("topleft", legend = paste0(names(loclen), ": n = ", loclen),
                   fill = cols, cex = 1.5, y.intersp = 1.5)
            dev.off()
      }
}