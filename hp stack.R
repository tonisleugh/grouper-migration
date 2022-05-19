#Toni Sleugh
#4.5.22

#Create stacked graph of individual fish detections (by hydrophone)

### load libraries ### 
library(ggplot2)
library(tidyquant)
library(gridExtra)
library(lunar)
library(viridis)
library(ggpubr)
library(padr)

### format data ###

#import tigerGrouper_detections
tgDetections <- as.data.frame(tigerGrouper_detections) 
tgDetections$Date <- strptime(tgDetections$Date, format = "%m/%d/%y") #format dates

### create vectors to be used later ###

ind.fishies <- sort(unique(tgDetections$tag)) #find all 10 tag numbers

fullMoon <- as.Date(c("2015-02-03","2015-03-05", "2015-04-04", "2015-05-03",
                      "2016-01-23")) #create vector for full moons
spawnStart <- as.Date(c("2015-02-01", "2015-03-03", "2015-04-02", "2015-05-01", 
                        "2016-01-21"))
spawnEnd <- as.Date(c("2015-02-15", "2015-03-17", "2015-04-16", "2015-05-13", 
                      "2016-02-04")) #create vectors for spawning seasons 

colors <- viridis(16) #create color palette for receivers

firstdet <- c(NA, "2015-02-04", NA)
lastdet <- c(NA, "2016-02-14", NA) #create vectors for first and last detection dates

### create function for plots ###

graphHPDetections <- function(df, fishnum) {
  fish <- subset(df, df$tag == ind.fishies[fishnum]) #subset df for one ind fish
  
  fish <- as.data.frame(table(fish$Name, fish$Date))
  names(fish) <- c("phone", "date", "freq") #create table for frequency of visits to 
  #each phone
  
  fish$date <- as.POSIXct(fish$date) #format date column
  fish <- rbind(firstdet, fish, lastdet) #add row for first & last day of detections
  fish$date <- as.POSIXct(fish$date) #reformat date column as dates
  fish <- pad(fish) #add rows for dates with no observations
  
  empty <- is.na(fish$freq) #create logi vector for added dates
  fish$freq[empty] <- c(0) #fill freq column with zeros for added dates
  
  for(i in unique(fish$phone)){
    
    fish.add <- subset(fish, is.na(fish$phone)) #create copy df for added dates
    fish.add$phone <- rep(as.character(i), length(fish.add$phone)) #add other hp 
    #name for added dates
    
    fish <- rbind(fish, fish.add) #combine og empty columns with ones from copy df
  }
  fish <- subset(fish, !is.na(fish$phone)) #remove empty phone rows
  fish.dateord <- order(fish$date) #create vector to sort by date
  fish <- fish[fish.dateord,] #sort df by date
  
  fish.pres <- fish$freq != 0 #create vector of all rows with detections
  fish$freq[fish.pres] <- c(1) #replace frequency of detection with 1 for presence/absence
  
  ggplot(fish, aes(x = as.Date(date), y = freq, col = phone)) +
    theme_classic() +
    geom_point() +
    labs(title = paste("Fish", as.character(fishnum)), x = NULL, y = NULL, 
         col = "Location") +
    scale_x_date(date_labels = "%m/%d/%y", 
                 limits = as.Date(c("2015-01-31", "2016-02-14"))) +
    scale_color_manual(values = c("LC REC 1" = colors[1] ,
                                  "LC Bld Bay MPA" = colors[7] ,
                                  "LC NN REC" = colors[3],
                                  "LC REC 14" = colors[13] ,
                                  "LC REC 4" = colors[5],
                                  "LC REC 7" = colors[9],
                                  "LC REC 8" = colors[11]),
                       limits = c("LC REC 1", "LC NN REC", "LC REC 4", "LC Bld Bay MPA",
                                  "LC REC 7", "LC REC 8", "LC REC 14"),
                       labels = c("1", "2", "4", "5", "6", "7", "11")) +
    annotate("rect", xmin = spawnStart, xmax = spawnEnd,
             ymax = Inf, ymin = -Inf, alpha = 0.15 ) +
    theme(text = element_text(size=10), 
          panel.border = element_rect(colour = "black", fill=NA, size=2)) + 
    scale_y_discrete(labels = c("Absent", "Present"), breaks = c(0, 1)) +
    annotate("point", x = fullMoon, y = min(fish$freq), shape = 24, color = colors[6], 
             fill = colors[6], size = 2)
}

### plot each fish ###

fh1 <- graphHPDetections(tgDetections, 1)

fh2 <- graphHPDetections(tgDetections, 2)

fh3 <- graphHPDetections(tgDetections, 3)

fh4 <- graphHPDetections(tgDetections, 4)

fh5 <- graphHPDetections(tgDetections, 5)

fh6 <- graphHPDetections(tgDetections, 6)

fh7 <- graphHPDetections(tgDetections, 7)

fh8 <- graphHPDetections(tgDetections, 8)

fh9 <- graphHPDetections(tgDetections, 9)

fh10 <- graphHPDetections(tgDetections, 10)

### combine & save plots ###

maybe <- ggarrange(fh1, fh8, fh10, nrow = 2, common.legend = TRUE,
                   legend = "right")

maybe$`1`
maybe$`2`
maybe$`3`

ggsave("hpstack3.jpeg", plot = maybe$`3`)

