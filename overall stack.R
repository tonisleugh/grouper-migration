#Toni Sleugh
#4.5.22

# Create stacked graph of individual fish detections (hydrophone inclusive)

### load libraries ###
library(ggplot2)
library(tidyquant)
library(gridExtra)
library(lunar)
library(padr)
library(grid)
library(viridis)

### format data ###

#import tigerGrouper_detections from .txt file
tgDetections <- as.data.frame(tigerGrouper_detections) 
tgDetections$Date <- as.POSIXct(tgDetections$Date, format = "%m/%d/%y") #format dates
tgDetections <- pad(tgDetections) #create empty rows for days with no detections

tot.det <- as.data.frame(table(tgDetections$tag, tgDetections$Date)) #create table 
#of frequency of detections per day for each ind.
names(tot.det) <- c("tagnum", "date", "freq")

ind.fishies <- unique(tot.det$tagnum) #find all 10 tag numbers

### create function for plots ###

fullMoon <- as.Date(c("2015-02-03","2015-03-05", "2015-04-04", "2015-05-03",
                      "2016-01-23")) #create vector of full moons
spawnStart <- as.Date(c("2015-02-01", "2015-03-03", "2015-04-02", "2015-05-01", 
                        "2016-01-21"))
spawnEnd <- as.Date(c("2015-02-15", "2015-03-17", "2015-04-16", "2015-05-13", 
                      "2016-02-04")) #create vectors for spawning seasons

colors <- viridis(5) #create color palette

graphDetections <- function(df, fishnum) {
  fish <- subset(df, df$tagnum ==ind.fishies[fishnum])
  ggplot(fish, aes(x = as.Date(date), y = freq)) +
    geom_point() +
    geom_ma(ma_fun = SMA, n = 5, col = "red", linetype = 1) + #moving average
    labs(title = paste("Fish", as.character(fishnum)), x = NULL, y = "  ") +
    scale_x_date(date_labels = "%m/%d/%y", 
                 limits = as.Date(c("2015-01-31", "2016-02-14"))) + #format x axis
    theme_classic() +
    theme(text = element_text(size=10), 
          panel.border = element_rect(colour = "black", fill=NA, size= 2)) +
    annotate("point", x = fullMoon, y = -35, shape = 24, color = colors[3], 
             fill = colors[3], size = 3) + #add triangles for full moon
    annotate("rect", xmin = spawnStart, xmax = spawnEnd, ymin = -Inf, ymax = Inf, 
             alpha = 0.5) #add rectangles for spawning windows
}


### create graphs for each fish ###


f1 <- graphDetections(tot.det, 1)

f2 <- graphDetections(tot.det, 2)

f3 <- graphDetections(tot.det, 3)

f4 <- graphDetections(tot.det, 4)

f5 <- graphDetections(tot.det, 5)

f6<- graphDetections(tot.det, 6)

f7<- graphDetections(tot.det, 7)

f8 <- graphDetections(tot.det, 8)

f9<- graphDetections(tot.det, 9)

f10 <- graphDetections(tot.det, 10)


### stack graphs ###

ylab <- textGrob("Daily Detection Frequency", rot = 90, gp = gpar(fontsize = 20))
xlab <- textGrob("Date", gp = gpar(fontsize = 20)) #create axis labels for figure

grid.arrange(f1, f6, f2, f7, f3, f8, f4, f9, f5, f10,
             ncol = 2, left = ylab, bottom = xlab) #combine graphs to create figure



grid.arrange(f1, f8, f10,
             ncol = 1, left = ylab, bottom = xlab) #combine graphs to create figure

