#'Toni Sleugh
#'4.18.22

#' Create maps of study area 

#' Code adapted from Dr. Kayla Blincow's github repository: 
#' https://github.com/kmblincow/Spatial-Ecology-of-Nassau-Grouper


### load libraries ###

library(tidyverse)
library(gridExtra)
library(ggmap)
library(maps)
library(mapdata)
library(sf)
library(ggsn)
library(padr)
library(viridis)
library(geometry)
library(sp)
library(geosphere)

### format data ###

#import tigerGrouper_detections from .txt file
tgDetections <- as.data.frame(tigerGrouper_detections) 
tgDetections$Date <- as.POSIXct(tgDetections$Date, format = "%m/%d/%y") #format dates

#create df with location info for each hydrophone
hp <- !duplicated(tgDetections$Name) #create logi vector for each unique hydrophone

hpLocation <- as.data.frame(cbind(tgDetections$Name[hp], 
                                  tgDetections$Latitude[hp], 
                                  tgDetections$Longitude[hp])) 
                                  #create matrix of lat/long for each hydrophone

hpLocation[,2]<- as.numeric(hpLocation[,2])
hpLocation[,3]<- as.numeric(hpLocation[,3]) #make lat/long numeric
names(hpLocation) <- c("phone", "lat", "long") #rename columns

hpLocation[2, 2] <- as.numeric(c(19.68983))
hpLocation[2, 3] <- as.numeric(c(-80.07072))
hpLocation[3, 2] <- as.numeric(c(19.65315))
hpLocation[3, 3] <- as.numeric(c(-80.12328)) #add in lat/long for Bld Bay & NN

hpLocation[nrow(hpLocation) + 4, ] <- NA #create empty rows for present hps with no detections

active <- !is.na(hpLocation$phone)

hpLocation$phone <- c(hpLocation$phone[active], 
                      "LC REC 9", "LC REC 10", "LC REC 3", "LC REC 15")
hpLocation$lat <- c(hpLocation$lat[active], 
                    19.7211, 19.69947, 19.66408, 19.67953)
hpLocation$long <- c(hpLocation$long[active], 
                     -79.9679, -79.94742, -80.11253, -80.021983) 
                    #add hps that were in the water but had no detections

hpLocation$det <- c(rep("Deployed", 7), 
                    rep("Deployed, no detections", 2), 
                    rep("Partially deployed, no detections", 2))
                    #add column for hp status

hpLocation$det[hpLocation$phone == "LC REC 8"] <- c("Partially deployed") 
hpLocation$det[hpLocation$phone == "LC REC 9"] <- c("Partially deployed, no detections") 

#fix REC 8 hp status (only deployed in january but had detections, unlike other 
#partially deployed hps)

### map of caribbean ###

world <- map_data("mapdata::worldHires") #load world map

carib <- world %>% 
  filter(long > -92 & long < -65 & lat > 16 & lat < 28.5) #set limits for caribbean map

xlabs <- c(90, 85, 80, 75, 70, 65)
ylabs <- c(15, 20, 25, 30) #create axis labels

countryLabels <- data.frame(label = c("Cuba", "Florida", "Mexico"),
                            xpos = c(-80, -81, -89),
                            ypos = c(22.5, 27, 20)) #create country labels

caribMap <- ggplot() + 
  geom_polygon(data = carib, aes(x=long, y = lat, group = group),
               fill = "gray60", color = "black") +
  labs(x = "Longitude", y = "Latitude", title = "Figure 1") +
  scale_x_continuous(limits = c(-92, -68), labels = paste0(xlabs,'°W'), 
                     breaks = c(-90, -85, -80, -75, -70, -65)) +
  scale_y_continuous(labels = paste0(ylabs,'°N'), breaks = c(15, 20, 25, 30)) +
  theme_classic() +
  theme(text = element_text(size=20), 
        panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  coord_fixed(1.3, expand = FALSE) +
  scalebar(x.min = -75, x.max = -70, y.min = 16.25, y.max = 17, dist = 500, 
           dist_unit = "km", height = 0.3, st.bottom = F, st.dist = 0.5, st.size = 3,
           transform = TRUE, model = "WGS84") +
  annotate("rect", xmin = -82, xmax = -79, ymin = 19, ymax = 20, color = "red",
           fill = NA)# +
#geom_label(data = countryLabels, aes(x = xpos, y = ypos, label = label))

### map of the cayman islands ###

caymans <- st_read("/Users/tonisleugh/Desktop/ /Grad/ngafatg/tiger grouper/cym_adm_2020_shp/cym_admbnda_adm0_2020.shp")
#load shapefile for caymans, from https://data.humdata.org/dataset/cod-ab-cym

cayMap <- ggplot() + 
  geom_sf(data = caymans, fill = "gray60", color = "black") +
  xlab(expression("Longitude")) +
  ylab(expression("Latitude")) +
  xlim(-81.75, -79.25) +
  scale_y_continuous(limits = c(19, 20), breaks = c(19, 19.5, 20)) +
  theme_classic() +
  theme(text = element_text(size=18, color = "black"), 
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        plot.background = element_rect(colour = "red", fill = NA, size = 2),
        axis.title = element_blank()) +
  annotate("rect",xmin = -80.13, xmax = -79.94, ymin = 19.62, ymax = 19.78,
           color = "red", fill = NA) +
  scalebar(x.min = -80, x.max = -79.5, y.min = 19.1, y.max = 19.2, dist = 50,
           dist_unit = "km", height = 0.3, st.bottom = F, st.dist = 0.5, st.size = 3,
           transform = TRUE, model = "WGS84") +
  coord_sf()

### map of little cayman with hydrophones ###

colors <- viridis(8) #create color palette

hpLocation$label <- c(NA, 5, 2, 11, 4, 6, 7, 8, 9, 3, 10) #create labels for hps

lilcayMap <- ggplot() + 
  geom_sf(data = caymans, fill = "gray60", color = "black") +
  geom_point(data = hpLocation, aes(x = long, y = lat), size = 2, color = colors[3]) + 
  xlim(-80.13, -79.94) +
  scale_y_continuous(limits = c(19.62, 19.78), breaks = c(19.65, 19.7, 19.75)) +
  labs(x = NULL, y = NULL) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(text = element_text(size=12, color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=2), 
        legend.text = element_text(size = 10), 
       legend.title = element_text(size = 12)) +
  scalebar(x.min = -80, x.max = -79.95, y.min = 19.63, y.max = 19.64, dist = 5, 
           dist_unit = "km", height = 0.3, st.bottom = F, st.dist = 0.5, st.size = 3,
           transform = TRUE, model = "WGS84") +
  annotate("point", x = -80.122850, y = 19.652769, shape = 24, size = 5) +
  annotate("point", x = -80.07072, y = 19.68983, shape = 5, size = 5) +
  geom_label(data = hpLocation, aes(label = label,
                                      x = (long),
                                     y = (lat + 0.0045)), size = 3.5) +
  geom_label(data = hpLocation, aes(label = "1",
                                    x = -80.12083,
                                    y = 19.64998 - 0.0045), size = 3.5) +
  coord_sf(expand = FALSE)


### admire maps ###

caribMap

cayMap

lilcayMap

### create maps with migration arrows ###

colors <- c(colors, "#FFFFFF") #add white to list of colors

#fish 1
migStartFin1 <- data.frame(x1 = c(-80.12083), #REC 1 
                          x2 = c(-80.06371), #REC 14
                          y1 = c(19.64998),
                          y2 = c(19.65643)) #create data frame with lat/long for 
                          #hydrophones of interest

f1Migration <- lilcayMap + 
  labs(title = "Fish 1") +
  geom_curve(data = migStartFin1, aes(x = x1, y = y1, xend = x2 + 0.025, yend = y2+ 0.005),
             arrow = arrow(length = unit(0.03, "npc")), curvature = 0.4) #march
#occurs at end of spawning season

#fish 8
migStartFin8 <- data.frame(x1 = c(-80.12083), #REC 1
                           x2 = c(-80.09628), #REC 4
                           y1 = c(19.64998),
                           y2 = c(19.67755))

f8Migration <- lilcayMap + 
  geom_curve(data = migStartFin8, aes(x = x1, y = y1, xend = x2 + 0.015, yend = y2 + 0.008),
             arrow = arrow(length = unit(0.03, "npc")), curvature = -0.45) + #march
  geom_curve(data = migStartFin8, aes(x = x2 + 0.01, y = y2 + 0.0125, 
                                      xend = x1 - 0.0005, yend = y1 + 0.005),#april
             arrow = arrow(length = unit(0.03, "npc"), ends = "both"),
             curvature = 0.5, linetype = 4) +
  labs(title = "Fish 8") 
  
#fish 10
migStartFin10 <- data.frame(x1 = c(-80.12083), #REC 1
                           x2 = c(-80.07072), #Bld Bay
                           x3 = c(-80.03178), #REC 7
                           x4 = c(-80.09628), #REC 4
                           x5 = c(-80.00019), #REC 8
                           y1 = c(19.64998),
                           y2 = c(19.68983),
                           y3 = c(19.70546),
                           y4 = c(19.67755),
                           y5 = c(19.71156))

f10Migration <- lilcayMap + 
  geom_curve(data = migStartFin10, aes(x = x1, y = y1, xend = x2 + 0.015, yend = y2 + 0.01),
             arrow = arrow(length = unit(0.03, "npc")), curvature = -0.3,
             linetype = 3) + #feb
  geom_curve(data = migStartFin10, aes(x = x2 + 0.015, y = y2 + 0.015, 
                                       xend = x1 - 0.0005, yend = y1 + 0.0075),
           curvature = 0.35) + #march
  geom_curve(data = migStartFin10, aes(x = x1 - 0.0005, y = y1 + 0.0075, 
                                       xend = x3 + 0.01, yend = y3 + 0.0045),
              arrow = arrow(length = unit(0.03, "npc"), ends = "both"), 
             curvature = -0.425) + #march
  geom_curve(data = migStartFin10, aes(x = x1 - 0.0005, y = y1 + 0.0175, 
                                       xend = x3 + 0.01, yend = y3 + 0.012),
            arrow = arrow(length = unit(0.03, "npc"), ends = "both"), 
            curvature = -0.425, linetype = 4) + #apr
  geom_curve(data = migStartFin10, aes(x = x3 + 0.015, y = y3 + 0.0025, 
                                       xend = x5 + 0.015, yend = y5 + 0.005),
             arrow = arrow(length = unit(0.03, "npc")), 
             curvature = -0.425, linetype = 5) +  #jan
  labs(title = "Fish 10")

### stack mig maps ###

grid.arrange(f1Migration, f8Migration, f10Migration, nrow = 1, left = "Latitude",
             bottom = "Longitude")
