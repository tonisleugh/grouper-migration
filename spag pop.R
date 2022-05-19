#Toni Sleugh
#04.21.22

# Create graphic showing when fish arrive & leave spawning aggregation

### load libraries ###
library(ggplot)
library(viridis)
library(gridExtra)

### create plots for each spawning month ###

colors <- viridis(8)

# FEBRUARY 15

spagPop.feb <- data.frame(date = seq(from = as.Date("2015-02-03"), 
                                     to = as.Date("2015-02-22"), by = "day"),
                          pop = c(0, 3, 5, 7, 8, 9, 10, 10, 10, 10, 10, 10, 7, 2,
                                  2, 2, 2, 2, 2, 1)) 
#create df for number of fish present on each day

spagPop.feb$pop.pro <- spagPop.feb$pop/max(spagPop.feb$pop) #create proportions
spagPop.feb$distfromfull <- seq(from = 0, to = length(spagPop.feb$date) -1, by = 1)
  
spfeb <- ggplot(spagPop.feb, aes(x = distfromfull, y = pop.pro)) +
  geom_line() +
  theme_classic() +
  theme(text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill=NA, size= 2)) +
  annotate("point", x = 0, y = -0.05, shape = 24, color = colors[3], 
           fill = colors[3], size = 3) +
  annotate("rect", xmin = 1, xmax = 4, ymin = -Inf, ymax = Inf, alpha = 0.5) +
  labs(x = NULL, y = NULL, title = "February 2015")

# MARCH 15

spagPop.mar <- data.frame(date = seq(from = as.Date("2015-03-01"), 
                                     to = as.Date("2015-03-17"), by = "day"),
                          pop = c(1, 2, 2, 4, 6, 7, 8, 9, 9, 9, 9, 9, 9, 9, 9, 8, 2)) 
#create df for number of fish present on each day

spagPop.mar$pop.pro <- spagPop.mar$pop/max(spagPop.mar$pop) #create proportions
spagPop.mar$distfromfull <- seq(from = -4, length.out = length(spagPop.mar$pop), by = 1)
#add column for distance from full moon

spmar <- ggplot(spagPop.mar, aes(x = distfromfull, y = pop.pro)) +
  geom_line() +
  theme_classic() +
  theme(text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill=NA, size= 2)) +
  annotate("point", x = 0, y = -0.05, shape = 24, color = colors[3], 
           fill = colors[3], size = 3) +
  labs(x = NULL, y = NULL, title = "March 2015")

# APRIL 15

spagPop.apr <- data.frame(date = seq(from = as.Date("2015-04-01"), 
                                     to = as.Date("2015-04-16"), by = "day"),
                          pop = c(2, 3, 4, 5, 5, 7, 8, 8, 8, 8, 8, 8, 8, 8, 6, 2)) 
#create df for number of fish present on each day

spagPop.apr$pop.pro <- spagPop.apr$pop/max(spagPop.apr$pop) #create proportions
spagPop.apr$distfromfull <- seq(from = -3, length.out = length(spagPop.apr$pop), by = 1)
#add column for distance from full moon

spapr <- ggplot(spagPop.apr, aes(x = distfromfull, y = pop.pro)) +
  geom_line() +
  theme_classic() +
  theme(text = element_text(size=10), 
        panel.border = element_rect(colour = "black", fill=NA, size= 2)) +
  annotate("point", x = 0, y = -0.05, shape = 24, color = colors[3], 
           fill = colors[3], size = 3) +
  labs(x = NULL, y = NULL, title = "April 2015")

### admire plots ###

spfeb

spmar

spapr

### stack plots ###

grid.arrange(spfeb, spmar, spapr, nrow = 1, left = "Proportion of Fish Present",
             bottom = "Days since Full Moon")
