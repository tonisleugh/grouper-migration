#' Toni Sleugh
#' 5.4.22

#' Create abacus plot of hydrophone status throughout study period

### load libraries ###
library(ggplot2)
library(viridis)

### create df with hydrophone labels & start/end dates ###

hpStatus <- data.frame(rec = c("1", "5/BB", "16/NN", "14", "4", "7", "8", "9", "10", "3",
                               "15"),
                       label = c(1, 5, 2, 11, 4, 6, 7, 8, 9, 3, 10),
                       dep.date = as.POSIXct(c("2015-02-04", "2015-02-04", "2015-02-17",
                                          "2015-03-07", "2015-03-07", "2015-03-07",
                                          "2016-01-22", "2015-02-04", "2015-02-04", 
                                          "2016-01-22", "2015-02-04")),
                       end.date = as.POSIXct(c("2016-02-14", "2016-02-14", "2016-02-14", 
                                          "2016-02-14", "2016-02-14", "2016-02-14", 
                                          "2016-02-14", "2016-02-14", "2016-02-14", 
                                          "2016-02-14", "2016-02-14")))

### plot ###

colors <- viridis(8)

ggplot(hpStatus) +
  geom_segment(aes(x = as.Date(dep.date), y = label, xend = as.Date(end.date), 
                                    yend = label)) +
  scale_y_reverse(breaks = 1:11) +
  theme_classic() +
  theme(text = element_text(size=20), 
        panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  labs(x = "Date", y = "Hydrophone") +
  scale_x_date(date_labels = "%m/%d/%y", 
               limits = as.Date(c("2015-01-31", "2016-02-14"))) +
  geom_segment(aes(x = as.Date("2015-02-04"), xend = as.Date("2016-02-14"),
                   y = 0, yend = 0), color = colors[3], linetype = 3) +
  annotate("rect", xmin = as.Date("2015-09-21"), xmax = as.Date("2015-09-23"),
           ymin = 0.5, ymax = 4.5, color = "white", fill = "white") +
  annotate("rect", xmin = as.Date("2015-09-21"), xmax = as.Date("2015-09-23"),
          ymin = 5.5, ymax = 9.5, color = "white", fill = "white") +
  annotate("rect", xmin = as.Date("2015-09-21"), xmax = as.Date("2015-09-23"),
           ymin = 10.5, ymax = 11.5, color = "white", fill = "white") +
  annotate("rect", xmin = as.Date("2015-09-21"), xmax = as.Date("2015-10-20"),
           ymin = 7.5, ymax = 8.5, color = "white", fill = "white")
  
