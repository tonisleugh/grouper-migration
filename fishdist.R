#' Toni Sleugh
#' 5.9.22
 
#' Find migration distances for commuter fish
 
### load libraries###
library(geosphere)

#Fish 1 - March 15

a <- distHaversine(c(-80.12083, 19.64998), c(-80.06371, 19.65643)) #dist from 1 to 14
b <- distHaversine(c(-80.021983, 19.67953), c(-80.06371, 19.65643)) #dist from 14 to 15

dist1 <- a + b/2

#Fish 8 - March & April 15

a <- distHaversine(c(-80.12083, 19.64998), c(-80.09628, 19.67755)) #dist from 1 to 4
b <- distHaversine(c(-80.09628, 19.67755), c(-80.07072, 19.68983)) #dist from 4 to 5

dist8 <- a + b/2

#Fish 10 - Feb 15

a <- distHaversine(c(-80.12083, 19.64998), c(-80.09628, 19.67755)) #dist from 1 to 4
b <- distHaversine(c(-80.09628, 19.67755), c(-80.07072, 19.68983)) #dist from 4 to 5
c <- distHaversine(c(-80.07072, 19.68983), c(-80.03178, 19.70546)) #dist from 5 to 7

dist10.1 <- a + b + c/2

#Fish 10 - March & April 15

a <- distHaversine(c(-80.12083, 19.64998), c(-80.09628, 19.67755)) #dist from 1 to 4
b <- distHaversine(c(-80.09628, 19.67755), c(-80.07072, 19.68983)) #dist from 4 to 5
c <- distHaversine(c(-80.07072, 19.68983), c(-80.03178, 19.70546)) #dist from 5 to 7
d <- distHaversine(c(-80.03178, 19.70546), c(-80.00019, 19.71156)) #dist from 7 to 8

dist10.2 <- a + b +c + d/2

#Fish 10 - Jan 16

a <- distHaversine(c(-80.03178, 19.70546), c(-80.00019, 19.71156)) #dist from 7 to 8
b <- distHaversine(c(-80.00019, 19.71156), c(-79.9679, 19.7211)) #sit from 8 to 9

dist10.3 <- a/2 + b/2

### find total distances for fish with multiple migrations ###

totDist.8 <- dist8*2

totDist.10 <- (dist10.1*2) + (dist10.2*3) + dist10.3

### total overall migration ###

totMig <- dist1 + totDist.8 + totDist.10

### average migration ###

avgMig <- totMig/3

