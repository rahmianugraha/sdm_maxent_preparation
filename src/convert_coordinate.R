#################################################################################################################################################
## Convert Coordinate (to convert lat-long coordinates to UTM, as MaxEnt required coordinates in UTM)

# Install the required packages
install.packages("rgdal")
library(rgdal)

# Set working directory and load the dataset
setwd("/YOUR/DIRECTORY")
data = read.csv("ailurops_ursinus_thinned_90m_thin2.csv")

# Setting existing coordinate as lat-long system
cord.dec = SpatialPoints(cbind(data$y, data$x), proj4string = CRS("+proj=longlat"))

# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=50M, Southern Hemisphere)
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32750"))
cord.UTM

# Plotting points
par(mfrow = c(1, 2))
plot(cord.dec, axes = TRUE, main = "Lat-Long Coordinates", cex.axis = 0.95)
plot(cord.UTM, axes = TRUE, main = "UTM Coordinates", col = "red", cex.axis = 0.95)

# Export the results
write.csv(cord.UTM, "ailurops_ursinus_thinned_90m_thin2_UTM.csv")
