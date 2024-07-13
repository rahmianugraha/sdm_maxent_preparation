#################################################################################################################################################
## Species Thinning (to minimize spatial autocorrelation of clustered points)

# Install the required packages
install.packages("spThin")
library(spThin)

# Set working directory and load the dataset
setwd("/YOUR/DIRECTORY")
ailurops_ursinus <- read.csv("ailurops_ursinus.csv") # contain the species coordinates (x, y)

# Begin species thinning with separate csv # this configuration is for the distance 90 m that want records to be separated by
ailurops_ursinus_thinned_90m <-
  thin( loc.data = ailurops_ursinus, 
        lat.col = "x", long.col = "y", 
        spec.col = "Species", 
        thin.par = 0.09, reps = 100, 
        locs.thinned.list.return = TRUE, 
        write.files = TRUE, 
        max.files = 5, 
        out.dir = "ailurops_ursinus_thinned_90m/", out.base = "ailurops_ursinus_thinned_90m", 
        write.log.file = TRUE,
        log.file = "ailurops_ursinus_thinned_90m_log_file.txt" )
