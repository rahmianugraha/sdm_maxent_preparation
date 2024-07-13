#################################################################################################################################################
## Multicollinearity Test (to test linearity between variables)

# Install the required packages
install.packages("psych")
install.packages("fmsb")

# Set working directory and load the dataset
setwd("/YOUR/DIRECTORY")
data <- read.csv("environmental variables.csv") # contains the value of each environmental variable that has been extracted from the sample test random points

# View correlation between variables
library(psych)

# Code for correlation along with t test
correlation <- corr.test(data1)
correlation$r
correlation$t
correlation$p
correlation$se
write.csv(correlation$r, "correlation_r.csv")

# Create matrix plots, pearson correlation, and data distribution.
pairs.panels(data, cex.cor = 1, stars=T)
# "cex.cor=" is used to set the font size of the correlation value. Adjust the cex.cor value if the number is too large/small.
# If the graph doesn't appear and there is a warning "Error in plot.new() : figure margins too large", expand the plot window so that the graph can be larger.
# pay attention to the resulting matrix. The initial indication of multicollinearity is that there is a strong correlation between variables.

# Calculate the VIF value to detect multicollinearity
VIFstepwise<-function(in_frame,thresh=10,trace=T,...){
  library(fmsb)
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    in_dat<-in_frame
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      vif_vals<-NULL
      var_names <- names(in_dat)
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      vif_max<-as.numeric(vif_vals[max_row,2])
      if(vif_max<thresh) break
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
    }
    return(names(in_dat))
  }
}

# Enable the VIF stepwise calculation function
VIF_result <- VIFstepwise(data) 
# note that this function automatically eliminates variables that have a VIF value of more than 10 and automatically repeats the VIF calculation from the new variable composition until there are no more variables that have a VIF value of more than 10.
# variables which are selected and do not have multicollinearity were then used as predictors for distribution modeling using MaxEnt

# Export the results
write.csv(VIFstepwise(data), "VIF_result.csv")
