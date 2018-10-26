###################################################
### Author: Daljit Singh (singhdj2@ksu.edu)     ###
### Oct 2018                                    ###
### Script description:                         ###
##  - estimates params of mix normal distri     ###
##                                              ###              
###################################################

## load packages with package manager pacman
if(!require(pacman)){
  install.packages(pacman)
}
p_load(pacman,mixtools, tidyverse)


## This first function takes single plot data as input and returns a mixture model summary and a density plot
## Use this function to fine tune the mu and lambda parameters for your data (needed later in the full run)
# mixFuncSingle <- function(dat,plot.nm){
#   plt <- paste0("17-LDH-STN-SAG-",plot.nm)
#   pt <- dat$diff.ht.cm[dat$plot_id==plt]
#   mod <- normalmixEM(pt, lambda = c(0.5,0.5), mu= c(0,65), mean.constr = c(0,NA),maxit = 2000,maxrestarts = 60)
#   summary(mod)
#   plot(mod,density=T, whichplots=2, main2=paste("Density Curves for plot ",plot.nm),xlim=c(-20,110))
# }


### Full mixture function with better error handling and added iterations to 
## ..account for random start values for some troubling plots (can be improved: parallelize,set.seed)
mixCalc <- function(x){
  # initialize an empty data frame to hold parameter values
  df <- setNames(data.frame(matrix(ncol = 9, nrow = 5)),
                 c('nIter','nRestarts','lambda1','lambda2','mu1','mu2','sig1','sig2','loglik'))
  #run several iterations of the mixture model (to avoid outliers due to multiple model convegance solutions)
  for(i in 1:5){
    z <- try(suppressWarnings(normalmixEM(x$diff.ht.cm, lambda = c(0.5,0.5), 
                                          mu= c(0,65), mean.constr = c(0,NA),
                                          maxit = 2000, verb = F, maxrestarts=60)), silent = T)
    if (class(z) == "try-error"){
      cat("Note: Error in model run. \n")
      df[i,] <- cbind(nIter=NA,nRestarts=NA,
                   lambda1=NA, lambda2=NA, mu1=NA,
                   mu2=NA, sig1=NA,sig2=NA,loglik=NA)
    } else {
      df[i,] <- cbind(nIter=length(w$all.loglik)-1,nRestarts=w$restarts,
                  lambda1=w$lambda[1], lambda2=w$lambda[2], mu1=w$mu[1],
                  mu2=w$mu[2], sig1=w$sigma[1],sig2=w$sigma[2],loglik=w$loglik)
    }
  }
  #df %>% data.frame() %>% filter(lambda1==max(lambda1))   #can use: summarise_at(vars(nIter:loglik),funs(mean,na.rm=T)) ; 
  return(df)    #output dataframe (gives better control to user for deciding summary per plot)
}


#### usage example on full data ####
# function requires two columns (plot_id, diff.ht.cm)
# note: this can take long time (e.g. 20-30 minutes for >1700 plots with 3-4k data points each)
# I'm working on parallelizing this funciton (efficient version coming soon!) 
dat.mix.norm <- dat.raw %>%
  group_by(plot_id) %>%        # group dataframe by plots
  do(mixCalc(.))               # collect function ouput as data frame

# take a look at our dataframe
head(dat.mix.norm)

## this is an example of getting single plot-level value per plot 
#  ...(summarizing multiple iterations of mixture estimates)
dat.mix.norm.summary <- dat.mix.norm %>% 
  group_by(plot_id) %>% 
  summarise_at(vars(nIter:loglik),funs(mean,na.rm=T)) %>%     # get mean of function values after removing NA
  mutate(DLmix = mu2*lambda2)                                 # get Digital Lodging mixture index

# check out the distribution
hist(dat.mix.norm.summary$DLmix)

## export data to disk
write.csv(dat.mix.norm.summary,'output/dat_mix_norm.csv',row.names = FALSE)




