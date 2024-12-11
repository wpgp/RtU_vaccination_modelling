
#Load these libraries
library(Metrics)
library(plyr)
library(xtable)
library(ggplot2)
library(reshape2)
library(MASS)
require(dplyr) 
library(car)
library(gtools)

set.seed(500) 

#Read in covariate and outcome data sets
cov_dat <- read.csv("Data Files/GIN_covariates.csv", header = TRUE) 
out_dat <- read.csv("Data Files/GIN_DTP_cluster_data_12_23_ratio.csv", header=TRUE) 


#Merge both the outcome and covariate data using the ID variable
#Keep all locations where covariate data are available
data.merge <- merge(cov_dat, out_dat, by = "DHSCLUST", all.x = TRUE) 

#Names of covariates in the data file
cov_names <- names(cov_dat)[-1]

#Rows of data with at least one missing covariate value
del <- numeric(nrow(data.merge))
for (i in 1:nrow(data.merge)){
  if (any(is.na(data.merge[i,2:45]))) del[i] <- i
}

#Delete rows with missing covariate values
if (length(which(del!=0))>0) data.merge <- data.merge[-which(del!=0),]     

#Delete rows of data where no individual was sampled from the data frame
zero.clust <- which(is.na(data.merge$total)|data.merge$total<=1)
if (length(zero.clust)>0){
  data.merge <- data.merge[-zero.clust,]
}

#Round off dtp2_1_count and dtp3_2_count
data.merge$dtp2_1_count <- round(data.merge$dtp2_1_count, 0)
data.merge$dtp3_2_count <- round(data.merge$dtp3_2_count, 0)

#Delete rows where there are no data for dtp2_1_count (DTP2_1_COUNT ONLY)
zero.clust <- which(is.na(data.merge$dtp2_1_count))
if (length(zero.clust)>0){
  data.merge <- data.merge[-zero.clust,]
}

#Delete rows where there are no data for dtp3_2_count (DTP3_2_COUNT ONLY)
zero.clust <- which(is.na(data.merge$dtp3_2_count)|data.merge$dtp3_2_count==Inf)
if (length(zero.clust)>0){
  data.merge <- data.merge[-zero.clust,]
}


#Outcome variable
total_sampled <- data.merge$total # Total number of individuals/children surveyed in each location
num_success   <- data.merge$dtp1_count # For DTP1
# num_success   <- data.merge$dtp2_1_count # For DTP2
# num_success   <- data.merge$dtp3_2_count # For DTP3


#Extract lon-lat coordinates from data frame
dat.coord <- read.csv("Data Files/GIN_cluster_info_complete.csv")

dat_coord <- dat.coord[,c(1,4,3)]
head(dat_coord)

#Data frame with covariates only 
covars <- data.merge[,c(2:45)]

#Use histograms to determine which covariates to log-transform
#Ideally, the relationships between the covariates and the empirical 
#logit transform of the data shoudl be examined to ensure that the transformation
#improves the linearity

par(mfrow=c(2,5))
for (i in 1:10){
  hist(covars[,i], main = names(covars)[i])
}

# windows()
par(mfrow=c(2,5))
for (i in 11:20){
  hist(covars[,i], main = names(covars)[i])
}

# windows()
par(mfrow=c(2,5))
for (i in 21:30){
  hist(covars[,i], main = names(covars)[i])
}

# windows()
par(mfrow=c(2,5))
for (i in 31:40){
  hist(covars[,i], main = names(covars)[i])
}

# windows()
par(mfrow=c(2,5))
for (i in 41:44){
  hist(covars[,i], main = names(covars)[i])
}

#Take logs of heavily skewed covariates
llog <- c(11,
          21, 24:29)

for (i in 1:length(llog)){
  if (i!=4) covars[,llog[i]] <- log(covars[,llog[i]] + 0.05) #0.05 added to avoid taking log of zero
  names(covars)[llog[i]] <- paste0("l_",names(covars)[llog[i]]) #Add "l" to the names of log-transformed covariates
}


#Linearity checks
out_emp_logit <- log(num_success + 0.5/(total_sampled - num_success + 0.5))

#Data frame for plotting
cdat <- covars
# cdat <- covars[c(41:44)]

dd <- data.frame(out_emp_logit,  cdat)

meltdat  = melt(dd, id.vars=c("out_emp_logit"))



#Linear regression lines
plotsingles1=ggplot(meltdat, aes(x=value, y=out_emp_logit, xlab="")) +
  geom_point()+
  #geom_smooth(col="red", method = "loess", span=0.8)+  
  geom_smooth(method="lm", fill=NA)+ 
  stat_smooth(method = "gam", formula = y ~ s(x),se=FALSE, col="green")+ #natural splines
  labs(x="", y="Empirical logit (MCV1)")+
  facet_wrap(~variable, scale="free_x", ncol = 5)+   
  scale_y_continuous()+
  theme(panel.background=element_rect(fill="white", colour="black"))

#Save plot 
#pdf("cov_plot_40_44.pdf", height = 7, width = 12, pointsize = 6)
plotsingles1
#dev.off()

#Remove data sets not needed for further processing
rm(meltdat)
rm(cdat)
rm(dd)


###Declare categorical covariates (e.g. urban-rural) as factors
covars <- data.frame(covars)
# covars <- within(covars, {
#  Urban_rural1 <- factor(Urban_rural1)
# })


#-------------------------Covariate selection starts from here--------------------#

#Fit single covariate models and rank the covariates based on their predictive R-square values
#Create a new data frame for this exercise
Data <- cbind(total_sampled, num_success, covars)

covlist <- names(Data[,-1]) #List of covariates
covlist = covlist[-grep("num_success", covlist)]


##The function below fits single covariate models using a Monte Carlo cross-validation approach
#repeated n.iter times with 20% of the data used for validation each time.
#The validation statistics calculated are averaged over the n.iter repetitions in the end

AICs=dev=nulldev=n.par=r2=pr2=iter=numeric()
model=response=character()
n.iter = 10
propsub = 0.8
resp = "cbind(num_success, total_sampled-num_success)"
resp1 = "num_success"
weights = "total_sampled"

for(i in 1:n.iter){
  subind=sample(1:nrow(Data), propsub*nrow(Data), replace=F)
  subdat=Data[subind,]
  preddat=Data[-subind,]
  ## the null model, for comparison
  nullmod=glm(formula(paste(resp, "~1")), data=Data, family = binomial(logit))
  for(j in covlist){
    form=formula(paste(resp, "~", j))
    pmod=glm(form, data=subdat, family = binomial(logit))
    fullmod=glm(form, data=Data, family = binomial(logit))
    ## pr2 checks the predictive power of the model against a 'new' subset of the data
    pr2=c(pr2,cor(pmod$fitted, subdat[[resp1]]/subdat[[weights]])^2) 
    ## AIC for the model on the full data
    AICs=c(AICs, AIC(fullmod))
    dev=c(dev, deviance(fullmod))
    n.par=c(n.par,1)
    iter=c(iter,i)
    r2=c(r2,cor(fullmod$fitted, Data[[resp1]]/Data[[weights]])^2) 
    model=c(model,j)
    response=c(response, resp1)
    #print(paste(i, j, sep="X"))
  }
}
op = data.frame(model, response, dev, AICs, r2, pr2, n.par )

# Matrix table of all covariates
singles = ddply(op, .(model), summarise,
                model=model[1],
                response=response[1],
                dev=dev[1],
                pr2=mean(pr2),
                AICs=AICs[1],
                r2=r2[1])

### Add deviance reduction
singles$devred = singles$dev/deviance(nullmod)

## Order by pr2 - predictive R-squared
singles = singles[order(singles$pr2, decreasing=TRUE),]

#Ranks
singles <- within(singles, ranks <- 1:nrow(singles))
singles

#Covariates and ranks
cov.rank <- data.frame(covariate=as.character(singles$model), ranks = singles$ranks)

#NB: You could decide to exclude some covariates that didn't perform well after this step
#But for this exercise, we will keep all of them

#################################################################################
#Separate categorical covariates before this step
#Keep old covars data frame
covars.old <- covars  
#Remove urban-rural covariate
#covars     <- covars[,-15]  

##Detection of multicollinearity through correlations between covariates and VIF analysis
#Correlations
#Determine correlations between the covariates and extract highly correlated pairs
#for screening and elimination. 
#Flag covariate pairs with correlations >= 0.8   #change to a higher value if necessary
corrs <- cor(covars[,-1])
bigcors <- matrix(0,1, 2) 
for (i in 2: nrow(corrs)){
  for (j in 1:(i-1)){
    if (abs(corrs[i,j])>=0.8) bigcors <- rbind(bigcors, c(i,j))  #Note 0.8 cut-off
  }
}

if (nrow(bigcors)>2) bigcors <- bigcors[-1,]
if (nrow(bigcors)==2) bigcors <- matrix(bigcors[-1,],1,2)

bigcors.dat <- matrix(0, nrow(bigcors), 3)
for (i in 1:nrow(bigcors)){
  bigcors.dat[i,] <- c(rownames(corrs)[bigcors[i,1]],colnames(corrs)[bigcors[i,2]], 
                       round(corrs[bigcors[i,1],bigcors[i,2]],3))
}

#Pairs of covariates with high correlations
bigcors.dat

#Select between pairs of highly-correlated covariates using their ranks
all.covs <- rownames(corrs)
for (i in 1:nrow(bigcors.dat)){
  #print(i)
  name.cov <- bigcors.dat[i,1:2]
  if (name.cov[1]%in%all.covs && name.cov[2]%in%all.covs){
    r1 <- which(cov.rank$covariate==name.cov[1])
    r2 <- which(cov.rank$covariate==name.cov[2])
    if (r1<r2) all.covs <- all.covs[-which(all.covs==name.cov[2])]
    if (r2<r1) all.covs <- all.covs[-which(all.covs==name.cov[1])]
  }
}

#Check that all remaining covariates are not highly correlated
correl  <- cor(covars[,all.covs])
correl #Correlation matrix

#Create new data frame containing only "uncorrelated" covariates
covars.1      <- covars[,all.covs]

#Reconstitute "Data" to include categorical covariates (urban-rural) 
Data <- cbind(total_sampled, num_success, covars.1)


#VIF analysis using all the remaining covariates
covnames <- as.character(names(covars.1))
form <- paste("cbind(num_success, total_sampled - num_success)","~", paste(covnames, collapse=" + "))
mod.vif <- glm(form, data=Data, family = binomial(logit))
summary(mod.vif)
vif(mod.vif)


########################## Method 1
#StepAIC regression - k = 2 penatly
n <- nrow(Data)
form <- paste("cbind(num_success, total_sampled - num_success)","~", paste(covnames, collapse=" + "))
fit  <- glm(form, data = Data, family = binomial(logit))
mod.step1 <- stepAIC(fit, trace=FALSE, direction = "backward") #Note penalty  
summary(mod.step1) 
vif(mod.step1)


########################## Method 2
#StepAIC regression - k = log(n) penatly
fit  <- glm(form, data = Data, family = binomial(logit))
mod.step2 <- stepAIC(fit, trace=FALSE, direction = "backward", k = log(n)) #Note penalty
summary(mod.step2) 
vif(mod.step2)
#-------------------------------------------------------------------



#DTP1
#-------------------------------------------------------------------
##Selected covariates using BIC penalty
# Results of covariates having VIF values less than 5
#-------------------------------------------------------------------
# gin_ch_2015_1km_v2     gin_dst_wdpa_cat1_2015_2018_1km           gin_pet_2014_2018_1km 
# 1.284304                        2.276787                           2.290141 
# gin_tt_motorised_1km_filled_v2            health_card_doc.prop              sba.prop 
# 1.962638                                    1.112140                        1.675043 


#DTP2_1
#-------------------------------------------------------------------

# l_cmr_dst_ACLED_2014_2018_1km            cmr_sheep_density_2010                  l_cmr_slope_2000 
# 1.301410                                       1.629830                             1.158617 
# MOD11A2_LST_Day_CMR_2014_2018_1km 
# 1.915943 


#DTP3_2
#-------------------------------------------------------------------

# cmr_dist_protected_areas_2017    l_cmr_dst_avg_bsgme_2015_2018_1km_masked 
# 1.340515                                    2.472741 
# MOD11A2_LST_Night_CMR_2014_2018_1km MOD13A2_061_CMR_NDVI_2014_2018_Mean_16d_1km 
# 1.775699                                    2.543021 
# l_CMR_population_v1_0_gridded_1km 
# 2.231888 


# As most of the VIF values are less than 4 for all DTP1, DTP2_1 and DTP3_2
# we didn't delete any covariates and continue with the first set of covariates
# In this case, DTP1 (6), DTP2_1 (5) and DTP3_2 (5)



# Final list of selected Covaraites
#-------------------------------------------------------------------
#Write selected covariate information for the next step of the analysis
covout_dtp1 <- c("gin_ch_2015_1km_v2", 
                 "gin_dst_wdpa_cat1_2015_2018_1km", 
                 "gin_pet_2014_2018_1km", 
                 "gin_tt_motorised_1km_filled_v2",

                 "gin_avg_malaria_prev_2014_2018_1km_filled", 
                 "GIN_MODIS_LST_Day_8day_avg_2014_2018_1km_v2",
          
                 "health_card_doc.prop",
                 "sba.prop",

                 "educ.prop",
                 "wealth.prop",
                 "media.prop"
                 ) 

Data <- cbind(total_sampled, num_success, covars)
Data.out <- Data[,covout_dtp1]
Data.out$DHSCLUST <- data.merge$DHSCLUST
Data.out1 <- merge(Data.out, dat_coord, by = "DHSCLUST")

# Adding Rural/Urban Covariate values in the final data frame
# Merge the data frames based on the DHSCLUST column
merged_data1 <- merge(Data.out1, dat.coord, by = "DHSCLUST", all.x = TRUE)
# Create a new column rural_urban in Data.out1 and populate it with corresponding values from dat.coord
Data.out1$urban1_rural0 <- merged_data1$URBAN_RURA
# Converting Urban to Urban 1 
# Converting Rural to Rural 0 
Data.out1$urban1_rural0[Data.out1$urban1_rural0=="U"] <- 1
Data.out1$urban1_rural0[Data.out1$urban1_rural0=="R"] <- 0

write.csv(Data.out1, "final_covariates_selected.csv")

