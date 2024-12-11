library(INLA)
library(raster)
library(maptools)
library(gtools)
library(sp)
library(spdep)

library(ggplot2)


#loading the data
vaxdata <- read.csv(paste0(filePathData,"GIN_DTP_cluster_data_12_23_ratio.csv"), header=TRUE)
vaxcov  <- read.csv(paste0(filePathData,"final_covariates_selected.csv"),header=TRUE)




#Align both data sets
data.merge <- merge(vaxdata, vaxcov, by="DHSCLUST")

#Change the name of Longitude and Latitude to lon and lat in the dataframe
names(data.merge)[names(data.merge) == 'LONGNUM'] <- 'lon'
names(data.merge)[names(data.merge) == 'LATNUM'] <- 'lat'

vaxcov.all  <- data.merge[,14:27]   #includes coordinates as Longitude & Latitude
vaxdata.all <- data.merge[,2:13]

#Delete clusters where TotChild is zero from vaxdata only
zero.clust <- which(is.na(vaxdata.all$total)|vaxdata.all$total<=1|is.na(vaxdata.all$dtp1_count))
if (length(zero.clust)>0){
  vaxdata.all <- vaxdata.all[-zero.clust,]
  vaxcov.all  <- vaxcov.all[-zero.clust,]
}

#All_ages
Numvacc.all    <- vaxdata.all$dtp1_count
weights.all    <- vaxdata.all$total

#Coordinates
coords.all    <- cbind(vaxcov.all$lon,vaxcov.all$lat)
min.dist  <- as.numeric(summary(dist(coords.all))[1]) #Min of distances between cluster locations #0.001272203

set.seed(200)

#######################################################
#Start k-fold cross-validation loop
cv <- 10  #i.e. 10-fold cross validation
lim <- floor(nrow(coords.all)/cv)

srand <- sample(1:nrow(coords.all),nrow(coords.all), replace=FALSE)

val.out <- matrix(0, cv, 6) 

ppred.out <- data.frame()   #################

for (kk in 1:cv){
 if (kk < cv) {qq <- (((kk-1)*lim)+1):(lim*kk); samp.c <- srand[qq]}
 if (kk == cv) {qq <- (((kk-1)*lim)+1):nrow(coords.all); samp.c <- srand[qq]}
 
coords.nc 	<- coords.all[samp.c,]
Numvacc.nc	<- Numvacc.all[samp.c]
weights.nc	<- weights.all[samp.c]
vaxcov.nc	<- vaxcov.all[samp.c,]

yp.nc=np.nc=rep(NA, length(Numvacc.nc))

#Use the rest for model estimation
coords  <- coords.all[-samp.c,]
Numvacc <- Numvacc.all[-samp.c] 
weights <- weights.all[-samp.c]
vaxcov  <- vaxcov.all[-samp.c,]


#Covariates
xp1     <- vaxcov$gin_ch_2015_1km_v2
xp2     <- vaxcov$gin_dst_wdpa_cat1_2015_2018_1km
xp3	    <- vaxcov$gin_pet_2014_2018_1km
xp4	    <- vaxcov$gin_tt_motorised_1km_filled_v2

xp5	    <- vaxcov$urban1_rural0

xp6     <- vaxcov$gin_avg_malaria_prev_2014_2018_1km_filled
xp7     <- vaxcov$GIN_MODIS_LST_Day_8day_avg_2014_2018_1km_v2
xp8     <- vaxcov$health_card_doc.prop
xp9     <- vaxcov$sba.prop
xp10    <- vaxcov$educ.prop
xp11    <- vaxcov$wealth.prop
xp12    <- vaxcov$media.prop



xv1     <- vaxcov.nc$gin_ch_2015_1km_v2
xv2     <- vaxcov.nc$gin_dst_wdpa_cat1_2015_2018_1km
xv3	    <- vaxcov.nc$gin_pet_2014_2018_1km
xv4	    <- vaxcov.nc$gin_tt_motorised_1km_filled_v2
xv5	    <- vaxcov.nc$urban1_rural0
xv6     <- vaxcov.nc$gin_avg_malaria_prev_2014_2018_1km_filled
xv7     <- vaxcov.nc$GIN_MODIS_LST_Day_8day_avg_2014_2018_1km_v2
xv8     <- vaxcov.nc$health_card_doc.prop
xv9     <- vaxcov.nc$sba.prop
xv10    <- vaxcov.nc$educ.prop
xv11     <- vaxcov.nc$wealth.prop
xv12     <- vaxcov.nc$media.prop



#Import CIV Admin Level0 Shapefiles
shp_civ  <- readShapePoly("Data/Shapefiles/Vectorised Shapefiles/National.shp")



#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Design SPDE mesh
#meshfit: fine triangulated mesh
meshfit <- inla.mesh.2d(loc = coords,boundary = shp_civ, 
                        max.edge = c(0.05, 0.5),
                        offset = c(0.5, 0.5),
                        cutoff = 0.05)

# Display number of vertices in the mesh
meshfit$n 

#Display SPDE mesh
plot(meshfit)
plot(shp_civ, add = TRUE)
points(coords, pch = 16, cex = 0.5, col = "red")


#-------------------------------------------------------------------------


#For priors
nu <- 1 #Matern smoothness parameter, redundant here as it implies alpha=2
alpha <- 2

#This is 5% of the extent of Cameroon in the north-south direction (i.e. 0.05*(ymax-ymin))
ymax <- max(coords[,2])
ymin <- min(coords[,2])
r0 <- 0.05*(ymax-ymin) #0.3095309


#Matern SPDE model object using inla.pcmatern
spde <- inla.spde2.pcmatern(mesh=meshfit, alpha=alpha, prior.range=c(r0, 0.01), prior.sigma=c(1, 0.01))

# Observation points
X0 <- model.matrix(~ -1 + xp1 + 
                     xp2 +
                     xp3 + 
                     xp4 +
                     factor(xp5) + 
                     xp6 + 
                     xp7 + xp8 + xp9 + xp10 + xp11 + xp12)


Xobs <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xp5)0"))])
#Xobs <-  as.data.frame(X0)

colnames(Xobs) <- c("x1", 
                    "x2",
                    "x3", 
                    "x4",
                    "x5_1", 
                    "x6", 
                    "x7", "x8", "x9", "x10", "x11", "x12")
Ap.i <- inla.spde.make.A(mesh=meshfit, loc=coords)
lp.i = rep(1,length(xp1))
stk.point <- inla.stack(tag='point',
                        data=list(y=Numvacc,n=weights),
                        A=list(Ap.i,1,1,1),
                        effects=list(s=1:spde$n.spde, rr=1:length(weights), intercept=rep(1, length(xp1)), Xobs))  #NOTE

# Validation points
X0 <- model.matrix(~ -1 + xv1 + 
                     xv2 +
                     xv3 + 
                     xv4 +
                     factor(xv5) + 
                     xv6 + 
                     xv7 + xv8 + xv9 + xv10 + xv11 + xv12)

Xval <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xv5)0"))])

colnames(Xval) <- c("x1", 
                    "x2",
                    "x3", 
                    "x4",
                    "x5_1", 
                    "x6", 
                    "x7", "x8", "x9", "x10", "x11", "x12")
Aval <- inla.spde.make.A(mesh=meshfit, loc=coords.nc)
lval = rep(1,length(xv1))
stk.val <- inla.stack(tag='val',
                      data=list(y=yp.nc,n=np.nc),
                      A=list(Aval,1,1,1),
                      effects=list(s=1:spde$n.spde, rr=(length(weights)+1):(length(weights)+length(xv1)), intercept=rep(1, length(xv1)), Xval)) #NOTE

#Points, grid
# Stack
stk.full <- inla.stack(stk.point)  #Note no stk.pred and stk.val

# Fit model
hyper.prec = list(theta = list(prior="pc.prec", param=c(3,0.01)))
control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000)  

#Note hyperparamters for iid RE, in INLA default is 1, 0.00001 - see documentation                                                         
# beta_i has a N(0, 10^6) default prior
formula  <- y ~ -1 + intercept + 
  x1 + 
  x2 +
  x3 + 
  x4 +
  x5_1 + 
  x6 + 
  x7 + x8 + x9 + x10 + x11 + x12 +
  f(s, model=spde) + f(rr, model="iid", hyper = hyper.prec) 
res <- inla(formula, data=inla.stack.data(stk.full), family="binomial", 
            Ntrials = stk.full$data$data$n,
            control.predictor=list(compute=TRUE, A=inla.stack.A(stk.full), link=1),
            control.compute=list(dic=TRUE, config = TRUE, waic=TRUE),
            control.fixed=control.fixed)


spde.result <- inla.spde2.result(inla=res, name="s", spde=spde)



##################POSTERIOR SAMPLING
nsamp <- 1000

#Posterior sampling
ps <- inla.posterior.sample(nsamp, res) 
contents <- res$misc$configs$contents

#ID for spatial random effect
idSpace <- contents$start[which(contents$tag=="s")]-1 +
  (1:contents$length[which(contents$tag=="s")])

#ID for iid effects
idR <- contents$start[which(contents$tag=="rr")]-1 +
  (1:contents$length[which(contents$tag=="rr")])

#ID for fixed effects
idX <- contents$start[which(contents$tag=="intercept")]-1 + (1:13) # fixed effects, 12 = no of regression coefficients

# extract samples 
xLatent <- matrix(0, nrow=length(ps[[1]]$latent), ncol=nsamp) 
xHyper <- matrix(0, nrow=length(ps[[1]]$hyperpar), ncol=nsamp) 
for(i in 1:nsamp){
  xLatent[,i] <- ps[[i]]$latent
  xHyper[,i] <- ps[[i]]$hyperpar
}
xSpace <- xLatent[idSpace,]
XR <- xLatent[idR,]
xX <- xLatent[idX,]



#Validation
#Draw samples for IID term
sample.IIDval <- matrix(0, length(xv1),  nsamp)
for (i in 1:nsamp){
  ID.precision <- xHyper[3,i]              #the 3rd row contains precision for rr; same as ps[[i]]$hyperpar[3]
  #ID.precision <- xHyper[2,i]              #the 3rd row contains precision for rr; same as ps[[i]]$hyperpar[3]
  ID.sigma <- ID.precision^-0.5
  sample.IIDval[, i] <- rnorm(length(xv1), sd=ID.sigma)
}

linpred     <- as.matrix(Aval %*% xSpace + as.matrix(cbind(1, Xval)) %*% xX + sample.IIDval)
inv.linpred <- inv.logit(linpred) 
pred.val    <- data.frame(t(apply(inv.linpred, 1, FUN=function(x){ c(mean(x), sd(x), quantile(x, probs=c(0.025,0.5,0.975)))}))) 
colnames(pred.val) <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant")
fitted.mean.val <- as.vector(data.matrix(as.vector(pred.val[,"mean"])))
fitted.low.val  <- as.vector(data.matrix(as.vector(pred.val[,"0.025quant"])))
fitted.up.val   <- as.vector(data.matrix(as.vector(pred.val[,"0.975quant"])))

prob.val <- Numvacc.nc/weights.nc
#plot(prob.val, fitted.mean.val)
corr <- cor(fitted.mean.val,prob.val)
rsq.val  <- (cor(fitted.mean.val,prob.val))^2
VMSE.val <- sum((fitted.mean.val-prob.val)^2)/length(prob.val)

count <- 0
for(r in 1:length(prob.val)){
  if ((prob.val[r] >= fitted.low.val[r]) && (prob.val[r] <= fitted.up.val[r])) count <- count + 1
}
cov.rate.val <- (count/length(prob.val))*100

perc_bias <- (sum(fitted.mean.val-prob.val)/sum(prob.val))*100
avg_bias <- sum(fitted.mean.val-prob.val)/length(prob.val)

gg <- cbind(prob.val, pred.val) 
ppred.out <- rbind(ppred.out, gg) 
val.out[kk, ] <- c(corr, rsq.val, VMSE.val, cov.rate.val, perc_bias, avg_bias)
print(val.out[kk, ])
}

colnames(val.out) <- c("correl", "rsq.val", "VMSE.val", "cov.rate.val", "perc_bias", "avg_bias")
write.csv(val.out, "val_out_dtp1_rand_ratio.csv")


colnames(ppred.out) <- c("prob.obs", "mean", "sd", "pred.low", "0.5quant", "pred.up") 
write.csv(ppred.out, "val_obs_pred_dtp1_rand_ratio.csv") 
