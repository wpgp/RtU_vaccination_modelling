
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

vaxcov  <- data.merge[,14:27]   #includes coordinates as Longitude & Latitude
vaxdata <- data.merge[,2:13]


#Delete clusters where TotChild is zero from vaxdata only
zero.clust <- which(is.na(vaxdata$total)|vaxdata$total<=1|is.na(vaxdata$dtp1_count))
if (length(zero.clust)>0){
  vaxdata <- vaxdata[-zero.clust,]
  vaxcov  <- vaxcov[-zero.clust,]
}

#All_ages
Numvacc    <- vaxdata$dtp1_count
weights    <- vaxdata$total

#Coordinates
coords    <- cbind(vaxcov$lon,vaxcov$lat)
min.dist  <- as.numeric(summary(dist(coords))[1]) #Min of distances between cluster locations #0.0003146808

set.seed(500)

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


#Read in prediction covariates
chk_count        <- raster(paste0(filePathData1,"gin_ch_2015_1km_v2.tif"))
dst_wdpa         <- raster(paste0(filePathData1,"gin_dst_wdpa_cat1_2015_2018_1km.tif"))
pet_val          <- raster(paste0(filePathData1,"gin_pet_2014_2018_1km.tif"))
tt_motorised     <- raster(paste0(filePathData1,"gin_tt_motorised_1km_filled_v2.tif"))

urban_rural	     <- raster(paste0(filePathData1,"GIN_urban_rural_1km.tif"))

avg_malaria      <- raster(paste0(filePathData1,"gin_avg_malaria_prev_2014_2018_1km_filled.tif"))
MODIS_LST_Day    <- raster(paste0(filePathData1,"GIN_MODIS_LST_Day_8day_avg_2014_2018_1km_v2.tif"))
dhs_health_card	 <- raster(paste0(filePathData1,"GIN_health_card_doc_krig.tif"))
dhs_sba          <- raster(paste0(filePathData1,"GIN_sba_krig.tif"))
dhs_educ         <- raster(paste0(filePathData1,"GIN_educ_krig.tif"))
dhs_wealth       <- raster(paste0(filePathData1,"GIN_wealth_krig.tif"))
dhs_media        <- raster(paste0(filePathData1,"GIN_media_krig.tif"))


#Prediction covariates
x1gp  <- getValues(chk_count)
x2gp 	<- getValues(dst_wdpa) 
x3gp 	<- getValues(pet_val)
x4gp	<- getValues(tt_motorised)

x5gp	<- getValues(urban_rural)

x6gp	<- getValues(avg_malaria)
x7gp	<- getValues(MODIS_LST_Day)
x8gp	<- getValues(dhs_health_card)
x9gp	<- getValues(dhs_sba)
x10gp	<- getValues(dhs_educ)
x11gp	<- getValues(dhs_wealth)
x12gp	<- getValues(dhs_media)

#Prediction grid
n25.p <- raster(paste0(filePathData1,"gin_mastergrid_1km_reclass.tif"))
Pred_grid2 <- coordinates(n25.p)

#Population data for population-weighted aggregation
popc     <- raster(paste0(filePathData1,"gin_f_15_49_2018_1km_R_filled.tif"))
popc	 <- getValues(popc) 

#Combine grid and covariates
pred.dat <- cbind(Pred_grid2, x1gp, 
                  x2gp, x3gp, x4gp, 
                  x5gp, 
                  x6gp, x7gp, x8gp, x9gp, x10gp, x11gp, x12gp, popc)

ind <- apply(pred.dat, 1, function(x) any(is.na(x)))
miss    <- which(ind==TRUE)
nonmiss <- which(ind==FALSE)

pred.dat.1 <- pred.dat[nonmiss, ]
pop <- pred.dat.1[,15]              #Population counts for weighted aggregation
coord.p <- pred.dat.1[,1:2]
ypred=npred=rep(NA, nrow(pred.dat.1))

#Import CIV Admin Level0 Shapefiles
shp_gin  <- readShapePoly("Data/Shapefiles/Vectorised Shapefiles/National.shp")

# Design SPDE mesh
#meshfit: fine triangulated mesh
meshfit <- inla.mesh.2d(loc = coords,boundary = shp_gin, 
                        max.edge = c(0.05, 0.5),
                        offset = c(0.5, 0.5),
                        cutoff = 0.05)

# Display number of vertices in the mesh
meshfit$n #6086

#Display SPDE mesh
#meshfit <- inla.mesh.2d(loc=coords,loc.domain=c.bnd, max.edge=c(0.2, 0.6),cutoff=0.18)
plot(meshfit)
plot(shp_gin, add=TRUE)
points(coords, pch = 16, cex = 0.5, col = "red")




#For priors
nu <- 1 #Matern smoothness parameter, redundant here as it implies alpha=2
alpha <- 2


#This is 5% of the extent of Cameroon in the north-south direction (i.e. 0.05*(ymax-ymin))
ymax <- max(coords[,2])
ymin <- min(coords[,2])
r0 <- 0.05*(ymax-ymin) #0.3095309


#Matern SPDE model object using inla.pcmatern
spde <- inla.spde2.pcmatern(mesh=meshfit, alpha=alpha, prior.range = c(r0, 0.01), prior.sigma = c(1, 0.01)) 


#For LGA-level estimates
coord.p <- pred.dat.1[,1:2]
# #--------------------------------------------------------------
#For SP-level estimates (344 units) SP
spol1   <- readShapePoly("Data/Shapefiles/Vectorised Shapefiles/SOUS_PREFECTURE.shp")
spol    = as(spol1, "SpatialPolygons")   #NOTE - no data in spol
sp.1    <- rep(NA, nrow(coord.p))
for(i in 1:length(spol)){
  sp.1[as.vector(which(!is.na(over(SpatialPoints(coord.p), spol[i]))))] <- i
}
# #--------------------------------------------------------------
#For PREFECTURE-level estimates (40 units) PREFECTURE
spol2   <- readShapePoly("Data/Shapefiles/Vectorised Shapefiles/PREFECTURE.shp")
spol    = as(spol2, "SpatialPolygons")   #NOTE - no data in spol
sp.2    <- rep(NA, nrow(coord.p))
for(i in 1:length(spol)){
  sp.2[as.vector(which(!is.na(over(SpatialPoints(coord.p), spol[i]))))] <- i
}
# #--------------------------------------------------------------
#For Region-level estimates (13 units) Region
spol3   <- readShapePoly("Data/Shapefiles/Vectorised Shapefiles/REGION.shp")
spol    = as(spol3, "SpatialPolygons")   #NOTE - no data in spol
sp.3    <- rep(NA, nrow(coord.p))
for(i in 1:length(spol)){
  sp.3[as.vector(which(!is.na(over(SpatialPoints(coord.p), spol[i]))))] <- i
}

# #--------------------------------------------------------------
# #--------------------------------------------------------------


# Observation points
X0 <- model.matrix(~ -1 + xp1 + xp2 + 
                     xp3 + 
                     xp4 + factor(xp5) + 
                     xp6 + 
                     xp7 + xp8 + xp9 + xp10 + xp11 + xp12)

Xobs <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xp5)0"))])

colnames(Xobs) <- c("x1", "x2", "x3", "x4", 
                    "x5_1", 
                    "x6", "x7", "x8", "x9", "x10", "x11", "x12")


Ap.i <- inla.spde.make.A(mesh=meshfit, loc=coords)
lp.i = rep(1,length(xp1))
stk.point <- inla.stack(tag='point',
                        data=list(y=Numvacc,n=weights),
                        A=list(Ap.i,1,1,1),
                        effects=list(s=1:spde$n.spde, rr=1:length(weights), intercept=rep(1, length(xp1)), Xobs))  #NOTE

# Prediction points - can be moved outside the loop
xpred1 <- pred.dat.1[,3]
xpred2 <- pred.dat.1[,4]
xpred3 <- pred.dat.1[,5]
xpred4 <- pred.dat.1[,6]
xpred5 <- pred.dat.1[,7]
xpred6 <- pred.dat.1[,8]
xpred7 <- pred.dat.1[,9] 
xpred8 <- pred.dat.1[,10]
xpred9 <- pred.dat.1[,11]
xpred10 <- pred.dat.1[,12]
xpred11 <- pred.dat.1[,13]
xpred12 <- pred.dat.1[,14]

X0 <- model.matrix(~ -1 + xpred1 + xpred2 + xpred3 + xpred4 + 
                     factor(xpred5) + 
                     xpred6 + xpred7 + xpred8 + xpred9 + xpred10 + xpred11 + xpred12)

Xpred <-  as.data.frame(X0[,-which(colnames(X0)%in%c("factor(xpred5)0"))])


colnames(Xpred) <- c("x1", "x2", "x3", "x4", 
                     "x5_1", 
                     "x6", "x7", "x8", "x9", "x10", "x11", "x12")


Apred <- inla.spde.make.A(mesh=meshfit, loc=coord.p)
lpred = rep(1,nrow(pred.dat.1))
stk.pred <- inla.stack(tag='pred',
                       data=list(y=ypred,n=npred),
                       A=list(Apred,1,1),
                       effects=list(s=1:spde$n.spde, rr=(length(weights)+1):(length(weights)+nrow(pred.dat.1)), Xpred)) #NOTE

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
  x7 + 
  x8 + 
  x9+ 
  x10 + 
  x11 + 
  x12 +
  f(s, model=spde) + f(rr, model="iid", hyper = hyper.prec) 


##################POSTERIOR SAMPLING
nsamp <- 1000

filePathData3 <- "Results/DTP1/"
load(file=paste0(filePathData3,"simprob_dtp1.rda"))
inv.linpred_dtp1 <- inv.linpred

filePathData4 <- "Results/DTP2_1/"
load(file=paste0(filePathData4,"simprob_dtp2_1.rda"))
inv.linpred_dtp2_1 <- inv.linpred


###################################################
#For DTP2

inv.linpred_dtp2 <- inv.linpred_dtp2_1 * inv.linpred_dtp1
inv.linpred <- inv.linpred_dtp2

pred.grid    <- data.frame(t(apply(inv.linpred, 1, FUN=function(x){ c(mean(x), sd(x), quantile(x, probs=c(0.025,0.5,0.975)))}))) 
colnames(pred.grid) <- c("mean", "sd", "0.025quant", "0.5quant", "0.975quant")
fitted.pred.mean   <- as.vector(data.matrix(as.vector(pred.grid[,"mean"])))
fitted.pred.sd     <- as.vector(data.matrix(as.vector(pred.grid[,"sd"])))
fitted.pred.median <- as.vector(data.matrix(as.vector(pred.grid[,"0.5quant"])))
fitted.pred.low    <- as.vector(data.matrix(as.vector(pred.grid[,"0.025quant"])))
fitted.pred.up     <- as.vector(data.matrix(as.vector(pred.grid[,"0.975quant"])))
#--------------------------------------------------------------------

#Remove data sets not needed for further processing
rm(inv.linpred_dtp1)
rm(inv.linpred_dtp2)
rm(inv.linpred_dtp2_1)
rm(avg_wetdays)
rm(dst_ACLED)
rm(dst_UCDP)
rm(l_civ_Pg)
rm(urban_rural)
rm(proximity_border)
rm(dhs_educ)
rm(dhs_health_card)
rm(wealth.prop)
rm(media.prop)
rm(avg_malaria)
rm(avg_pet)


# Force R to perform this check, and free the memory right away,
gc()
#--------------------------------------------------------------------


#Prediction grid
n25.p <- raster(paste0(filePathData1,"gin_mastergrid_1km_reclass.tif"))

#save.image("test_DTP2.RData")
# load("test_DTP2.RData")

#Mean
ll=1:length(ind); ll[nonmiss] = fitted.pred.mean; ll[miss] = NA
rr.mean = raster(n25.p)
values(rr.mean) = ll

#sd
ll=1:length(ind); ll[nonmiss] = fitted.pred.sd; ll[miss] = NA
rr.sd = raster(n25.p); values(rr.sd) = ll

#low
ll=1:length(ind); ll[nonmiss] = fitted.pred.low; ll[miss] = NA
rr.low = raster(n25.p); values(rr.low) = ll

#up
ll=1:length(ind); ll[nonmiss] = fitted.pred.up; ll[miss] = NA
rr.up = raster(n25.p); values(rr.up) = ll

#median
ll=1:length(ind); ll[nonmiss] = fitted.pred.median; ll[miss] = NA
rr.med = raster(n25.p); values(rr.med) = ll

writeRaster(rr.mean, paste0(filePathData2, "inla_vax_dtp2_mean_ratio.tif"), overwrite=TRUE)
writeRaster(rr.sd,   paste0(filePathData2, "inla_vax_dtp2_sd_ratio.tif"), overwrite=TRUE)
writeRaster(rr.low,  paste0(filePathData2, "inla_vax_dtp2_low_ratio.tif"), overwrite=TRUE)
writeRaster(rr.up,   paste0(filePathData2, "inla_vax_dtp2_up_ratio.tif"), overwrite=TRUE)
writeRaster(rr.med,  paste0(filePathData2, "inla_vax_dtp2_median_ratio.tif"), overwrite=TRUE)

#Calculate weighted population SP, Prefecture and Regions estimates
#--------------------------------------------------------------
#For SP-level estimates and Uncertainty (344 units) SP
#--------------------------------------------------------------
# #SP estimates and uncertainty (sd) 
dd    <- 1:nrow(spol1)
dd.un <- unique(sp.1)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.1==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.1==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred[which(sp.1==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.1==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.1==dd_num[i])]
    ext <- as.vector(apply(inv.linpred[which(sp.1==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext), sd(ext), quantile(ext, probs=c(0.025,0.5,0.975))))						
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, "SP_estimates_dtp2.csv")

#-----------------------------------------------------------
##For PREFECTURE-level estimates and Uncertainty (40 units) PREFECTURE
#--------------------------------------------------------------
dd    <- 1:nrow(spol2)
dd.un <- unique(sp.2)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.2==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.2==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred[which(sp.2==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.2==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.2==dd_num[i])]
    ext <- as.vector(apply(inv.linpred[which(sp.2==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext), sd(ext), quantile(ext, probs=c(0.025,0.5,0.975))))						
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0(filePathData2, "Prefecture_estimates_dtp2.csv"))

#-----------------------------------------------------------
#Region estimates and uncertainty (13 Units)
#-----------------------------------------------------------
dd    <- 1:nrow(spol3)
dd.un <- unique(sp.3)
dmiss <- which(!dd%in%dd.un)

if (length(dmiss)>0) dd_num <- dd[-dmiss]
if (length(dmiss)==0) dd_num <- dd

dist_out <- matrix(0, length(dd_num), 5)
for (i in 1:length(dd_num)){
  if (length(which(sp.3==dd_num[i]))==1){ 
    pop.ext <- pop[which(sp.3==dd_num[i])] 
    ext <- as.vector(sapply(inv.linpred[which(sp.3==dd_num[i]),], FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE))) 
  }
  if (length(which(sp.3==dd_num[i]))>1){  
    pop.ext <- pop[which(sp.3==dd_num[i])]
    ext <- as.vector(apply(inv.linpred[which(sp.3==dd_num[i]),], 2, FUN=function(x) weighted.mean(x, w=pop.ext, na.rm=TRUE)))
  }
  
  dist_out[i,] <- as.vector(c(mean(ext), sd(ext), quantile(ext, probs=c(0.025,0.5,0.975))))						
}

dist_out <- cbind(dd_num, dist_out)
colnames(dist_out) <- c("ID", "mean", "sd", "0.025quant", "0.5quant", "0.975quant")

#The district-level estimates will have the same ordering as in the shapefile if they have the same no of areas
write.csv(dist_out, paste0(filePathData2, "Region_estimates_dtp2.csv"))


#-------------------------------------------------------------------------------
#Threshold calculations - 80% Coverage
ff1=function(x) length(which(x>=0.80))/nsamp
y.80 <- apply(inv.linpred, 1, ff1)   #Check me
ll=1:length(ind); ll[nonmiss] = y.80; ll[miss] = NA
rr.80 = raster(n25.p); values(rr.80) = ll
writeRaster(rr.80, paste0(filePathData2, "inla_vax_dtp2_80perc_thresh.tif"), overwrite=TRUE)


