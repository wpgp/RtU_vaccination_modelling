library(raster)
library(maptools)
library(gtools)
library(sp)
library(sf)
library(spdep)
library(ggplot2)

# Get census data for each region (Admin level 1)

census_data <- read.csv("GIN Population Distribution.csv", fileEncoding = "latin1")


# Calculate urban and rural population
census_data$urban_pop <- census_data$urban_pop_percn * census_data$population
census_data$rural_pop <- census_data$rural_pop_percn * census_data$population

# Select columns for the final data frame
final_data <- census_data[, c("Région_Ad", "population", 
                              "urban_pop_percn", "rural_pop_percn", 
                              "urban_pop", "rural_pop")]



# #Population raster
pop <- raster("gin_ppp_2018_1km_v2.tif") #1 km raster
plot(pop)

# Get coordinates
Pred_grid2 <- coordinates(pop)
popc	 <- getValues(pop) 
pred.dat <- cbind(Pred_grid2, popc)

ind <- apply(pred.dat, 1, function(x) any(is.na(x)))
miss    <- which(ind==TRUE)
nonmiss <- which(ind==FALSE)

pred.dat.1 <- pred.dat[nonmiss, ]
coord.p <- pred.dat.1[,1:2]
popd <- pred.dat.1[,3]              #Population counts 
dat_prop <- final_data

# Shapefile of Region level 
#For Admin level 1 estimates
spol2   <- readShapePoly("Region_merged.shp")
spol    = as(spol2, "SpatialPolygons")   #NOTE - no data in spol
sp.2    <- rep(NA, nrow(coord.p))
for(i in 1:length(spol)){
  sp.2[as.vector(which(!is.na(over(SpatialPoints(coord.p), spol[i]))))] <- i
}

#Align dat_prop with shapefile
dat_prop_a <- merge(spol2, dat_prop, by="Région_Ad")
# prop_state_urb <- dat_prop_a$urban_pop_percn/100
prop_state_urb <- dat_prop_a$urban_pop_percn

sp_urb <- numeric(length(sp.2))
for(i in 1:nrow(spol2)){
  dd <- which(sp.2==i)
  if(prop_state_urb[i]<1){
  pp <- popd[dd]
  pps <- sort(pp, decreasing = TRUE)  #Sort pixels in decreasing pop size
  ppo <- order(pp, decreasing = TRUE) #Rank pixels in decreasing pop size
  add <- numeric(length(dd))
  prop <- 0 ; count <- 1 
  out <- 0 #Urban grid cells
  #Determine grid cells to be classified as urban by starting with the pixel with the largest pop
  #until the state-level urban prop is reached. All other pixels are classified as rural
  while(prop <= prop_state_urb[i]){
    prop <- sum(pps[1:count])/sum(pp)
    out <- c(out, ppo[count])
    count <- count + 1
  }
  oo <- numeric(length(dd))
  oo[out[-1]] <- 1
  sp_urb[dd] <- oo
  #sum(pp[oo==1])/sum(pp)  #for checks
  }
  if(prop_state_urb[i]==1) sp_urb[dd] <- 1
}

#Construct output raster
#Mean
ll=1:length(ind); ll[nonmiss] = sp_urb; ll[miss] = NA
urban_rural = raster(pop); values(urban_rural) = ll

#Plot raster
plot(urban_rural)
plot(pop)

writeRaster(urban_rural, "GIN_urban_rural_1km.tif", overwrite=TRUE)




