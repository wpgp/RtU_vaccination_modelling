#load packages
library(broom)
library(data.table)
library(DescTools)
library(dplyr)
library(fields)
library(foreign)
library(MASS)
library(naniar)
library(plyr)
library(raster)
library(readstata13)
library(rms)
library(spdep)
library(summarytools)
library(tidyverse)
library(lubridate)
library(zoo)


#Read household, woman and child data files
ngdhs_hh <- read.dta("Data/GNHR71FL.DTA",
                    convert.factors=TRUE)      #DHS household recode
ngdhs_ch <- read.dta("Data/GNKR71FL.DTA",
                    convert.factors=TRUE)      #DHS children's recode
ngdhs_wo <- read.dta("Data/GNIR71FL.DTA",
                    convert.factors=TRUE)      #DHS women's recode

#Create unique ids by combining cluster, household and individual (woman) ids
hh<- ngdhs_hh %>% 
  mutate(clus_hhid = paste(hv001, hv002, sep = "_"))
ch<- ngdhs_ch %>% 
  mutate(clus_hhid = paste(v001, v002, sep = "_"),
         caseid = paste(clus_hhid, v003, sep = "_"))

#Prefix variable names in women file with "w" to distinguish
#it from the child variables
colnames(ngdhs_wo)<- paste("w",colnames(ngdhs_wo), sep = "")
wo<- ngdhs_wo %>% 
  mutate(clus_hhid = paste(wv001, wv002, sep = "_"),
         caseid = paste(clus_hhid, wv003, sep = "_"))

#Join household, woman and child files using unique id created
hh_ch_wo <- ch %>% 
  left_join(hh, by = "clus_hhid") %>% 
  left_join(wo, by = "caseid") 

rm(ngdhs_ch,ngdhs_hh,ngdhs_wo)

#Selection of variables
hh_ch_wo <- hh_ch_wo %>%
  dplyr::select(caseid, v001, v002, v003, b5, b8, hw1, wv106, hv270, h1a, hv009, wv171a, wv169a, 
                #hw5, 
                v157, v158, v159,
                wv130, m3a, m3b, m3c, b2, v006, v007, b1) %>%
  dplyr::rename(DHSCLUST=v001, alive = b5, age_in_years = b8, age_child=hw1, educ=wv106, wealth=hv270, health_card_doc=h1a,
                hhsize=hv009, internet=wv171a, m_phone=wv169a, 
                #stunting=hw5, 
                newspaper=v157, radio=v158, tv=v159,
                religion_w=wv130, yob=b2, moi=v006, yoi=v007, mob=b1)

#Filter age and children alive
hh_ch_wo <- hh_ch_wo %>% 
  filter(alive == "yes") 
#%>%  filter(age_in_years == 1 |age_in_years == 2 )

#Calculate child's age in months to replace missing values in age_child
hh_ch_wo <- hh_ch_wo %>%
  mutate(y=yoi-yob,
         m=moi-mob,
         age_months_new= (12*y) + m)

#Recode variables
hh_ch_wo <- hh_ch_wo %>%
  mutate(wealth= recode(wealth,
                        "poorest"=0,
                        "poorer"=0,
                        "middle"=1,
                        "richer"=1,
                        "richest"=1)) %>%
  mutate(health_card_doc = recode(health_card_doc,
                                  "does not have either card or other document" = 0,
                                  "has card/other document and none were seen" = 0,
                                  "has only health card and wasn't seen" = 0,
                                  "has only other document and wasn't seen" = 0,
                                  "has card/other document and both were seen" = 1,
                                  "has card/other document but only card was seen" = 1,
                                  "has card/other document but only other document was seen" = 1,
                                  "has only health card and was seen" = 1,
                                  "has only other document and was seen" = 1)) %>% 
  mutate(sba = if_else(m3a=="yes"|m3b=="yes", 1,0)) %>% #|m3c=="yes" -----NOTE
  mutate(educ = recode(educ,
                       "no education" = 0,
                       "primary" = 1,
                       "secondary" = 1,
                       "higher" = 1)) %>%
  #mutate(stunting = if_else(stunting < -2, 0, 1)) %>%
  mutate(newspaper= recode(newspaper,
                           "less than once a week" = 0,
                           "not at all" = 0,
                           "at least once a week" = 1),
         radio= recode(radio,
                       "less than once a week" = 0,
                       "not at all" = 0,
                       "at least once a week" = 1),
         tv= recode(tv,
                    "less than once a week" = 0,
                    "not at all" = 0,
                    "at least once a week" = 1)) %>% 
  mutate(media= if_else(newspaper == 1 | radio == 1 | tv == 1, 1, 0)) %>% 
  mutate(religion_w = recode(religion_w,
                             "catholic" = 0,
                             "other christians" = 0,
                             #"seventh day adventist / baptist" = 0,
                             "protestant" = 0,
                             #"other christian" = 0,
                             "muslim" = 1,
                             "animist" = 1,
                             "none" = 1,
                             "other" = 1)) %>% 
  mutate(m_phone = if_else(m_phone == "yes", 1,0)) %>%
  mutate(internet = recode(internet,
                           "never" = 0,
                           "yes, before last 12 months" = 1,
                           "yes, last 12 months" = 1)) %>% 
  mutate(phone_internet = if_else(m_phone==1 | internet==1, 1, 0)) 


#Selection of variables
ch_rec <- hh_ch_wo %>%
  dplyr::select(DHSCLUST, v002, v003, age_months_new, age_in_years, wealth,
                health_card_doc, sba, 
                #stunting, 
                educ, media, religion_w, m_phone,
                phone_internet, hhsize) 

nrow(ch_rec)

## Omit rows from the dataframe with missing entries 
#ch_rec			<- ch_rec[complete.cases(ch_rec$health_card_doc),]

#Aggregation to cluster level - complete cases
aggDHS1 		<- ch_rec %>% group_by(DHSCLUST)
aggDHS1 		<- aggDHS1 %>% dplyr::summarise( 
  wealth.y = sum(wealth), 
  wealth.n = n(),
  educ.y = sum(educ), 
  educ.n = n(),
  sba.y = sum(sba),
  sba.n = n(),
  media.y = sum(media),
  media.n = n(),
  hhsize = mean(hhsize),
  religion.y = sum(religion_w),
  religion.n = n(),
  phone_internet.y = sum(phone_internet),
  phone_internet.n = n())

#Aggregation to cluster level - health_card_doc
ch_rec1			<- ch_rec[complete.cases(ch_rec$health_card_doc),]
aggDHS2 		<- ch_rec1 %>% group_by(DHSCLUST)
aggDHS2 		<- aggDHS2 %>% dplyr::summarise(
  health_card_doc.y = sum(health_card_doc),
  health_card_doc.n = n())%>%
  dplyr::select(DHSCLUST, health_card_doc.y, health_card_doc.n) 

# #Aggregation to cluster level - stunting
# ch_rec2			<- ch_rec[complete.cases(ch_rec$stunting),]
# aggDHS3 		<- ch_rec2 %>% group_by(DHSCLUST)
# aggDHS3 		<- aggDHS3 %>% dplyr::summarise(
#   stunting.y = sum(stunting),
#   stunting.n = n()) %>%
#   dplyr::select(DHSCLUST, stunting.y, stunting.n) 

#Join all three data frames
aggDHS <- aggDHS1 %>% 
  left_join(aggDHS2, by = "DHSCLUST") 

#%>% left_join(aggDHS3, by = "DHSCLUST")   



#Read in cluster coordinates and join to data frame
clustlonlat		<- read.csv("Data/GIN_cluster_coordinates.csv", head = T)

#Join to data frame
aggDHS		<- aggDHS %>% 
  left_join(clustlonlat, by = "DHSCLUST")

#Create data frame
clustdf 		<- as.data.frame(aggDHS)
head(clustdf)

## Tidy up the data frame.
clustdf$wealth.prop	<- round(clustdf$wealth.y/clustdf$wealth.n,4)
clustdf$educ.prop	<- round(clustdf$educ.y/clustdf$educ.n,4)
clustdf$health_card_doc.prop	<- round(clustdf$health_card_doc.y/clustdf$health_card_doc.n,4)
clustdf$sba.prop	<- round(clustdf$sba.y/clustdf$sba.n,4)
clustdf$media.prop	<- round(clustdf$media.y/clustdf$media.n,4)
clustdf$religion.prop	<- round(clustdf$religion.y/clustdf$religion.n,4)
clustdf$phone_internet.prop	<- round(clustdf$phone_internet.y/clustdf$phone_internet.n,4)
#clustdf$stunting.prop	<- round(clustdf$stunting.y/clustdf$stunting.n,4)
clustdf$hhsize		<- round(clustdf$hhsize,4)
head(clustdf)
dim(clustdf)
write.csv(clustdf, "clustdf.csv")

#Delete clusters with missing coords
del <- which(is.na(clustdf$LATNUM))
if (length(del)>0) clustdf <- clustdf[-del,]




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## The first exercise that we will do is to compare the spatial parameter (that
## is an additional argumement) from the Krig function in the "fields" package.
## We will compare by randomly selecting 80% of the data as training, and the
## remaining 20% as validation. 


set.seed(1212)

ll 		<- dim(clustdf)[[1]]					
nc 		<- (20/100)*ll				 	
samp.c 	<- sample(1:ll, nc, replace = F)

# 80% Training (Kriging)
coords.cc		<- clustdf[-samp.c, 17:18]
wealth.cc 		<- clustdf$wealth.prop[-samp.c] 		
educ.cc 	<- clustdf$educ.prop[-samp.c] 		
health_card_doc.cc 		<- clustdf$health_card_doc.prop[-samp.c] 		
sba.cc		<- clustdf$sba.prop[-samp.c] 	
media.cc		<- clustdf$media.prop[-samp.c] 	
hhsize.cc	<- clustdf$hhsize[-samp.c] 	
phone_internet.cc	<- clustdf$phone_internet.prop[-samp.c] 
#stunting.cc	<- clustdf$stunting.prop[-samp.c] 
religion.cc	<- clustdf$religion.prop[-samp.c] 

# 20% Validation (Prediction)
coords.nc		<- clustdf[samp.c, 17:18]
wealth.nc 		<- clustdf$wealth.prop[samp.c] 		
educ.nc 	<- clustdf$educ.prop[samp.c] 		
health_card_doc.nc 		<- clustdf$health_card_doc.prop[samp.c] 		
sba.nc		<- clustdf$sba.prop[samp.c] 	
media.nc		<- clustdf$media.prop[samp.c] 	
hhsize.nc	<- clustdf$hhsize[samp.c] 	
phone_internet.nc	<- clustdf$phone_internet.prop[samp.c] 
#stunting.nc	<- clustdf$stunting.prop[samp.c] 
religion.nc	<- clustdf$religion.prop[samp.c]  	

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## We "train" the data via. the Krig function (alongside the training coordinates)
## Then use the predict function with the testing coordinates. Afterwards, we 
## calculate the mean absolute error (MAE) and root mean square error (RMSE)
## using the validation (observed) proportions.

## The difference between the function CompareSP1 and CompareSP2 is a logit and
## back transformation. As the Krig function assumes a Gaussian random spatial 
## processes, we can result with predictions that are beyond [0,1]. But as we 
## are working with proportions, anything beyond the interval does not make a 
## lot of sense. A remedy to this problem is to first transform the proportions
## to a logit scale; run krig function on the logit scaled data; predict with the 
## logit scaled data; then back-transform it when writing it on a raster file.

## Before this, we made minor adjustments to the data. As there will be proportions
## of 0 and 1, this will result in logits of 0 and Inf. Hence, we "shift" 0 "up"
## by a very small value (0.0001) and "shift" 1 "down" by a very small value 
## (0.0001). 

## Additionally, we allowed for an argument that specifies the kind of spatial
## parameter we want to work with. For comparison purposes, we will be cross
## validating the MAE and RMSE of Krig models with parameters Q1, Q2 (median),
## and Q3. See below for instructions and guidelines to specifications.


CompareSP1 <- function(TRAINX, TESTX, TRAINCOORDS, TESTCOORDS, dd){
  
  for(i in 1:length(TRAINX)){
    
    if(TRAINX[i] == 0){TRAINX[i] = 0 + 0.0001}	
    if(TRAINX[i] == 1){TRAINX[i] = 1 - 0.0001}
  }
  
  logittrans 		<- log(TRAINX/(1-TRAINX))
  
  ## Change the number within the bracket to select different
  ## distances. Guidelines: Q1 [2]; Median [3];  Q3 [5].
  
  dists			<- as.numeric(summary(dist(TRAINCOORDS))[dd])
  
  krig.fit		<- Krig(TRAINCOORDS, logittrans, theta = dists)
  pred.krig		<- predict(krig.fit, TESTCOORDS)
  
  backtrans		<- exp(pred.krig)/(1+exp(pred.krig))
  
  MAE			<- sum(abs(backtrans-TESTX))/length(TESTX)
  VMSE			<- sum((backtrans-TESTX)^2)/length(TESTX)
  RMSE			<- sqrt(VMSE)
  
  round(data.frame(MAE, RMSE),4)
}


## We summaried household size as a mean. Therefore, there is no need to logit
## and back transform the covariate household. Hence, we removed this feature
## in the function CompareSP2.  
CompareSP2 <- function(TRAINX, TESTX, TRAINCOORDS, TESTCOORDS, dd){
  
  dists			<- as.numeric(summary(dist(TRAINCOORDS))[dd])
  krig.fit		<- Krig(TRAINCOORDS, TRAINX, theta = dists)
  pred.krig		<- predict(krig.fit, TESTCOORDS)
  
  MAE			<- sum(abs(pred.krig-TESTX))/length(TESTX)
  VMSE			<- sum((pred.krig-TESTX)^2)/length(TESTX)
  RMSE			<- sqrt(VMSE)
  
  round(data.frame(MAE, RMSE),4)
}

WealthQ1 <- CompareSP1(wealth.cc, wealth.nc, coords.cc, coords.nc, 2)
WealthQ2 <- CompareSP1(wealth.cc, wealth.nc, coords.cc, coords.nc, 3)
WealthQ3 <- CompareSP1(wealth.cc, wealth.nc, coords.cc, coords.nc, 5)

rbind(WealthQ1, WealthQ2, WealthQ3) 

EducationQ1 <- CompareSP1(educ.cc, educ.nc, coords.cc, coords.nc, 2)
EducationQ2 <- CompareSP1(educ.cc, educ.nc, coords.cc, coords.nc, 3)
EducationQ3 <- CompareSP1(educ.cc, educ.nc, coords.cc, coords.nc, 5)

rbind(EducationQ1, EducationQ2, EducationQ3) 


del1 <- which(is.na(health_card_doc.cc))
if(length(del1)>0){health_card_doc.cc <- health_card_doc.cc[-del1];  hcoords.cc <- coords.cc[-del1,]}
if(length(del1)==0){health_card_doc.cc <- health_card_doc.cc;  hcoords.cc <- coords.cc}
del2 <- which(is.na(health_card_doc.nc))
if(length(del2)>0){health_card_doc.nc <- health_card_doc.nc[-del2];  hcoords.nc <- coords.nc[-del2,]}
if(length(del2)==0) {health_card_doc.nc <- health_card_doc.nc;  hcoords.nc <- coords.nc}

health_card_docQ1 <- CompareSP1(health_card_doc.cc, health_card_doc.nc, hcoords.cc, hcoords.nc, 2)
health_card_docQ2 <- CompareSP1(health_card_doc.cc, health_card_doc.nc, hcoords.cc, hcoords.nc, 3)
health_card_docQ3 <- CompareSP1(health_card_doc.cc, health_card_doc.nc, hcoords.cc, hcoords.nc, 5)

rbind(health_card_docQ1, health_card_docQ2, health_card_docQ3) 


sbaQ1 <- CompareSP1(sba.cc, sba.nc, coords.cc, coords.nc, 2)
sbaQ2 <- CompareSP1(sba.cc, sba.nc, coords.cc, coords.nc, 3)
sbaQ3 <- CompareSP1(sba.cc, sba.nc, coords.cc, coords.nc, 5)

rbind(sbaQ1, sbaQ2, sbaQ3)

hhsizeQ1 <- CompareSP2(hhsize.cc, hhsize.nc, coords.cc, coords.nc, 2)
hhsizeQ2 <- CompareSP2(hhsize.cc, hhsize.nc, coords.cc, coords.nc, 3)
hhsizeQ3 <- CompareSP2(hhsize.cc, hhsize.nc, coords.cc, coords.nc, 5)

rbind(hhsizeQ1, hhsizeQ2, hhsizeQ3)

ph_intQ1 <- CompareSP2(phone_internet.cc, phone_internet.nc, coords.cc, coords.nc, 2)
ph_intQ2 <- CompareSP2(phone_internet.cc, phone_internet.nc, coords.cc, coords.nc, 3)
ph_intQ3 <- CompareSP2(phone_internet.cc, phone_internet.nc, coords.cc, coords.nc, 5)

rbind(ph_intQ1, ph_intQ2, ph_intQ3) 

relQ1 <- CompareSP2(religion.cc, religion.nc, coords.cc, coords.nc, 2)
relQ2 <- CompareSP2(religion.cc, religion.nc, coords.cc, coords.nc, 3)
relQ3 <- CompareSP2(religion.cc, religion.nc, coords.cc, coords.nc, 5)

rbind(relQ1, relQ2, relQ3) 


mediaQ1 <- CompareSP2(media.cc, media.nc, coords.cc, coords.nc, 2)
mediaQ2 <- CompareSP2(media.cc, media.nc, coords.cc, coords.nc, 3)
mediaQ3 <- CompareSP2(media.cc, media.nc, coords.cc, coords.nc, 5)

rbind(mediaQ1, mediaQ2, mediaQ3) 

####All variables tested yielded very similar MAE and RMSE, so decied to use Q1/Q2.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## The second exercise we will do is to create raster files for the interpolated
## area via. the Krig function. Majority of the function is the same as CompareSP1
## and CompareSP2 from the first exercise. 

## We included two modules within the functions KrigingTIF1 and KrigingTIF2 for
## the second exercise. The first is the output (plotting) of the raster file
## and saving it as a .tif file. The second feature is a workaround for the 
## problem of prediction.


KrigingTIF1 <- function(X, COORDS, TIFNAME, dd){
  
  aa	<- raster("Data/gin_mastergrid_1km_reclass.tif")
  bb	<- coordinates(aa)
  
  ID		<- is.na(getValues(aa))
  miss    	<- which(ID == T)
  nonmiss 	<- which(ID == F)
  
  for(i in 1:length(X)){
    
    if(X[i] == 0){X[i] = 0 + 0.0001}	
    if(X[i] == 1){X[i] = 1 - 0.0001}
  }
  
  logittrans 	<- log(X/(1-X))
  
  dists		<- as.numeric(summary(dist(COORDS))[dd])
  krig.fit	<- Krig(COORDS, logittrans, theta = dists)
  
  cutsize 	<- 10000
  rows	 	<- dim(bb)[[1]]
  cuts		<- split(bb[,1:2], rep(1:ceiling(rows/cutsize), 
                               each = cutsize, length.out = rows))
  
  pred.out		<- NA
  pred.krig		<- list()
  backtrans		<- list()
  
  for(ff in 1:length(cuts)){
    
    pred.krig[[ff]] <- as.vector(predict(krig.fit,matrix(cuts[[ff]], ncol = 2)))
    backtrans[[ff]] <- as.vector(exp(pred.krig[[ff]])/(1+exp(pred.krig[[ff]])))
  }
  
  pred.out		<- unlist(backtrans)
  pred.out[miss] 	<- NA
  
  values(aa)		<- pred.out
  
  plot(aa, zlim = c(0,1))
  writeRaster(aa, TIFNAME, overwrite = TRUE)
}


## Likewise, we do not need to logit and back-transform household. So we created
## another function for this covariate without these features.

KrigingTIF2 <- function(X, COORDS, TIFNAME, dd){
                                                              
  aa	<- raster("Data/gin_mastergrid_1km_reclass.tif") 
  bb	<- coordinates(aa)                                    
  
  ID		<- is.na(getValues(aa))
  miss    	<- which(ID == T)
  nonmiss 	<- which(ID == F)
  
  dists		<- as.numeric(summary(dist(COORDS))[dd])
  krig.fit	<- Krig(COORDS, X, theta = dists)
  
  cutsize 	<- 10000
  rows	 	<- dim(bb)[[1]]
  cuts		<- split(bb[,1:2], rep(1:ceiling(rows/cutsize), 
                               each = cutsize, length.out = rows))
  
  pred.out		<- NA
  pred.krig		<- list()
  backtrans		<- list()
  
  for(ff in 1:length(cuts)){
    
    pred.krig[[ff]] <- as.vector(predict(krig.fit,
                                         matrix(cuts[[ff]], ncol = 2)))
  }
  
  pred.out		<- unlist(pred.krig)
  pred.out[miss] 	<- NA
  
  values(aa)		<- pred.out
  
  plot(aa)
  writeRaster(aa, TIFNAME, overwrite = TRUE)
}

# Wealth
#--------------
KrigingTIF1(clustdf$wealth.prop, 	clustdf[,17:18], "wealth_krig.tif", 	5)  #NOTE THAT 2 is Q1 of distances

# Education
#--------------
KrigingTIF1(clustdf$educ.prop, clustdf[,17:18], "educ_krig.tif", 2)

# Health Card
#--------------
del1 <- which(is.na(clustdf$health_card_doc.prop))
if(length(del1)>0){
  health_card_doc.proph <- clustdf$health_card_doc.prop[-del1]
  clustdfh <- clustdf[-del1,]
}
KrigingTIF1(clustdfh$health_card_doc.prop, 	clustdfh[,17:18], "health_card_doc_krig.tif", 	2)

# Sba
#--------------
KrigingTIF1(clustdf$sba.prop, 	clustdf[,17:18], "sba_krig.tif", 	3)

# Media
#--------------
KrigingTIF1(clustdf$media.prop, 	clustdf[,17:18], "media_krig.tif", 	2)


# Phone Internet
#--------------
KrigingTIF1(clustdf$phone_internet.prop, 	clustdf[,17:18], "phone_internet_krig.tif", 	5)

# Kriging
#--------------
del1 <- which(is.na(clustdf$religion.prop))
if(length(del1)>0){
  religion.proph <- clustdf$religion.prop[-del1]
  clustdfh <- clustdf[-del1,]
}

KrigingTIF1(clustdfh$religion.prop, 	clustdfh[,17:18], "religion_krig.tif", 	2)

#Write Clsuter-level data to file
write_csv(clustdf, "GIN_DHS_covariates_cluster.csv")

