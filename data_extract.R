library(foreign)
library(haven)


# Import Data
# ------------------------------------------------------
data1 <- read.dta('GNKR71DT/GNKR71FL.DTA',convert.factors=FALSE) 
# ------------------------------------------------------


#List and select variable names
#List of variables to extract

#Cluster and Interview
#v001 - Cluster_number; v006 - month_of_interview; v007 - year_of_interview; 
#v016 - day of interview	

#Child variables
#hw1 - child's age in months; b1 - month of birth; b2 - year of birth; b3 - date of birth (cmc); b5 - alive

#Measles
#h9 - received measles 1; h9d h9m h9y -  measles 1 day, month and year		
#h9 - no - 0, 1 - vaccination date on card, 2 -   reported by mother, 3 - vaccination marked on card, 8 -  don't know

#DTP
#h3 - received dpt 1; h3d h3m h3y - dpt 1 day, month, year	
#h5 - received dpt 2; h5d h5m h5y - dpt 2 day, month,	year
#h7 - received dpt 3; h7d h7m h7y - dpt 3 day, month, year 

# s1508d1 s1508d2 s1508d3 - received dpt 1	received dpt 2	received dpt 3
# sd1508d1 sd1508d2 sd1508d - dpt 1 day	dpt 2 day	dpt 3 day
# sm1508d1 sm1508d2 sm1508d3 - dpt 1 month dpt 2 month dpt 3 month	
# sy1508d1 sy1508d2 sy1508d3 - dpt 1 year	dpt 2 year	dpt 3 year

#Subset to children who were alive
data <- subset(data1, b5=1)

vars1 = c("v001", "v006", "v007", "v016", "hw1", "b1", "b2", "b3", "h9", "h9d", "h9m", "h9y")

vars2 = c("v001", "v006", "v007", "v016", "hw1", "b1", "b2", "b3", "h3", "h3d", "h3m", "h3y", "h5",
          "h5d", "h5m", "h5y", "h7", "h7d", "h7m", "h7y")

#Measles
data.m.ext = data[vars1]
head(data.m.ext)

#DTP
data.D.ext = data[vars2]
head(data.D.ext)


#Exclusion of rows with missing vaccination data
#Note that h3, h5 and h7 have similar missing patterns
data.m.ext = data.m.ext[!is.na(data.m.ext$h9),]
data.D.ext = data.D.ext[!is.na(data.D.ext$h3),] 
data.D.ext = data.D.ext[!is.na(data.D.ext$h5),]
data.D.ext = data.D.ext[!is.na(data.D.ext$h7),]


#Calculation of child's age in months
#Measles
xx = data.m.ext$v006 - data.m.ext$b1     #month.int - month.birth
yy = data.m.ext$v007 - data.m.ext$b2     #year.int - year.birth
age.m = 12*yy + xx
data.m.ext$age.m = age.m

#DTP
xx1 = data.D.ext$v006 - data.D.ext$b1     #month.int - month.birth
yy1 = data.D.ext$v007 - data.D.ext$b2     #year.int - year.birth
age.D = 12*yy1 + xx1
data.D.ext$age.D = age.D

#Further Processing and writing of files - Measles
vars.1 = c("v001", "age.m", "h9", "h9d", "h9m", "h9y")
data.m = data.m.ext[vars.1] 
vacc = data.m$h9
l1 = which(vacc==0|vacc==8) #no=0; don't know=8
l2 = which(vacc==1|vacc==2|vacc==3) #vax date on card; reported by mother; vax marked on card
vacc.num = 1:length(vacc)
vacc.num[l1]=0; vacc.num[l2]=1
data.m$received_measles = vacc.num


#Rename column headings and save files for map and SIA analysis
colnames(data.m) = c("Cluster_no", "childs_age_in_months", "received_measles_ch",
                     "measles_day", "measles_month", "measles_year", "received_measles")

data.m.file1 = data.m[c("Cluster_no", "childs_age_in_months","received_measles")]

write.csv(data.m.file1, "Vax_GIN_measles_proc.csv")


#-------------------------------------------------------------------------------
# ---------------------- DTP Data ----------------------------------------------
#-------------------------------------------------------------------------------

#Further Processing and writing of files - DTP
vars.2 = c("v001", "age.D", "h3", "h5", "h7")
data.D = data.D.ext[vars.2]

l4 = which(data.D$h3==0|data.D$h3==8); l5 = which(data.D$h3==1|data.D$h3==2|data.D$h3==3) 
vacc.num.D1 = 1:nrow(data.D); vacc.num.D1[l4]=0; vacc.num.D1[l5]=1 

l4 = which(data.D$h5==0|data.D$h5==8); l5 = which(data.D$h5==1|data.D$h5==2|data.D$h5==3) 
vacc.num.D2 = 1:nrow(data.D); vacc.num.D2[l4]=0; vacc.num.D2[l5]=1 

l4 = which(data.D$h7==0|data.D$h7==8); l5 = which(data.D$h7==1|data.D$h7==2|data.D$h7==3) 
vacc.num.D3 = 1:nrow(data.D); vacc.num.D3[l4]=0; vacc.num.D3[l5]=1 


#Use DHS logic to process vacc.num.D1,2,3 further - see https://dhsprogram.com/data/Guide-to-DHS-Statistics/Vaccination.htm
received_dtp1=received_dtp2=received_dtp3=0
for (i in 1:nrow(data.D)){
  ss <- sum(vacc.num.D1[i]+vacc.num.D2[i]+vacc.num.D3[i])
  received_dtp1[i] <- as.integer(ss >= 1)
  received_dtp2[i] <- as.integer(ss >= 2)
  received_dtp3[i] <- as.integer(ss >= 3)
  if (ss == 0) {
    received_dtp1[i] <- 0
    received_dtp2[i] <- 0
    received_dtp3[i] <- 0
  }
}




data.D$received_dtp1 <- received_dtp1
data.D$received_dtp2 <- received_dtp2
data.D$received_dtp3 <- received_dtp3


colnames(data.D) = c("Cluster_no", "childs_age_in_months", "received_dtp1_ch", 
                     "received_dtp2_ch", "received_dtp3_ch", "received_dtp1", "received_dtp2", "received_dtp3")

write.csv(data.D, "Vax_GIN_dtp_proc.csv")            

