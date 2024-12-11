

#-------------Read in data---------------------#
data1 <- read.csv("Vax_GIN_dtp_proc.csv", header = T)

length(unique(data1$Cluster_no))
max(unique(data1$Cluster_no))

#Find missing clusters in data1
clust <- 1:max(unique(data1$Cluster_no))    #722 clusters
miss <- clust[!clust %in% unique(data1$Cluster_no)] #missing clusters in stata file
miss

clust=clust[-miss]


#----------------Process data to extract counts at the cluster level------------------------#
#Summarize data1 - total and by dpt1, dpt2, dpt3
n.clust = length(unique(data1$Cluster_no))
agebrks = c(0,8,11,23,35)


#Unique cluster nos
Clust <- unique(data1$Cluster_no)

clust.tab.1DTP1 = clust.tab.2DTP1 = clust.tab.1DTP2 = clust.tab.2DTP2 = clust.tab.1DTP3 = clust.tab.2DTP3 = matrix(0, nrow = n.clust, ncol = 4)
for (i in 1:n.clust)
{
  print(i)
  #For DTP1
  subdat.1 = subset(data1, Cluster_no==Clust[i], select=c(childs_age_in_months, received_dtp1))
  for (j in 1:(length(agebrks)-1)){
    if (j>1) subdat.2 = subset(subdat.1, childs_age_in_months>agebrks[j] & childs_age_in_months<=agebrks[j+1])
    if (j==1) subdat.2 = subset(subdat.1, childs_age_in_months>=agebrks[j] & childs_age_in_months<=agebrks[j+1])
    print(j)
    clust.tab.1DTP1[i,j] = nrow(subdat.2)
    clust.tab.2DTP1[i,j] = length(subdat.2[subdat.2[,2]==1,2])
  }
  
  #For DTP2
  subdat.1 = subset(data1, Cluster_no==Clust[i], select=c(childs_age_in_months, received_dtp2))
  for (j in 1:(length(agebrks)-1)){
    if (j>1) subdat.2 = subset(subdat.1, childs_age_in_months>agebrks[j] & childs_age_in_months<=agebrks[j+1])
    if (j==1) subdat.2 = subset(subdat.1, childs_age_in_months>=agebrks[j] & childs_age_in_months<=agebrks[j+1])
    print(j)
    clust.tab.1DTP2[i,j] = nrow(subdat.2)
    clust.tab.2DTP2[i,j] = length(subdat.2[subdat.2[,2]==1,2])
  }
  
  #For DTP3
  subdat.1 = subset(data1, Cluster_no==Clust[i], select=c(childs_age_in_months, received_dtp3))
  for (j in 1:(length(agebrks)-1)){
    if (j>1) subdat.2 = subset(subdat.1, childs_age_in_months>agebrks[j] & childs_age_in_months<=agebrks[j+1])
    if (j==1) subdat.2 = subset(subdat.1, childs_age_in_months>=agebrks[j] & childs_age_in_months<=agebrks[j+1])
    print(j)
    clust.tab.1DTP3[i,j] = nrow(subdat.2)
    clust.tab.2DTP3[i,j] = length(subdat.2[subdat.2[,2]==1,2])
  }
}


#Add column headings
#Totals ind. age groups
colnames(clust.tab.1DTP1) = colnames(clust.tab.1DTP2) = colnames(clust.tab.1DTP3) = c("0-8", "9-11", "12-23", "24-35")

#Numbers vaccinated ind. age groups
colnames(clust.tab.2DTP1) = colnames(clust.tab.2DTP2) = colnames(clust.tab.2DTP3) = c("0-8", "9-11", "12-23", "24-35")
head(clust.tab.1DTP1); head(clust.tab.2DTP1)


#Total counts for 0-35
clust.tab.3DTP1 = clust.tab.3DTP2 = clust.tab.3DTP3 = matrix(0, nrow = n.clust, ncol = 3)
for (i in 1:n.clust) #NOTE 35
{
  sub.dat3DTP1 = subset(data1, Cluster_no==Clust[i] & childs_age_in_months <= 35, select = c(Cluster_no, childs_age_in_months,received_dtp1))
  sub.dat3DTP2 = subset(data1, Cluster_no==Clust[i] & childs_age_in_months <= 35, select = c(Cluster_no, childs_age_in_months,received_dtp2))
  sub.dat3DTP3 = subset(data1, Cluster_no==Clust[i] & childs_age_in_months <= 35, select = c(Cluster_no, childs_age_in_months,received_dtp3))
  
  Totchild.DTP1 = nrow(sub.dat3DTP1); Numvacc.DTP1 = length(sub.dat3DTP1$received_dtp1[sub.dat3DTP1$received_dtp1==1])
  Totchild.DTP2 = nrow(sub.dat3DTP2); Numvacc.DTP2 = length(sub.dat3DTP2$received_dtp2[sub.dat3DTP2$received_dtp2==1])
  Totchild.DTP3 = nrow(sub.dat3DTP3); Numvacc.DTP3 = length(sub.dat3DTP3$received_dtp3[sub.dat3DTP3$received_dtp3==1])
  
  clust.tab.3DTP1[i,]= c(as.numeric(sub.dat3DTP1[1,1]), Totchild.DTP1, Numvacc.DTP1) 
  clust.tab.3DTP2[i,]= c(as.numeric(sub.dat3DTP2[1,1]), Totchild.DTP2, Numvacc.DTP2) 
  clust.tab.3DTP3[i,]= c(as.numeric(sub.dat3DTP3[1,1]), Totchild.DTP3, Numvacc.DTP3) 
}

colnames(clust.tab.3DTP1)=c("cluster", "Totchild", "Numvacc")
colnames(clust.tab.3DTP2)=c("cluster", "Totchild", "Numvacc")
colnames(clust.tab.3DTP3)=c("cluster", "Totchild", "Numvacc")

#----------------------------------------------------------------------------------------------------#
#DTP1

all.var=data.frame(1:length(clust.tab.1DTP1[,1]))
all.var$Age0_8_Tot = clust.tab.1DTP1[,1]; all.var$Age0_8_Vax = clust.tab.2DTP1[,1]
all.var$Age9_11_Tot = clust.tab.1DTP1[,2]; all.var$Age9_11_Vax = clust.tab.2DTP1[,2]
all.var$Age12_23_Tot = clust.tab.1DTP1[,3]; all.var$Age12_23_Vax = clust.tab.2DTP1[,3]
all.var$Age24_35_Tot = clust.tab.1DTP1[,4]; all.var$Age24_35_Vax = clust.tab.2DTP1[,4]
all.var$Totchild = clust.tab.3DTP1[,2]; all.var$Numvacc = clust.tab.3DTP1[,3]
all.var$Age9_35_Tot = all.var$Totchild - all.var$Age0_8_Tot
all.var$Age9_35_Vax = all.var$Numvacc - all.var$Age0_8_Vax
all.var$DHSCLUST = Clust
all.var = all.var[,-1]

#---Export processed vacc file------#
write.csv(all.var, "GIN_Vax_DTP1.csv")


#DTP2
all.var=data.frame(1:length(clust.tab.1DTP2[,1]))
all.var$Age0_8_Tot = clust.tab.1DTP2[,1]; all.var$Age0_8_Vax = clust.tab.2DTP2[,1]
all.var$Age9_11_Tot = clust.tab.1DTP2[,2]; all.var$Age9_11_Vax = clust.tab.2DTP2[,2]
all.var$Age12_23_Tot = clust.tab.1DTP2[,3]; all.var$Age12_23_Vax = clust.tab.2DTP2[,3]
all.var$Age24_35_Tot = clust.tab.1DTP2[,4]; all.var$Age24_35_Vax = clust.tab.2DTP2[,4]
all.var$Totchild = clust.tab.3DTP2[,2]; all.var$Numvacc = clust.tab.3DTP2[,3]
all.var$Age9_35_Tot = all.var$Totchild - all.var$Age0_8_Tot
all.var$Age9_35_Vax = all.var$Numvacc - all.var$Age0_8_Vax
all.var$DHSCLUST = Clust
all.var = all.var[,-1]

#---Export processed vacc file------#
write.csv(all.var, "GIN_Vax_DTP2.csv")


#DTP3
all.var=data.frame(1:length(clust.tab.1DTP3[,1]))
all.var$Age0_8_Tot = clust.tab.1DTP3[,1]; all.var$Age0_8_Vax = clust.tab.2DTP3[,1]
all.var$Age9_11_Tot = clust.tab.1DTP3[,2]; all.var$Age9_11_Vax = clust.tab.2DTP3[,2]
all.var$Age12_23_Tot = clust.tab.1DTP3[,3]; all.var$Age12_23_Vax = clust.tab.2DTP3[,3]
all.var$Age24_35_Tot = clust.tab.1DTP3[,4]; all.var$Age24_35_Vax = clust.tab.2DTP3[,4]
all.var$Totchild = clust.tab.3DTP3[,2]; all.var$Numvacc = clust.tab.3DTP3[,3]
all.var$Age9_35_Tot = all.var$Totchild - all.var$Age0_8_Tot
all.var$Age9_35_Vax = all.var$Numvacc - all.var$Age0_8_Vax
all.var$DHSCLUST = Clust
all.var = all.var[,-1]

#---Export processed vacc file------#
write.csv(all.var, "GIN_Vax_DTP3.csv")


