# author : Prashant Istalkar 
# contact : istalkarps@gmail.com

# code to implement MMO algorithm
# give the input in csv file with each column as one vaiable and row as catchement. 
# First two col as lat and long 
# change the threshold accodringly 



rm(list=ls())  # removing all the variables from workspace



mydata='D:MMO'      # giving folder link
setwd(mydata)    # setting working directory


mydata = as.matrix(read.csv2(file='MMO code sample data.csv',sep = ',',header = TRUE)) # reading it as matrix
mydata = apply(mydata,2, as.numeric) # converting it to numeric
nn=494 # no. of basins 

threshold= 0.875   # change the threshold based on need 



# excluding lat and long and selecting only 3 to 6 columns 
Data=mydata[,3:8]

# creating empty matrix 
nm=nn-1 
Nrow=sum(1:nm)
fina_matrix<-matrix(,nrow =Nrow, ncol = 6 )

# getting weigts for all variables using canbera method 
for(mm in 1:6){
  m=Data[,mm]
  r<-matrix(,nrow = nn,ncol = nn)
  nnn=nn-1
  for(i in 1:nnn) {
    k = i + 1
    for (j in k:nn) {
      sx = m[i]
      sy = m[j]
      r[j, i] = 1 - abs((sx - sy) / (sx + sy))
    }
    rm(j, k,sx,sy)
  }
  r[is.nan(r)] = 0
  rr <- as.vector(r)
  rr <- na.omit(rr)
  fina_matrix[,mm]=rr 
  rm(rr,r,i,m)
}

rm(Data,mm)

# taking mean of all variables 
Mean_row<-rowMeans(fina_matrix)
rm(fina_matrix)
# $$$$$$$$$$$$$$$$$$$$$    putting threshold $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#

Mean_row[Mean_row < threshold] <- 0 

###################### Now use of igraph packeage ########################################
library(igraph)
Mean_row=t(Mean_row)
# creating  graph for 500 points #
q<-graph.full(n=nn,directed=FALSE)

# modularuty formula 
community=multilevel.community(q,Mean_row)
grps=membership(community)

########################### Combing grpups with lat and long and saving to csv#########################################
lat_long=mydata[,1:2]   # sepering lat and long

sort_mat<-matrix(,nrow = nn,ncol=9)    # creating empty matix 
sort_mat[,1]=grps                      # keeping groups in above matrix
sort_mat[,2:9]=mydata #lat_long                # keeping lat long   

z<-sort_mat[order(sort_mat[,1]),]       # sorting according to groups 

# to save in csv 
maximum<-max(z[,1])
Rang<-1:maximum

LAT<-matrix(,nrow=nn,ncol = nn)

for (i in 1:maximum) {
  grp_index <- which(z[, 1] == Rang[i], arr.ind = TRUE)
  
  Group_1 <- z[grp_index, ]
  name <- sprintf("threshold %g group %d.csv",threshold, i)
  #col_names=c('group','lat','long')
  col_names=c('group','lat','long','PPT',	'Tavg'	,'RH',	'Stream',	'Elev',	'slope')
  if (class(Group_1) == "numeric") {
    Group_1 <- t(Group_1)
  }
  write.table(Group_1, name,sep = ",", row.names = FALSE,col.names=col_names)
  
  lat<-z[grp_index,2]
  lat<-as.matrix(lat)
  LAT[1:dim(lat)[1],i]<-lat 
  rm(grp_index, Group_1, name,lat)
  
  
}
name=sprintf('MMO threshold %g group %d.csv',threshold,maximum)
write.table(LAT, name,na = "",row.names = FALSE,col.names = FALSE,append = TRUE,sep = ",")

#################### Noww saving each group lat vaues as column to find commin percentage with kmeans ######

