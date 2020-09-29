cluster_data_cord<-cord_fattyacid_cluster_res[[1]]
table_cord_cluster<-data.frame(Variable=character(),median_cluster1=numeric(),
                               stdev_cluster1=numeric(),median_cluster2=numeric(),
                               stdev_cluster2=numeric(),stringsAsFactors = F)
for (i in 8:26){
  
  median_fattyacid<-tapply(cluster_data_cord[,i],cluster_data_cord[,6],median)
  stdev_fattyacid<-tapply(cluster_data_cord[,i],cluster_data_cord[,6],sd)
  table_cord_cluster[i,]<-c(colnames(cluster_data_cord)[i],
                            round(median_fattyacid[[1]],3),
                            round(stdev_fattyacid[[1]],3),
                            round(median_fattyacid[[2]],3),
                            round(stdev_fattyacid[[2]],3))
}
table_cord_cluster<-na.omit(table_cord_cluster)
write.csv(table_cord_cluster,"Table_cord_cluster_mediansd.csv")
####### table mother ###############

cluster_data_mother<-mother_fattyacid_cluster_res[[1]]
table_mother_cluster<-data.frame(Variable=character(),median_cluster1=numeric(),
                               stdev_cluster1=numeric(),median_cluster2=numeric(),
                               stdev_cluster2=numeric(),stringsAsFactors = F)
for (i in 27:45){
  
  median_fattyacid<-tapply(cluster_data_mother[,i],cluster_data_mother[,6],median)
  stdev_fattyacid<-tapply(cluster_data_mother[,i],cluster_data_mother[,6],sd)
  table_mother_cluster[i,]<-c(colnames(cluster_data_mother)[i],
                            round(median_fattyacid[[1]],3),
                            round(stdev_fattyacid[[1]],3),
                            round(median_fattyacid[[2]],3),
                            round(stdev_fattyacid[[2]],3))
}
table_mother_cluster<-na.omit(table_mother_cluster)
write.csv(table_mother_cluster,"Table_mother_cluster_mediansd.csv")
