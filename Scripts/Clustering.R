require(cluster)
data_cluster<-data_fatty_acid[,2:20]
#data_cluster<-data_fatty_acid[,21:39]
##########################
function_cluster<-function(data_cluster,title){
  my_data <- na.omit(data_cluster)
  #my_data<-dat_iaeaFA[,-1]
  my_data <- scale(my_data)
  #####################################################
  dist_mydata<-daisy(my_data,metric="euclidean",stand=F)
  
  silinfo<-c()
  for(i in 2:10){
    
    clust_mydata<-pam(dist_mydata,i,diss = T)
    silinfo[i]<-clust_mydata$silinfo$avg.width
  }
  ##########################################
  png("optimal_number_of_clusters_cord.png",units = 'in',height = 6,width = 6,res=300)
  opt_cluster_plot<-plot(silinfo,type="h",xlab="No.of Clusters ",ylab="Average Silhouette width",lwd=2) ## optimal number of clusters
  dev.off()
  
  ###############################################
  clust_mydata<-pam(dist_mydata,k=which.max(silinfo),diss=T)
  data<-merge(data.frame(clust_mydata$clustering),data_fatty_acid,by=0)
  rownames(data)<-data[,1]
  colnames(data)[2]<-"Cluster_Groups"
 # saveRDS(data,"data_clusters_full.RDS")
  ################### cluster ggplot########################
  my_data.pca <- prcomp(my_data,
                        center = TRUE,
                        scale. = TRUE) 
  data<-merge(my_data.pca$x[,1:3],data,by=0)
  rownames(data)<-data[,1]
  data$Cluster_Groups<-factor(data$Cluster_Groups)
####################################### 
  require(ggplot2)
  require(ggforce)
  #png("Clusters_cord_pairedsamples.png",width=5,height = 5,units = 'in',res=300)
  png("Clusters_cord_fattyacid.png",width=5,height = 5,units = 'in',res=300)
  cluster_plot<-ggplot(data,aes(PC1,PC2))+geom_point(aes(colour=Cluster_Groups),shape=19,size=3)+
    ggtitle(title)+
    #ylim(c(-6,6))+
    geom_mark_ellipse(aes(fill=Cluster_Groups))+
  theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")
  dev.off()
#########################################  
  return(list(data,cluster_plot,opt_cluster_plot))
}
cord_fattyacid_cluster_res<-function_cluster(data_fatty_acid[,2:20],"Cord Fatty acid cluster")
mother_fattyacid_cluster_res<-function_cluster(data_fatty_acid[,21:39],"Mother Fatty acid cluster")
  
  
  
  
  
  ############# Randomization of clusters ###########
  my_data<-na.omit(datmfa_mot_cord[,1:19])
  
  fn_permut<-function(x){
    
    sample(x)
  }
  random_data<-apply(my_data,2,fn_permut)
  
  random_data <- scale(random_data)
  dist_randomdata<-daisy(random_data,metric="manhattan",stand=F)
  silinfo_random<-c()
  for(i in 2:10){
    
    clust_randomdata<-pam(dist_randomdata,i,diss = T)
    silinfo_random[i]<-clust_randomdata$silinfo$avg.width
  }
  
  plot(silinfo_random,type="h")
  clust_randomdata<-pam(dist_randomdata,2,diss=T)
  png("Mother_fattyacid_clusters_random.png",units = 'in',height = 6,width=6,res=300)
  clusplot(clust_randomdata,col.p="black",span=T,color=T,main="Mother Fatty Acid Random Clusters")
  dev.off()
  