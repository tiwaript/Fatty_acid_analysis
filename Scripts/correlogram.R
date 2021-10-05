library(corrplot)

### script for correlogram ###
dat.cor<-data_fatty_acid[,1:39]

colnames(dat.cor)<-gsub("c_","M_",colnames(dat.cor))
colnames(dat.cor)<-gsub("cord_","C_",colnames(dat.cor))
colnames(dat.cor)<-gsub("_28wk","",colnames(dat.cor))

### arranging data as per fatty acid categories 

dat.cor<-dat.cor[,c("key","C_MYR","C_PAL","C_STE","C_SFA", "C_OLE","C_PALO","C_MYRO", "C_MUFA" ,"C_LA","C_GLA","C_DGLA",  
                    "C_AA" ,"C_Omega_6_PUFA","C_ALA", "C_EPA" , "C_DPA", "C_DHA","C_Omega_3_PUFA","C_Omega6_Omega_3_FA_ratio",
                    "M_MYR","M_PAL","M_STE","M_SFA", "M_OLE","M_PALO","M_MYRO", "M_MUFA" ,"M_LA","M_GLA","M_DGLA", 
                    "M_AA" ,"M_Omega_6_PUFA","M_ALA", "M_EPA" , "M_DPA", "M_DHA","M_Omega_3_PUFA","M_Omega6_Omega_3_FA_ratio")]

colnames(dat.cor)[14]<-"C_Omega6PUFA"
colnames(dat.cor)[19]<-"C_Omega3PUFA"
colnames(dat.cor)[20]<-"C_Omega6/Omega3"

colnames(dat.cor)[33]<-"M_Omega6PUFA"
colnames(dat.cor)[38]<-"M_Omega3PUFA"
colnames(dat.cor)[39]<-"M_Omega6/Omega3"
### correlogram 
#png("correlogram_motherCordFattyacid_.png",height = 6,width=8,units = 'in',res=300)
png("Sigcorrelogram_motherCordFattyacid_.png",height = 6,width=8,units = 'in',res=300)
cor_M<-cor(dat.cor[,-1],use = "complete.obs")
res1 <- cor.mtest(dat.cor[,-1], conf.level = .95)
corrplot(cor_M, method="circle",tl.col="red", tl.srt=90,tl.cex =0.6,p.mat = res1$p, sig.level = 0.05, insig = "blank")
dev.off()
### Anthro data with fatty acid 
dat.cor<-merge(dat.cor,data_fatty_acid[,c("key","g2_nht","g2_ncrl","g2_nhc","g2_nch",
                                          "g2_nabd","g2_nma","g2_ntr","g2_nss",
                                          "g2_csumofskinfold","g1_bw")],by="key")

### Baby Anthro and cord fatty acid
#png("correlogram_Anthro_CordFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
png("Sigcorrelogram_Anthro_CordFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
cor_M<-cor(dat.cor[,c(2:20,40:49)],use = "complete.obs")
res1 <- cor.mtest(dat.cor[,c(2:20,40:49)], conf.level = .95)
corrplot(cor_M, method="circle",tl.col="black", tl.srt=90,tl.cex =0.6,p.mat = res1$p, sig.level = 0.05, insig = "blank")
dev.off()

### Baby Anthro and Mother fatty acid 
#png("correlogram_Anthro_MotherFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
png("Sigcorrelogram_Anthro_MotherFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
cor_M<-cor(dat.cor[,-c(1:20)],use = "complete.obs")
res1 <- cor.mtest(dat.cor[,-c(1:20)], conf.level = .95)
corrplot(cor_M, method="circle",tl.col="black", tl.srt=90,tl.cex =0.6,p.mat = res1$p, sig.level = 0.05, insig = "blank")
dev.off()

### Mother MMN and CORD MMN with Fatty acid and Anthro data 
dat.cor<-merge(dat.cor,data_fatty_acid[,c("key","b12_28wk","folate_28wk","cyst_28wk","glut_28wk",
                                 "cb6plp_28wk","cb6pyx_28wk","cb2_28wk", "hcy_28wk", 
                                  "fol_eclia_28wk","vitb12_eclia_28wk",
                                  "g2_cb12","g2_cfolate","g2_chcy","g2_ccyst", "g2_cb2",
                                  "g2_cb6plp","g2_cb6pyx")],by="key")

### mother MMN and cord fatty acid 
png("Sigcorrelogram_motherMMN_CordFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
#png("correlogram_motherMMN_CordFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
cor_M<-cor(dat.cor[,c(2:20,50:57)],use = "complete.obs")
res1 <- cor.mtest(dat.cor[,c(2:20,50:57)], conf.level = .95)
corrplot(cor_M, method="circle",tl.col="black", tl.srt=90,tl.cex =0.8,p.mat = res1$p, sig.level = 0.05, insig = "blank")
dev.off()

### mother MMN and Mother fatty acid
#png("correlogram_motherMMN_MotherFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
png("Sigcorrelogram_motherMMN_MotherFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
cor_M<-cor(dat.cor[,c(21:39,50:57)],use = "complete.obs")
res1 <- cor.mtest(dat.cor[,c(21:39,50:57)], conf.level = .95)
corrplot(cor_M, method="circle",tl.col="black", tl.srt=90,tl.cex =0.8,p.mat = res1$p, sig.level = 0.05, insig = "blank")
dev.off()

### Cord MMN and Mother Fatty acid
#png("correlogram_CORDMMN_MotherFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
png("Sigcorrelogram_CORDMMN_MotherFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
cor_M<-cor(dat.cor[,c(21:39,60:66)],use = "complete.obs")
res1 <- cor.mtest(dat.cor[,c(21:39,60:66)], conf.level = .95)
corrplot(cor_M, method="circle",tl.col="black", tl.srt=90,tl.cex =0.8,p.mat = res1$p, sig.level = 0.05, insig = "blank")
dev.off()

### Cord MMN and cord fatty acid
#png("correlogram_CORDMMN_CORDFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
png("Sigcorrelogram_CORDMMN_CORDFattyAcid.png",height = 6,width = 8,units = 'in',res=300)
cor_M<-cor(dat.cor[,c(2:20,60:66)],use = "complete.obs")
res1 <- cor.mtest(dat.cor[,c(2:20,60:66)], conf.level = .95)
corrplot(cor_M, method="circle",tl.col="black", tl.srt=90,tl.cex =0.8,p.mat = res1$p, sig.level = 0.05, insig = "blank")
dev.off()

## 