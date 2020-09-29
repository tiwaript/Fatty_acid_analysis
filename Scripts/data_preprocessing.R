require(foreign)
data_fatty_acid<-read.spss("PRIYA GIRLS MFA ANALYSIS DATA_26SEP2020.sav",to.data.frame = T,
                           use.value.labels = T)
dat_labels<-data.frame(attr(data_fatty_acid,"variable.labels"))


data_fatty_acid<-data_fatty_acid[-83,] ## Duplicated entry
rownames(data_fatty_acid)<-data_fatty_acid[,1]
