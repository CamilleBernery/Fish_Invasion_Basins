library(ggplot2)
library(ggpubr)
library(dplyr)

rm(list=ls())
setwd("D:/these/Axe_3")
DATABASE<-read.csv2("./Output/DATABASE.csv")
database1<-subset(DATABASE, Freq.exotic>0)
database0<-subset(DATABASE, Freq.exotic==0)

DATABASE$Freq.exotic01<-NA

for (i in 1:length(DATABASE[,1])) {
  if (DATABASE[i,"Freq.exotic"]>0) {
    DATABASE[i,"Freq.exotic01"]<-1
  }else{DATABASE[i,"Freq.exotic01"]<-0}
}

DATABASE$Freq.exotic01<-as.factor(DATABASE$Freq.exotic01)



#############number of natives##################

shapiro.test(DATABASE$Freq.native)
shapiro.test(database1$Freq.native)
shapiro.test(database0$Freq.native)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$Freq.native, breaks=100)
hist(database1$Freq.native, breaks=100)
hist(database0$Freq.native, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=log10(Freq.native+1), x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(database1$Freq.native, 
                    database0$Freq.native)



#############number of dams##################
shapiro.test(DATABASE$Nb.dams)
shapiro.test(database1$Nb.dams)
shapiro.test(database0$Nb.dams)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$Nb.dams, breaks=100)
hist(database1$Nb.dams, breaks=100)
hist(database0$Nb.dams, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=log10(Nb.dams+1), x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(log10(database1$Nb.dams+1), 
            log10(database0$Nb.dams+1))

############temperature amplitude###########


shapiro.test(DATABASE$Temp.ampl)
shapiro.test(database1$Temp.ampl)
shapiro.test(database0$Temp.ampl)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$Temp.ampl, breaks=100)
hist(database1$Temp.ampl, breaks=100)
hist(database0$Temp.ampl, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=log10(Temp.ampl+1), x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(log10(database1$Temp.ampl+1), 
            log10(database0$Temp.ampl+1))


############mean temp###########
shapiro.test(DATABASE$MedianBio1)
shapiro.test(database1$MedianBio1)
shapiro.test(database0$MedianBio1)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$MedianBio1, breaks=100)
hist(database1$MedianBio1, breaks=100)
hist(database0$MedianBio1, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=MedianBio1, x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(database1$MedianBio1, 
            database0$MedianBio1)




############precipitation amplitude###########
shapiro.test(DATABASE$Prec.ampl)
shapiro.test(database1$Prec.ampl)
shapiro.test(database0$Prec.ampl)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$Prec.ampl, breaks=100)
hist(database1$Prec.ampl, breaks=100)
hist(database0$Prec.ampl, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=log10(Prec.ampl+1), x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)



##test
wilcox.test(log10(database1$Prec.ampl+1), 
            log10(database0$Prec.ampl+1))



############mean prec###########
shapiro.test(DATABASE$MedianBio12)
shapiro.test(database1$MedianBio12)
shapiro.test(database0$MedianBio12)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$MedianBio12, breaks=100)
hist(database1$MedianBio12, breaks=100)
hist(database0$MedianBio12, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=MedianBio12, x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(database1$MedianBio12, 
            database0$MedianBio12)


############HFmedian###########
shapiro.test(DATABASE$HFmedian)
shapiro.test(database1$HFmedian)
shapiro.test(database0$HFmedian)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$HFmedian, breaks=100)
hist(database1$HFmedian, breaks=100)
hist(database0$HFmedian, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=HFmedian, x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(database1$HFmedian, 
            database0$HFmedian)


############Elevationmedian###########
shapiro.test(DATABASE$MedianeElev)
shapiro.test(database1$MedianeElev)
shapiro.test(database0$MedianeElev)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$MedianeElev, breaks=100)
hist(database1$MedianeElev, breaks=100)
hist(database0$MedianeElev, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=MedianeElev, x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(database1$MedianeElev, 
            database0$MedianeElev)


############Diff elevation###########
shapiro.test(DATABASE$DiffElev)
shapiro.test(database1$DiffElev)
shapiro.test(database0$DiffElev)
#####none of the modalities are normaly distributed --> Mann whitney test

##histograms
hist(DATABASE$DiffElev, breaks=100)
hist(database1$DiffElev, breaks=100)
hist(database0$DiffElev, breaks=100)


##boxplot
#x11()
ggplot(DATABASE, aes(y=DiffElev, x=Freq.exotic01, color=Freq.exotic01)) +
  geom_boxplot(notch=TRUE)




##test
wilcox.test(database1$DiffElev, 
            database0$DiffElev)
