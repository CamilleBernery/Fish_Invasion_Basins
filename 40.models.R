

library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)
library(cluster)
library(tibble)
library(dummies)
library(PCAmixdata)
library(reshape2)
library(ks)
library(stringr)
library(ggpubr)
library(car)
source("R/R_rainclouds.R")


###get data
rm(list=ls())
setwd("D:/these/Axe_3")
DATABASE<-read.csv2("./Output/DATABASE.csv")


###separate invaded basins and not invaded basins
database1<-subset(DATABASE, Freq.exotic>0)
database0<-subset(DATABASE, Freq.exotic==0)

DATABASE$Freq.exotic01<-NA

for (i in 1:length(DATABASE[,1])) {
  if (DATABASE[i,"Freq.exotic"]>0) {
    DATABASE[i,"Freq.exotic01"]<-1
  }else{DATABASE[i,"Freq.exotic01"]<-0}
}

DATABASE$Freq.exotic01<-as.factor(DATABASE$Freq.exotic01)


####Verify colinearity ----------DATABASE
Data<- DATABASE %>%
  dplyr::select(-c("X.2", "X", "X.1", "Freq.exotic01"))

#correlations
mcor <- cor(Data, use = "pairwise.complete.obs")
mcor
library(corrplot)
corrplot(mcor) 


Data2<- Data[,c("Area.Bassins", "MedianBio1", "MedianBio12", "Temp.ampl","Prec.ampl", "Freq.exotic", "Freq.native", 
                "Nb.dams","HFmedian", "MedianeElev", "DiffElev")]

#correlations
mcor <- cor(Data2, use = "pairwise.complete.obs")
mcor
corrplot(mcor) 

##freqnative-areabassin 0.67
#temp.ampl-mediabio1 -0.64
#♦prec ampl-medianbio12 0.71
#diffelev-medianelev 0.61


#vif----------------DATABASE

# create models
# regressing each trait against all other traits using regression 
# and assessing variance inflation factors (VIF; vif function - ‘car’ package) 


mod_list <- vector(mode = "list")

colSums(is.na(Data2))
data = na.omit(Data2)

for(i in names(data)){
  response = data %>% pull(i)
  predictors = data %>% dplyr::select(-all_of(i))
  mod_list[[i]] <- lm(response ~ ., data = predictors)
}


# Compute vif values
alias(mod_list[[1]])
vif_values <- lapply(mod_list, vif)
vif_all <- bind_rows(vif_values)



# Traits were then removed based on their summed VIF values
# until VIF values were <3 for all remaining traits
 colSums(vif_all, na.rm = T)



var_to_remove <- names(which.max(colSums(vif_all, na.rm = T)))

repeat{
  mod_list <- vector(mode = "list")
  data = na.omit(Data2 %>%
                   dplyr::select(-all_of(var_to_remove)))

  for(i in names(data)){
    response = data %>% pull(i)
    predictors = data %>% dplyr::select(-all_of(i))
    mod_list[[i]] <- lm(response ~ ., data = predictors)
  }

  # Compute vif values
  vif_values <- lapply(mod_list, vif)
  vif_all <- bind_rows(vif_values)
  var_to_remove <- c(var_to_remove, names(which.max(colSums(vif_all, na.rm = T))))

  if(!any(vif_all>3)){ break }
}

var_to_remove
colnames(vif_all)


########"Temp.ampl"   "MedianBio12"








#####colinearity database 1 (que les bassin envahis)################

####Verify colinearity ----------database1

Data1<- database1 %>%
  dplyr::select(-c("X.2", "X", "X.1"))

#correlations
mcor <- cor(Data1, use = "pairwise.complete.obs")
mcor
library(corrplot)
corrplot(mcor) 


Data12<- Data1[,c("Area.Bassins", "MedianBio1", "MedianBio12", "Temp.ampl","Prec.ampl", "Freq.exotic", "Freq.native", 
                "Nb.dams","HFmedian", "MedianeElev", "DiffElev")]

#correlations
mcor <- cor(Data12, use = "pairwise.complete.obs")
mcor
corrplot(mcor) 

##freqnative-areabassin +0.74
#temp.ampl-mediabio1 -0.66
#♦prec ampl-medianbio12 +0.73
#diffelev-medianelev 0.53


#vif----------------database1

# create models
# regressing each trait against all other traits using regression 
# and assessing variance inflation factors (VIF; vif function - ‘car’ package) 


mod_list <- vector(mode = "list")

colSums(is.na(Data12))
data = na.omit(Data12)

for(i in names(data)){
  response = data %>% pull(i)
  predictors = data %>% dplyr::select(-all_of(i))
  mod_list[[i]] <- lm(response ~ ., data = predictors)
}


# Compute vif values
alias(mod_list[[1]])
vif_values <- lapply(mod_list, vif)
vif_all <- bind_rows(vif_values)



# Traits were then removed based on their summed VIF values
# until VIF values were <3 for all remaining traits
colSums(vif_all, na.rm = T)



var_to_remove <- names(which.max(colSums(vif_all, na.rm = T)))

repeat{
  mod_list <- vector(mode = "list")
  data = na.omit(Data12 %>%
                   dplyr::select(-all_of(var_to_remove)))
  
  for(i in names(data)){
    response = data %>% pull(i)
    predictors = data %>% dplyr::select(-all_of(i))
    mod_list[[i]] <- lm(response ~ ., data = predictors)
  }
  
  # Compute vif values
  vif_values <- lapply(mod_list, vif)
  vif_all <- bind_rows(vif_values)
  var_to_remove <- c(var_to_remove, names(which.max(colSums(vif_all, na.rm = T))))
  
  if(!any(vif_all>3)){ break }
}

var_to_remove   ##"Temp.ampl"    "Area.Bassins" "MedianBio12" 
colnames(vif_all)




#######variable à considiérer pouir les modèles

Datamodel<- na.omit(Data1[,c("MedianBio1","Prec.ampl", "Freq.exotic", "Freq.native", 
                  "Nb.dams","HFmedian", "MedianeElev", "DiffElev")])


hist(Datamodel$Freq.exotic, breaks=20)

mod<-glm(Freq.exotic~MedianBio1+Prec.ampl+Freq.native+ 
         Nb.dams+HFmedian+MedianeElev+DiffElev, 
         data = Datamodel, na.action = na.omit, family="poisson" ) 


##Overdispersion??
library(AER)
library(DHARMa)
summary(mod)
dis<-dispersiontest(mod) ##yes

sim <- simulateResiduals(mod, n=99)
testDispersion(sim)
#AIC
AIC(mod)

##overdispersion donc negative bino
library(MASS)
modnb<-glm.nb(Freq.exotic~MedianBio1+Prec.ampl+Freq.native+ 
           Nb.dams+HFmedian+MedianeElev+DiffElev, 
         data = Datamodel) 

#AIC
AIC(modnb)

##still overdispersion? : no
sim <- simulateResiduals(modnb, n=99)
testDispersion(sim)


###step AIC
modnbfiltre<- stepAIC(modnb, direction = "both")
modnbfiltre$theta
modnb$theta
AIC(modnbfiltre)

#shaoptest
shap<-shapiro.test(residuals(modnbfiltre))


###Residus   #####PAS DU TOUT NORMAUX QUE FAIRE???????
res<-resid(modnbfiltre)
res<-as.data.frame(res)


######pvalues
summary(modnbfiltre)
anov<-Anova(modnbfiltre, test='F')
infopvalueglobalintro[["globalintro"]]<-anov


######estimates
est<-summary(modnbfiltre)
str(est)
est$coefficients
estimates<-est$coefficients[,c("Estimate","Std. Error")]

##variable importance
library(caret)
imp<-varImp(modnbfiltre)


#####pseudo r2
R2<-1-(modnbfiltre$deviance/modnbfiltre$null.deviance)
R2

