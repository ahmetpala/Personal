library(openxlsx)
library(BBmisc)
library(neuralnet)
library(dplyr)
library(caret)
library(stringr)
library(ggplot2)
library(psych)
library(lme4)
library(afex)

reports_original<-read.xlsx("C:/Users/ahmet.pala/Desktop/Ahmet Pala - Personal/M.S. Thesis/Raporlar/reports_h.xlsx")
reports<-reports_original[,-c(3,7,33,34)]
colnames(reports)[9]<-"Ts"


plotting<-function(Hull_No,Load_Cond){
  idata<-data.frame(reports[reports$Model_No==Hull_No,])
  x=reports[reports$Model_No==Hull_No & reports$Loading_Condition == Load_Cond,32]
  y=reports[reports$Model_No==Hull_No & reports$Loading_Condition == Load_Cond,40]
  plot(x,y,ylab = "Hull Resistance (N)",xlab = "Speed (m/s)",main = paste(Hull_No,idata[1,3],Load_Cond,sep="-"))
  #text(x,y, round(y,2), cex=1)
}

# Data Summary
str(reports)
experiments_hull_based<-data.frame(table(reports$Model_No))
colnames(experiments_hull_based)<-c("Model_No","Total_Experiments")
experiments_hull_based<-merge(experiments_hull_based,unique(reports[,c(2,3)]),by="Model_No",all.y = FALSE) 
experiments_hull_based<-experiments_hull_based[,c(1,3,2)] #Total Number of experiments for each Hull with their types


experiments_type_based<-data.frame(aggregate(Total_Experiments~ Ship_Type,data = experiments_hull_based, sum))
d<-data.frame(table(experiments_hull_based$Ship_Type))
colnames(d)<-c("Ship_Type","Number_of_Hulls")
experiments_type_based<-merge(experiments_type_based,d,by="Ship_Type")
experiments_type_based<-experiments_type_based[,c(1,3,2)] #Total Number of Ship Types with Total Number of Experiments



#Plotting Some Resistance Values with Speeds
plotting(380,"Design") #Barge
plotting(380,"Ballast")

plotting(289,"Design") #Bulk Carrier
plotting(289,"Ballast")

plotting(404,"Design") #Catamaran

plotting(255,"Design") #Container
plotting(255,"Ballast")

plotting(288,"Design") #Ferry

plotting(422,"Design") #Fishing Vessel
plotting(422,"Ballast")

plotting(306,"Design") #General Cargo Ship
plotting(306,"Ballast")

plotting(292,"Design") #Motor Yacht
plotting(292,"Heavy Loaded")

plotting(332,"Design") #Speacial Purpose Ship
plotting(332,"Heavy Loaded")

plotting(265,"Design") #Tanker
plotting(265,"Ballast")

plotting(340,"Design") #Tugboat
plotting(340,"Ballast")

colnames(reports)
pairs(~Hull_Total_Res+`LWL`+`BWL`+`Ts`+`N`+`AT`+`CB`+
        `CP`+ `Vs`,
      data=cargoships)


#### At This point, Holtrop Results will be added to the plots ####
plotting(424,"Design") #General Cargo
lines(reports[reports$Model_No==424 & reports$Loading_Condition == "Design",32],
      reports[reports$Model_No==424 & reports$Loading_Condition == "Design",44],col="green")


# Initial Linear Model
cargoships<-reports[reports$Ship_Type=="Tanker" | reports$Ship_Type== "General Cargo Ship" 
                    | reports$Ship_Type=="Container Ship" | reports$Ship_Type=="Bulk Carrier"
                    | reports$Ship_Type=="Ferry",]

# After all, we know that transforms of Vs is quite significant for this problem. Therefore, we will try to add them to the model.

set.seed(2020) # Homogenious data splitting for train and test
cargoships_unique<-cargoships
cargoships_unique$merged<-paste(cargoships_unique$Model_No,cargoships_unique$Ship_Type,sep = ",")
cargoships_unique<-data.frame(unique(cargoships_unique$merged))
colnames(cargoships_unique)<-"merged"

cargoships_unique<-data.frame(str_split_fixed(cargoships_unique$merged, ",", 2))
colnames(cargoships_unique)<-c("Model_No","Ship_Type")


train.rows<- createDataPartition(y= cargoships_unique$Ship_Type, p=0.33, list = FALSE)
train_Model_Nos<-cargoships_unique[train.rows,]

train.data<-cargoships[cargoships$Model_No %in% train_Model_Nos$Model_No,]
test.data<-cargoships[!(cargoships$Model_No %in% train_Model_Nos$Model_No),]



lm1<-lm(log(Hull_Total_Res) ~ LBP + LWL + BWL + Ts + TA + TF + N + AWS +
          AR + AB + HB + AT + HT + CB + CP + CM + CWP + LCB + LCF + Fr +
           Bulb + Number_of_Propeller + Vs + L_B_Ratio, 
         data=train.data) # It is the most suitable transformation
plot(lm1,which = 1) # Constant Variance assumption is satisfied. Linearity assumption is violated.
mean(lm1$residuals)
summary(lm1)
AIC(lm1)

lm2<-lm(log(Hull_Total_Res) ~ LBP + LWL + BWL + Ts + TA + TF + N + AWS +
          AR + AB + HB + AT + HT + CB + CP + CM + CWP + LCB + LCF + Fr +
          Bulb + Number_of_Propeller + Vs + L_B_Ratio + sqrt(LWL), 
        data=train.data) # Adding sqrt(LWL)
plot(lm2,which = 1)
AIC(lm2) # AIC was decreased. sqrt(LWL) is significant
summary(lm2)

lm3<-lm(log(Hull_Total_Res) ~ LBP + LWL + BWL + Ts + TA + TF + N + AWS +
          AR + AB + HB + AT + HT + CB + CP + CM + CWP + LCB + LCF + Fr +
          Bulb + Number_of_Propeller + Vs + I(Vs^2) + L_B_Ratio + sqrt(LWL), 
        data=train.data) # Adding Vs^2
plot(lm3,which = 1)
AIC(lm3) # AIC was decreased. Vs^2 is significant
summary(lm3)

lm4<-lm(log(Hull_Total_Res) ~ LBP + LWL + BWL + Ts + TA + TF + N + AWS +
          AR + AB + HB + AT + HT + CB + CP + CM + CWP + LCB + LCF + Fr +
          Bulb + Number_of_Propeller + Vs + I(Vs^2) + sqrt(Vs) + L_B_Ratio + sqrt(LWL), 
        data=train.data) # Adding sqrt(Vs)
plot(lm4,which = 1)
AIC(lm4) # AIC was decreased. sqrt(Vs) is significant
summary(lm4)

lm5<-lm(log(Hull_Total_Res) ~ LBP + LWL + BWL + Ts + TA + TF + N + AWS +
          AR + AB + HB + AT + HT + CB + CP + CM + CWP + LCB + LCF + Fr +
          Bulb + Number_of_Propeller + Vs + I(Vs^2) + I(Vs^3)+ sqrt(Vs) + L_B_Ratio + sqrt(LWL), 
        data=train.data) # Adding Vs^3
plot(lm5,which = 1)
AIC(lm5) # AIC was decreased. Vs^3 is significant
summary(lm5) # Now, I removed insignificant variables step by step.

lm6<-lm(log(Hull_Total_Res) ~ LBP + LWL + BWL + Ts + N +
          AR + HT + CB + CM + CWP + LCB + LCF + Fr +
          Bulb + Number_of_Propeller + Vs + I(Vs^2) + I(Vs^3)+ sqrt(Vs) + L_B_Ratio + sqrt(LWL), 
        data=train.data)
plot(lm6,which = 1)
AIC(lm6) 
summary(lm6)


results<-(data.frame(exp(predict(lm6,test.data))))
colnames(results)<-"LM1"
results_data<-cbind(test.data,results)
results_data<-results_data[,c(1:39,41,42,43,44,40,45)]

MSE.lm <- function(Ship_Type){
  MSE<-sum((results_data[results_data$Ship_Type==Ship_Type,44] - 
         results_data[results_data$Ship_Type==Ship_Type,45])^2)/nrow(results_data[results_data$Ship_Type==Ship_Type,])
  return(MSE)
}

MSE.Overall<-sum((results_data$Hull_Total_Res - results_data$LM1)^2)/nrow(results_data)

MSE.Tanker<-MSE.lm("Tanker")
MSE.Container<-MSE.lm("Container Ship")
MSE.BulkCarrier<-MSE.lm("Bulk Carrier")
MSE.Ferry<-MSE.lm("Ferry")
MSE.GeneralCargo<-MSE.lm("General Cargo Ship")
MSE.Summary<-data.frame(rbind(MSE.Tanker,MSE.Container,MSE.BulkCarrier,MSE.Ferry,MSE.GeneralCargo,MSE.Overall))
colnames(MSE.Summary)<-"MSE"
MSE.Summary

plot(results_data[results_data$Model_No==306,44], col="blue",data=results_data,type = "p",
     xlab = "Speed Index", ylab = "Hull Resistance", 
     main = "306 - General Cargo Ship" )
lines(results_data[results_data$Model_No==306,45],col="red", type = "p")
legend(0.01, 18, legend=c("Real Hull Resistance", "Predicted Resistance"),
       col=c("blue", "red"), lty=1:2, cex=0.6)

plot(results_data[results_data$Model_No==289,44], col="blue",data=results_data,type = "p",
     xlab = "Speed Index", ylab = "Hull Resistance", 
     main = "289 - Bulk Carrier" )
lines(results_data[results_data$Model_No==289,45],col="red", type = "p")
legend(0, 17, legend=c("Real Hull Resistance", "Predicted Resistance"),
       col=c("blue", "red"), lty=1:2, cex=0.5)

plot(results_data[results_data$Model_No==309,44], col="blue",data=results_data,type = "p",
     xlab = "Speed Index", ylab = "Hull Resistance", 
     main = "309 - Tanker" )
lines(results_data[results_data$Model_No==309,45],col="red", type = "p")
legend(0, 18, legend=c("Real Hull Resistance", "Predicted Resistance"),
       col=c("blue", "red"), lty=1:2, cex=0.6)


plot(results_data[results_data$Model_No==293,44], col="blue",data=results_data,type = "p",
     xlab = "Speed Index", ylab = "Hull Resistance", 
     main = "293 - Container Ship" )
lines(results_data[results_data$Model_No==293,45],col="red", type = "p")
legend(0, 16, legend=c("Real Hull Resistance", "Predicted Resistance"),
       col=c("blue", "red"), lty=1:2, cex=0.6)

plot(results_data[results_data$Model_No==288,44], col="blue",data=results_data,type = "p",
     xlab = "Speed Index", ylab = "Hull Resistance", 
     main = "288 - Ferry" ,ylim = c(0,60))
lines(results_data[results_data$Model_No==288,45],col="red", type = "p")
legend(1, 45, legend=c("Real Hull Resistance", "Predicted Resistance"),
       col=c("blue", "red"), lty=1:2, cex=0.6)


# Longitidunal Data Approach Cont. (21.02.2020) - Adding Individual Error Terms
library(nlme)
orthodont<-Orthodont
orthomale<-subset(orthodont, Sex=="Male")
plot(orthomale, ylab = "Distance")

#Analysis Strategy 1 - Linear Models for each individual
lm.list<-lmList(distance ~I(age - 11), data = orthomale)
plot(intervals(lm.list)) #We need to estimate 16*2=32 parameters for this model
#Analysis Strategy 2 - Fixed Effect Models - Fixing the slopes, Intercepts depend on individuals
ortho.lm<-lm(distance ~ Subject + I(age-11) - 1, data = orthomale) # -1 means without intercept.
summary(ortho.lm)

library(effects)
plot(allEffects(ortho.lm), cex=0.8, rotx=90)# Here we need to estimate 17 parameters, it is still high.

#Analysis Strategy 3 - Mixed Effects Model - General Intercept and Slopes, But individual error terms
ortho.lme<-lme(distance ~I(age -11), random = ~ 1|Subject, data = orthomale)
summary(ortho.lme)
intervals(ortho.lme)

ortho.pred<-data.frame(predict(ortho.lme, orthomale))
ortho.pred$real<-orthomale$distance
plot(ortho.pred$predict.ortho.lme..orthomale.)
lines(ortho.pred$real, type = "p", col = "red")
mse.orthomale<-sum((ortho.pred$predict.ortho.lme..orthomale.-ortho.pred$real)^2)/nrow(ortho.pred) # MSE is quite low


# 23.02.2020 - Applying Mixed Effects Models on ship data
cargoships<-reports[reports$Ship_Type=="Tanker" | reports$Ship_Type== "General Cargo Ship" 
                    | reports$Ship_Type=="Container Ship" | reports$Ship_Type=="Bulk Carrier",]

set.seed(1) # Homogenious data splitting for train and test
cargoships$Ind<-paste(cargoships$Model_No,cargoships$Loading_Condition,sep = ",")


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
cargoships.norm<-cargoships
cargoships.norm[,c(6:29,32,33,41,42)] <- as.data.frame(lapply(cargoships[,c(6:29,32,33,41,42)], normalize))

cargoships_unique<-cargoships
cargoships_unique$merged<-paste(cargoships_unique$Model_No,cargoships_unique$Ship_Type,sep = ",")
cargoships_unique<-data.frame(unique(cargoships_unique$merged))
colnames(cargoships_unique)<-"merged"

cargoships_unique<-data.frame(str_split_fixed(cargoships_unique$merged, ",", 2))
colnames(cargoships_unique)<-c("Model_No","Ship_Type")


train.rows<- createDataPartition(y= cargoships_unique$Ship_Type, p=0.6, list = FALSE)
train_Model_Nos<-cargoships_unique[train.rows,]

train.data<-cargoships.norm[cargoships.norm$Model_No %in% train_Model_Nos$Model_No,]
test.data<-cargoships.norm[!(cargoships.norm$Model_No %in% train_Model_Nos$Model_No),]

# Building initial linear model with individual error terms


lmer.cargoships<-lmer(log(Hull_Total_Res) ~ LWL + BWL + 
                        AWS + CP + CM + Bulb + Vs + I(Vs^2) +
                        sqrt(Vs) + (1|Ind), data = train.data)
summary(lmer.cargoships)
AIC(lmer.cargoships)
plot(lmer.cargoships)

pairs.panels(cargoships[,c(7,8,14,22,23,30,32)],
             gap=0,
             pch = 21,cex.main=1.75, cex.lab=1.54, cex.axis=2) # MELM selected input relationship graph


 
#1-var(residuals(lmer.cargoships))/(var(model.response(model.frame(lmer.cargoships)))) # R-squared value of the lmer model
coef(lmer.cargoships)


SSE<-data.frame(exp(fitted(lmer.cargoships)))
SSE$real<-train.data$Hull_Total_Res
colnames(SSE)<-c("prediction","real")
mean_real<-mean(SSE$real)
SSE$errors<-(SSE$real-SSE$prediction)^2
SSE$sst<-(SSE$real-mean_real)^2
1-sum(SSE$errors)/sum(SSE$sst)

#Try to predict test data and check the model performance
results.lmer<-(data.frame(exp(predict(lmer.cargoships,test.data, allow.new.levels = TRUE))))
colnames(results.lmer)<-"LMER"
results_data_lmer<-cbind(test.data,results.lmer)
results_data_lmer<-results_data_lmer[,c(1:39,41,42,43,44,45,40,46)]

MSE.lmer <- function(Ship_Type){
  MSE<-sum((results_data_lmer[results_data_lmer$Ship_Type==Ship_Type,45] - 
              results_data_lmer[results_data_lmer$Ship_Type==Ship_Type,46])^2)/nrow(results_data_lmer[results_data_lmer$Ship_Type==Ship_Type,])
  return(MSE)
}

MSE.Overall<-sum((results_data_lmer$Hull_Total_Res - results_data_lmer$LMER)^2)/nrow(results_data_lmer)

MSE.Tanker<-MSE.lmer("Tanker")
MSE.Container<-MSE.lmer("Container Ship")
MSE.BulkCarrier<-MSE.lmer("Bulk Carrier")
#MSE.Ferry<-MSE.lmer("Ferry")
MSE.GeneralCargo<-MSE.lmer("General Cargo Ship")
MSE.Summary<-data.frame(rbind(MSE.Tanker,MSE.Container,MSE.BulkCarrier,MSE.GeneralCargo,MSE.Overall))
colnames(MSE.Summary)<-"MSE"
MSE.Summary


set.seed(1)
CrossValidationMELM<-function(data,k,repl=1){
  
  result_total_melm_cv<-data.frame(matrix(nrow = repl*k,ncol = 7))
  colnames(result_total_melm_cv)<-c("rep","Fold","MELM_Train","MELM_Test","MaxErrorID","MEshiptype","AvrMSE")
  for(r in 1:repl){
    sumerr=0
    for(i in 1:k){
      currentInd=(r-1)*k+i
      result_total_melm_cv[currentInd,1]<-r
      result_total_melm_cv[currentInd,2]<-i
      data_unique<-data
      data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
      data_unique<-data.frame(unique(data_unique$merged))
      colnames(data_unique)<-"merged"
      
      data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
      colnames(data_unique)<-c("Model_No","Ship_Type")
      
      prob=1-(1/k)
      train1.rows<- createDataPartition(y= data_unique$Ship_Type, p=prob, list = FALSE)
      train1_Model_Nos<-data_unique[train1.rows,]
      
      train1.data<-data[data$Model_No %in% train1_Model_Nos$Model_No,]
      test1.data<-data[!(data$Model_No %in% train1_Model_Nos$Model_No),]
      
      MELM<-lmer(log(Hull_Total_Res) ~ LWL + BWL + AWS +
                   CP + CM +  
                   Bulb + Vs + I(Vs^2) + sqrt(Vs)  +  
                  (1|Ind), data = train1.data)
      
      pr.melm.test <- predict(MELM,test1.data,allow.new.levels = TRUE)
      pr.melm_test<-exp(pr.melm.test)
      test.r <- test1.data$Hull_Total_Res
      
      pr.melm.train <- predict(MELM,train1.data,allow.new.levels = TRUE)
      pr.melm_train<-exp(pr.melm.train)
      train.r <- train1.data$Hull_Total_Res
      
      MSE.melm.test <- sum((test.r - pr.melm_test)^2)/nrow(test1.data)
      result_total_melm_cv[currentInd,4]<-MSE.melm.test
      sumerr<-sumerr+MSE.melm.test
      #max error
      errorsFold<-data.frame((test.r - pr.melm_test)^2)
      colnames(errorsFold)<-"error"
      indice<-which(errorsFold$error==max(errorsFold$error))
      result_total_melm_cv[currentInd,5]<-test1.data[indice,2]
      result_total_melm_cv[currentInd,6]<-test1.data[indice,3]
      
      MSE.melm.train <- sum((train.r - pr.melm_train)^2)/nrow(train1.data)
      result_total_melm_cv[currentInd,3]<-MSE.melm.train
    }
    averaFolds<-sumerr/k
    for(t in 1:k){
      result_total_melm_cv[(r-1)*k+t,7]<-averaFolds
    }
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_melm_cv)
}
melm_cv_summary<-CrossValidationMELM(cargoships.norm,4,repl = 50)
head(melm_cv_summary, n=20)
melm_cv_summary$merged<-paste(melm_cv_summary$MaxErrorID,melm_cv_summary$MEshiptype,sep = "-")
mean(melm_cv_summary$AvrMSE)


# Standard Linear Model on The Same Dataset (25.02.2020)
lm.wtf<-lm(log(Hull_Total_Res) ~ Ship_Type + LWL + N +
             CB + CM + CWP + Bulb + Number_of_Propeller + 
             Vs + I(Vs^2) + I(Vs^3)+ sqrt(Vs) + L_B_Ratio, 
        data=train.data) 
plot(lm.wtf,which = 1)
AIC(lm.wtf) 
summary(lm.wtf) 


results.lm.wtfr<-(data.frame(exp(predict(lm.wtf,test.data))))
colnames(results.lm.wtfr)<-"LMER"
results.lm.wtfr<-cbind(test.data,results.lm.wtfr)
results.lm.wtfr<-results.lm.wtfr[,c(1:39,41,42,43,44,45,40,46)]

MSE.lm.wtf <- function(Ship_Type){
  MSE<-sum((results.lm.wtfr[results.lm.wtfr$Ship_Type==Ship_Type,45] - 
              results.lm.wtfr[results.lm.wtfr$Ship_Type==Ship_Type,46])^2)/nrow(results.lm.wtfr[results.lm.wtfr$Ship_Type==Ship_Type,])
  return(MSE)
}

MSE.Overall<-sum((results.lm.wtfr$Hull_Total_Res - results.lm.wtfr$LMER)^2)/nrow(results.lm.wtfr)

MSE.Tanker<-MSE.lm.wtf("Tanker")
MSE.Container<-MSE.lm.wtf("Container Ship")
MSE.BulkCarrier<-MSE.lm.wtf("Bulk Carrier")
#MSE.Ferry<-MSE.lmer("Ferry")
MSE.GeneralCargo<-MSE.lm.wtf("General Cargo Ship")
MSE.Summary<-data.frame(rbind(MSE.Tanker,MSE.Container,MSE.BulkCarrier,MSE.GeneralCargo,MSE.Overall))
colnames(MSE.Summary)<-"MSE"
MSE.Summary

set.seed(2)
CrossValidationGLM<-function(data,k,repl=1){
  result_total_glm_cv<-data.frame(matrix(nrow = repl*k,ncol = 7))
  colnames(result_total_glm_cv)<-c("rep","Fold","GLM_Train","GLM_Test","MaxErrorID","MEshiptype","AvrMSE")
  for(r in 1:repl){
 sumerr=0
  for(i in 1:k){
    currentInd=(r-1)*k+i
    result_total_glm_cv[currentInd,1]<-r
    result_total_glm_cv[currentInd,2]<-i
    data_unique<-data
    data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
    data_unique<-data.frame(unique(data_unique$merged))
    colnames(data_unique)<-"merged"
    
    data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
    colnames(data_unique)<-c("Model_No","Ship_Type")
    
    prob=1-(1/k)
    train1.rows<- createDataPartition(y= data_unique$Ship_Type, p=prob, list = FALSE)
    train1_Model_Nos<-data_unique[train1.rows,]
    
    train1.data<-data[data$Model_No %in% train1_Model_Nos$Model_No,]
    test1.data<-data[!(data$Model_No %in% train1_Model_Nos$Model_No),]
    
    GLM<-lm(log(Hull_Total_Res) ~ Ship_Type + LWL + N +
              CB + CM + CWP + Bulb + Number_of_Propeller + 
              Vs + I(Vs^2) + I(Vs^3)+ sqrt(Vs) + L_B_Ratio ,
              data=train1.data) 
    
    pr.glm.test <- predict(GLM,test1.data)
    pr.glm_test<-exp(pr.glm.test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.glm.train <- predict(GLM,train1.data)
    pr.glm_train<-exp(pr.glm.train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.glm.test <- sum((test.r - pr.glm_test)^2)/nrow(test1.data)
    result_total_glm_cv[currentInd,4]<-MSE.glm.test
    sumerr<-sumerr+MSE.glm.test
    #max error
    errorsFold<-data.frame((test.r - pr.glm_test)^2)
    colnames(errorsFold)<-"error"
    indice<-which(errorsFold$error==max(errorsFold$error))
    result_total_glm_cv[currentInd,5]<-test1.data[indice,2]
    result_total_glm_cv[currentInd,6]<-test1.data[indice,3]
    
    MSE.glm.train <- sum((train.r - pr.glm_train)^2)/nrow(train1.data)
    result_total_glm_cv[currentInd,3]<-MSE.glm.train
  }
    averaFolds<-sumerr/k
    for(t in 1:k){
      result_total_glm_cv[(r-1)*k+t,7]<-averaFolds
    }
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_glm_cv)
}
glm_cv_summary<-CrossValidationGLM(cargoships.norm,4,repl = 50)
head(glm_cv_summary,n=20)
glm_cv_summary$merged<-paste(glm_cv_summary$MaxErrorID,glm_cv_summary$MEshiptype,sep = "-")
mean(glm_cv_summary$AvrMSE)

table(glm_cv_summary$merged)

# Model No 274N Investigation
aa<-data.frame(colMeans(cargoships[cargoships$Model_No!="274N",6:41]))
bb<-data.frame(colMeans(cargoships[cargoships$Model_No=="274N"&cargoships$Loading_Condition=="Design",
                                   6:41]))
cc<-data.frame(colMeans(cargoships[,6:41]))
H274N_Inv<-cbind(cc,aa,bb)
colnames(H274N_Inv)<-c("All_Ships","Tankers_without_H274N","H274N")
H274N_Inv$diff_Tankerss<-abs((H274N_Inv$Tankers_without_H274N-H274N_Inv$H274N)/H274N_Inv$Tankers_without_H274N)*100
H274N_Inv$diff_All<-H274N_Inv$All_Ships-H274N_Inv$H274N


### GLM ### 
# GLM with Gamma Distribution and Log Link Function
glm1<-glm(Hull_Total_Res ~ LWL + BWL + TA + AWS +
            AB + AT + CP + CM + CWP + Vs + I(Vs^2) + I(Vs^3), 
          data=train.data,family = Gamma(link = "log"))
plot(glm1,which=1)
summary(glm1)
AIC(glm1)


pairs.panels(cargoships[,c(7,8,10,14,17,19,22,23,24,32)],
             gap=0,
             pch = 21,cex.main=1.75, cex.lab=1.54, cex.axis=2) # GLM selected input relationship graph


CrossValidationGLM_Real<-function(data,k,repl=1){
  result_total_glm_cv<-data.frame(matrix(nrow = repl*k,ncol = 7))
  colnames(result_total_glm_cv)<-c("rep","Fold","GLM_Train","GLM_Test","MaxErrorID","MEshiptype","AvrMSE")
  for(r in 1:repl){
    sumerr=0
    for(i in 1:k){
      currentInd=(r-1)*k+i
      result_total_glm_cv[currentInd,1]<-r
      result_total_glm_cv[currentInd,2]<-i
      data_unique<-data
      data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
      data_unique<-data.frame(unique(data_unique$merged))
      colnames(data_unique)<-"merged"
      
      data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
      colnames(data_unique)<-c("Model_No","Ship_Type")
      
      prob=1-(1/k)
      train1.rows<- createDataPartition(y= data_unique$Ship_Type, p=prob, list = FALSE)
      train1_Model_Nos<-data_unique[train1.rows,]
      
      train1.data<-data[data$Model_No %in% train1_Model_Nos$Model_No,]
      test1.data<-data[!(data$Model_No %in% train1_Model_Nos$Model_No),]
      
      GLM<-glm(Hull_Total_Res ~ LWL + BWL + TA + AWS +
                 AB + AT + CP + CM + CWP + 
                 Vs + I(Vs^2) + I(Vs^3), 
               data=train.data,family = Gamma(link = "log"))
      
      pr.glm.test <- predict(GLM,test1.data)
      pr.glm_test<-exp(pr.glm.test)
      test.r <- test1.data$Hull_Total_Res
      
      pr.glm.train <- predict(GLM,train1.data)
      pr.glm_train<-exp(pr.glm.train)
      train.r <- train1.data$Hull_Total_Res
      
      MSE.glm.test <- sum((test.r - pr.glm_test)^2)/nrow(test1.data)
      result_total_glm_cv[currentInd,4]<-MSE.glm.test
      sumerr<-sumerr+MSE.glm.test
      #max error
      errorsFold<-data.frame((test.r - pr.glm_test)^2)
      colnames(errorsFold)<-"error"
      indice<-which(errorsFold$error==max(errorsFold$error))
      result_total_glm_cv[currentInd,5]<-test1.data[indice,2]
      result_total_glm_cv[currentInd,6]<-test1.data[indice,3]
      
      MSE.glm.train <- sum((train.r - pr.glm_train)^2)/nrow(train1.data)
      result_total_glm_cv[currentInd,3]<-MSE.glm.train
    }
    averaFolds<-sumerr/k
    for(t in 1:k){
      result_total_glm_cv[(r-1)*k+t,7]<-averaFolds
    }
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_glm_cv)
}
deneme<-CrossValidationGLM_Real(cargoships.norm,k=4,repl = 50)
mean(deneme$AvrMSE)


### ANN Model (13.03.2020) ###
#Normalizing the Data Set first
cargoships<-reports[reports$Ship_Type=="Tanker" | reports$Ship_Type== "General Cargo Ship" 
                    | reports$Ship_Type=="Container Ship" | reports$Ship_Type=="Bulk Carrier",]

set.seed(999999999) # Homogenious data splitting for train and test
cargoships$Ind<-paste(cargoships$Model_No,cargoships$Loading_Condition,sep = ",")
cargoships$L_Hull_Total_Res<-log(cargoships$Hull_Total_Res)
cargoships$Vs2<-(cargoships$Vs)^2
cargoships$sqVs<-(cargoships$Vs^(0.5))
cargoships$sqLWL<-(cargoships$LWL^(0.5))
cargoships$Container<-ifelse(cargoships$Ship_Type=="Container Ship",1,0)
cargoships$GeneralCargo<-ifelse(cargoships$Ship_Type=="General Cargo Ship",1,0)
cargoships$BulkCarrier<-ifelse(cargoships$Ship_Type=="Bulk Carrier",1,0)
cargoships$Tanker<-ifelse(cargoships$Ship_Type=="Tanker",1,0)

cargoships_unique<-cargoships # Stratified Sampling
cargoships_unique$merged<-paste(cargoships_unique$Model_No,cargoships_unique$Ship_Type,sep = ",")
cargoships_unique<-data.frame(unique(cargoships_unique$merged))
colnames(cargoships_unique)<-"merged"

cargoships_unique<-data.frame(str_split_fixed(cargoships_unique$merged, ",", 2))
colnames(cargoships_unique)<-c("Model_No","Ship_Type")

train.rows<- createDataPartition(y= cargoships_unique$Ship_Type, p=0.75, list = FALSE)
train_Model_Nos<-cargoships_unique[train.rows,]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
cargoships.norm<-cargoships
cargoships.norm[,c(6:29,32,33,41,42)] <- as.data.frame(lapply(cargoships[,c(6:29,32,33,41,42)], normalize))
train.data<-cargoships.norm[cargoships.norm$Model_No %in% train_Model_Nos$Model_No,]
test.data<-cargoships.norm[!(cargoships.norm$Model_No %in% train_Model_Nos$Model_No),]

train.data.norm<-train.data
test.data.norm<-test.data



#  Cross Validation with Forward Feature Selection Results
CrossValidationAnn<-function(data,k,nofNeuron,algo="rprop+",AF="logistic",repl=1,thresh=0.4){

  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("Fold","MSE_Train","MSE_Test")
  result_total_ann_myv$Fold<-1:k
  for(i in 1:k){
    data_unique<-data
    data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
    data_unique<-data.frame(unique(data_unique$merged))
    colnames(data_unique)<-"merged"
    
    data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
    colnames(data_unique)<-c("Model_No","Ship_Type")
    
    prob=1-(1/k)
    train1.rows<- createDataPartition(y= data_unique$Ship_Type, p=prob, list = FALSE)
    train1_Model_Nos<-data_unique[train1.rows,]
    
    train1.data<-data[data$Model_No %in% train1_Model_Nos$Model_No,]
    test1.data<-data[!(data$Model_No %in% train1_Model_Nos$Model_No),]
    
    ann<-neuralnet(Hull_Total_Res ~ Vs + AWS + CM + CB + LWL + HT + D, 
                   data = train1.data, hidden = nofNeuron,
                   threshold = thresh, lifesign = "minimal", rep = repl,
                   algorithm = algo,act.fct = AF,
                   lifesign.step = 10, linear.output = TRUE,stepmax=1e7)
    
    ahm<-data.frame(t(ann$result.matrix))
    pr.nn.test <- compute(ann,test1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_test <- pr.nn.test$net.result
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn.train <- compute(ann,train1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_train <- pr.nn.train$net.result
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    #max error
    errorsFold<-data.frame((test.r - pr.nn_test)^2)
    colnames(errorsFold)<-"error"
    indice<-which(errorsFold$error==max(errorsFold$error))
    result_total_ann_myv[i,4]<-test1.data[indice,2]
    result_total_ann_myv[i,5]<-test1.data[indice,3]
    result_total_ann_myv[i,6]<-errorsFold[indice,1]
 }
  return(mean(result_total_ann_myv$MSE_Test))
  #return(result_total_ann_myv)
}
deneme<-CrossValidationAnn(cargoships.norm,k=4,nofNeuron = 2,thresh = 0.5)
deneme

experimentation<-data.frame(matrix(nrow = 20,ncol = 6))
colnames(experimentation)<-c("Neuron","Algorithm","ActivationFunc","MSEcrossTest","MSEforOriginalTrain","MSEforORiginalTest")
algorithmsforCV<-c('rprop+', 'rprop-', 'slr')
actFuncCV<-c( 'logistic' , 'tanh')
countE=1
set.seed(2021)
for(n in 1:10){
  for(a in 1:2){
    for(ac in 1:1){
      experimentation[countE,1]<-n
      experimentation[countE,2]<-algorithmsforCV[a]
      experimentation[countE,3]<-actFuncCV[ac]
      experimentation[countE,4]<-CrossValidationAnn(cargoships.norm,k=4,nofNeuron = n,
                                                   algo =algorithmsforCV[a],AF=actFuncCV[ac], repl=1,thresh = 0.5)
      annAll<-neuralnet(Hull_Total_Res ~ Vs + AWS + CM + CB + LWL + HT + D, 
                     data = train.data.norm, hidden = n,
                     threshold = 0.5, lifesign = "minimal", rep = 1,algorithm = algorithmsforCV[a],act.fct =actFuncCV[ac],
                     lifesign.step = 10, linear.output = TRUE,stepmax=1e7)
     
      ahm<-data.frame(t(annAll$result.matrix))
      pr.nn.test <- compute(annAll,test.data.norm,rep = which(ahm$error==min(ahm$error)))
      pr.nn_test <- pr.nn.test$net.result
      #pr.nn_test<-exp(pr.nn_test)
      test.r <- test.data.norm$Hull_Total_Res
      experimentation[countE,6] <- sum((test.r - pr.nn_test)^2)/nrow(test.data.norm)
      
      pr.nn.train <- compute(annAll,train.data.norm,rep = which(ahm$error==min(ahm$error)))
      pr.nn_train <- pr.nn.train$net.result
      #pr.nn_train<-exp(pr.nn_train)
      train.r <- train.data.norm$Hull_Total_Res
     
      experimentation[countE,5]<- sum((train.r - pr.nn_train)^2)/nrow(train.data.norm)
      countE<-countE+1
    }
  }
}
experimentation


#feature selection using forward selection with ann cross validation
# First Feature Selection Results by Column Numbers: 32 12 23 22 20 21 15 26  8  9
CrossValidationAnnFeature<-function(data,FeatureSet,k=4,nofNeuron=2,algo="rprop+",AF="logistic",repl=1,thresh=0.5){
  if(length(FeatureSet)>1){ f <- as.formula(paste("Hull_Total_Res ~", paste(colnames(cargoships[,FeatureSet]), collapse =  " + ")))
  }
  if(length(FeatureSet)==1){ f <- as.formula(paste("Hull_Total_Res ~", colnames(cargoships[FeatureSet[1]])))
  }
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 6))
  colnames(result_total_ann_myv)<-c("Fold","MSE_Train","MSE_Test","HullNo","ShipType","MaxError")
  result_total_ann_myv$Fold<-1:k
  for(i in 1:k){
    data_unique<-data
    data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
    data_unique<-data.frame(unique(data_unique$merged))
    colnames(data_unique)<-"merged"
    
    data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
    colnames(data_unique)<-c("Model_No","Ship_Type")
    
    prob=1-(1/k)
    train1.rows<- createDataPartition(y= data_unique$Ship_Type, p=prob, list = FALSE)
    train1_Model_Nos<-data_unique[train1.rows,]
    
    train1.data<-data[data$Model_No %in% train1_Model_Nos$Model_No,]
    test1.data<-data[!(data$Model_No %in% train1_Model_Nos$Model_No),]
    
    ann<-neuralnet(f, data = train1.data, hidden = nofNeuron,
                   threshold = thresh, lifesign = "minimal", rep = repl,
                   algorithm = algo,act.fct = AF,
                   lifesign.step = 10, linear.output = TRUE,stepmax=1e7)
    if(length(ann)>9){
    ahm<-data.frame(t(ann$result.matrix))
    pr.nn.test <- compute(ann,test1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_test <- pr.nn.test$net.result
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn.train <- compute(ann,train1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_train <- pr.nn.train$net.result
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    #max error
    errorsFold<-data.frame((test.r - pr.nn_test)^2)
    colnames(errorsFold)<-"error"
    indice<-which(errorsFold$error==max(errorsFold$error))
    result_total_ann_myv[i,4]<-test1.data[indice,2]
    result_total_ann_myv[i,5]<-test1.data[indice,3]
    result_total_ann_myv[i,6]<-errorsFold[indice,1]
    }
    if(length(ann)==9){
      result_total_ann_myv[i,4]<-NA
      result_total_ann_myv[i,5]<-NA
      result_total_ann_myv[i,3]<-100000000
      result_total_ann_myv[i,2]<-100000000
    }
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_ann_myv)
}
#forward selection
allFeatures<-c(7:15,17:26,30:32,41)
selectedFeatures<-vector("numeric",length = 0)
#numberofFeatures<-length(allFeatures)
numberofFeatures<-10

FeatureSelectionSummary<-data.frame(matrix(nrow=numberofFeatures,ncol=7))
colnames(FeatureSelectionSummary)<-c("nofFeatures","MeanCVTrain","MeanCVTest","MaxErrorHullno","MaxErrorShipType","MaxError","formula")
set.seed(2021)
for(i in 1:numberofFeatures){
  minError=100000000
  minindex<-1
  maxError<-0
  shipType<-"addas"
  hullno<-"akjda"
  averageErrorTrain<-1000
  for(j in 1:length(allFeatures)){
    currentFeatures<-selectedFeatures
    currentFeatures[length(selectedFeatures)+1]<-allFeatures[j]
    result<-CrossValidationAnnFeature(data=cargoships.norm, FeatureSet = currentFeatures)
    average<-mean(result[,3])
    if(average<minError){
      minError=average
      minindex=j
      maxError<-max(result[,6])
      shipType<-result[result$MaxError==maxError,5]
      hullno<-result[result$MaxError==maxError,4]
      averageErrorTrain<-mean(result[,2])
    }
  }
  selectedFeatures[length(selectedFeatures)+1]<-allFeatures[minindex]
  allFeatures<-allFeatures[-minindex]
  if(length(selectedFeatures)>1){ f <- as.formula(paste("Hull_Total_Res ~", paste(colnames(cargoships[,selectedFeatures]), collapse =  " + ")))
  }
  if(length(selectedFeatures)==1){ f <- as.formula(paste("Hull_Total_Res ~", colnames(cargoships[selectedFeatures[1]])))
  }
  FeatureSelectionSummary[i,1]<-i
  FeatureSelectionSummary[i,7]<-format(f)
  FeatureSelectionSummary[i,2]<-averageErrorTrain
  FeatureSelectionSummary[i,3]<-minError
  FeatureSelectionSummary[i,4]<-hullno
  FeatureSelectionSummary[i,5]<-shipType
  FeatureSelectionSummary[i,6]<-maxError
}


pairs.panels(cargoships[,c(32,14,23,21,7,20,13)],
             gap=0,
             pch = 21,cex.main=1.75, cex.lab=1.54, cex.axis=2) # ANN selected input relationship graph



plt_data<-test.data
set.seed(2021)
annAA<-neuralnet(Hull_Total_Res ~ Vs + AWS + CM + CB + LWL + HT + D,
                 data = train.data, hidden = 2, algorithm = "rprop+",
                 threshold = 0.5, lifesign = "full",
                 lifesign.step = 10, linear.output = TRUE,stepmax=1e7)
#plot(annAA)

test.data.N<-test.data[test.data$Model_No==382&test.data$Loading_Condition=="Design",]
test.data.N<-test.data
pr.nn.test <- compute(annAA,test.data.N)
pr.nn_test <- pr.nn.test$net.result
#pr.nn_test<-exp(pr.nn_test)
test.r <- test.data.N$Hull_Total_Res
xxx<-cbind(pr.nn_test,test.data)
MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test.data.N)
MSE.nn.test

plt_data$ANN<-pr.nn_test

plot(test.r,pr.nn_test,main = "ANN Results",xlab = "Hull Resistance", ylab = "Predicted Resistance")
abline(0,1)



# Mixed Effect Linear Model
lmer.cargoships<-lmer(log(Hull_Total_Res) ~ LWL + BWL + 
                        AWS + CP + CM + Bulb + Vs + 
                        I(Vs^2) + sqrt(Vs) + (1|Ind), 
                      data = train.data)
summary(lmer.cargoships)
AIC(lmer.cargoships)
plot(lmer.cargoships)


results.lmer<-(data.frame(exp(predict(lmer.cargoships,test.data, allow.new.levels = TRUE))))
colnames(results.lmer)<-"LMER"
results_data_lmer<-cbind(test.data,results.lmer)
results_data_lmer<-results_data_lmer[,c(1:39,41,42,43,44,45,40,54)]


plt_data$LMER<-results_data_lmer$LMER


MSE.lmer <- function(Ship_Type){
  MSE<-sum((results_data_lmer[results_data_lmer$Ship_Type==Ship_Type,45] - 
              results_data_lmer[results_data_lmer$Ship_Type==Ship_Type,46])^2)/nrow(results_data_lmer[results_data_lmer$Ship_Type==Ship_Type,])
  return(MSE)
}

MSE.Overall<-sum((results_data_lmer$Hull_Total_Res - results_data_lmer$LMER)^2)/nrow(results_data_lmer)

MSE.Tanker<-MSE.lmer("Tanker")
MSE.Container<-MSE.lmer("Container Ship")
MSE.BulkCarrier<-MSE.lmer("Bulk Carrier")
#MSE.Ferry<-MSE.lmer("Ferry")
MSE.GeneralCargo<-MSE.lmer("General Cargo Ship")
MSE.Summary<-data.frame(rbind(MSE.Tanker,MSE.Container,MSE.BulkCarrier,MSE.GeneralCargo,MSE.Overall))
colnames(MSE.Summary)<-"MSE"
MSE.Summary


plot(results_data_lmer$Hull_Total_Res,results_data_lmer$LMER,
     main = "Mixed Effect Linear Model Results",xlab = "Hull Resistance", ylab = "Predicted Resistance")
abline(0,1)

hist((results_data_lmer$Hull_Total_Res - results_data_lmer$LMER),breaks = 12,main = "Error Frequency",
     xlab = "Error")


# Standard Linear Model on The Same Dataset (15.05.2020 Comparison with ANN)
lm.wtf<-lm(log(Hull_Total_Res) ~ Ship_Type + LWL + N +
             CB + CM + CWP + Bulb + Number_of_Propeller + 
             Vs + I(Vs^2) + I(Vs^3)+ sqrt(Vs) + L_B_Ratio, 
           data=train.data) 
plot(lm.wtf,which = 1)
AIC(lm.wtf) 
summary(lm.wtf) 


results.lm.wtfr<-(data.frame(exp(predict(lm.wtf,test.data))))
colnames(results.lm.wtfr)<-"LMER"
train.data<-train.data[,-c(43,53)]
test.data<-test.data[,-c(45:53)]
results.lm.wtfr<-cbind(test.data,results.lm.wtfr)
results.lm.wtfr<-results.lm.wtfr[,c(1:39,41,42,43,44,40,45)]

plt_data$LM<-results.lm.wtfr$LMER

MSE.lm.wtf <- function(Ship_Type){
  MSE<-sum((results.lm.wtfr[results.lm.wtfr$Ship_Type==Ship_Type,44] - 
              results.lm.wtfr[results.lm.wtfr$Ship_Type==Ship_Type,45])^2)/nrow(results.lm.wtfr[results.lm.wtfr$Ship_Type==Ship_Type,])
  return(MSE)
}

MSE.Overall<-sum((results.lm.wtfr$Hull_Total_Res - results.lm.wtfr$LMER)^2)/nrow(results.lm.wtfr)

MSE.Tanker<-MSE.lm.wtf("Tanker")
MSE.Container<-MSE.lm.wtf("Container Ship")
MSE.BulkCarrier<-MSE.lm.wtf("Bulk Carrier")
#MSE.Ferry<-MSE.lmer("Ferry")
MSE.GeneralCargo<-MSE.lm.wtf("General Cargo Ship")
MSE.Summary<-data.frame(rbind(MSE.Tanker,MSE.Container,MSE.BulkCarrier,MSE.GeneralCargo,MSE.Overall))
colnames(MSE.Summary)<-"MSE"
MSE.Summary



# Final GLM Model
glm1<-glm(Hull_Total_Res ~ LWL + BWL + TA + AWS +
            AB + AT + CP + CM + CWP + 
            Vs + I(Vs^2) + I(Vs^3), 
          data=train.data,family = Gamma(link = "log"))
plot(glm1,which=1)
summary(glm1)


deneme<-data.frame(exp(predict(glm1,test.data)))
colnames(deneme)<-"Prediction"
deneme$Original<-test.data$Hull_Total_Res
deneme$dif<-deneme$Prediction-deneme$Original


plot(deneme$Original,deneme$Prediction)
abline(0,1)

mse.glm<-sum((deneme$Prediction-deneme$Original)^2)/nrow(test.data)
mse.glm

plt_data$GLM<-deneme$Prediction


ha<-data.frame(table(plt_data$Model_No))
d<-plt_data[plt_data$Model_No==ha[14,1] & plt_data$Loading_Condition=="Ballast",]
plot(d[,32],d[,40], col="blue",type = "p",
     xlab = "Speed (m/s)", ylab = "Hull Resistance (N)",main = "Predicted Hull Resistance")
lines(d[,32],d[,54],col="red", type = "p",pch=3) #ANN
lines(d[,32],d[,57],col="brown", type = "l") #GLM
lines(d[,32],d[,55],col="blue", type = "l") #MELM
lines(d[,32],d[,56],col="black", type = "l") #LM
lines(d[,32],d[,44],col="brown", type = "l") #Holtrop
legend(0.8, 16, legend=c("ANN", "MELM","LM","Holtrop"),
       col=c("red", "blue","black","brown"), lty=1:2, cex=0.6)

(sum((test.data$Holtrop-test.data$Hull_Total_Res)^2)/nrow(test.data))
sqrt(0.91)



## Final Generalized Linear Mixed Models ##

glmer.cargoships<-glmer(Hull_Total_Res ~ LWL + BWL + 
                          AWS + CB + CWP +
                          Bulb + Vs + sqrt(Vs) + (1|Ind), family = Gamma(link = "log"),
                        data = train.data)
summary(glmer.cargoships)
plot(glmer.cargoships)

# library(EnvStats)
# aa<-data.frame(predict(glm1,train.data))
# colnames(aa)<-"aa"
# egamma(aa$aa)
# cc<-glm(aa~1,family = Gamma,data=aa)
# summary(cc)

pairs.panels(cargoships[,c(7,8,14,21,24,30,32)],
             gap=0,
             pch = 21,cex.main=1.75, cex.lab=1.54, cex.axis=2) # GLMM selected input relationship graph


results.glmer<-(data.frame(exp(predict(glmer.cargoships,test.data, allow.new.levels = TRUE))))
colnames(results.glmer)<-"GLMER"
results_data_glmer<-cbind(test.data,results.glmer)
results_data_glmer<-results_data_glmer[,c(1:39,41,42,43,44,45,40,54)]


plt_data$LMER<-results_data_glmer$GLMER


MSE.glmer <- function(Ship_Type){
  MSE<-sum((results_data_glmer[results_data_glmer$Ship_Type==Ship_Type,45] - 
              results_data_glmer[results_data_glmer$Ship_Type==Ship_Type,46])^2)/nrow(results_data_glmer[results_data_glmer$Ship_Type==Ship_Type,])
  return(MSE)
}

MSE.Overall<-sum((results_data_glmer$Hull_Total_Res - results_data_glmer$GLMER)^2)/nrow(results_data_glmer)

MSE.Tanker<-MSE.glmer("Tanker")
MSE.Container<-MSE.glmer("Container Ship")
MSE.BulkCarrier<-MSE.glmer("Bulk Carrier")
#MSE.Ferry<-MSE.lmer("Ferry")
MSE.GeneralCargo<-MSE.glmer("General Cargo Ship")
MSE.Summary<-data.frame(rbind(MSE.Tanker,MSE.Container,MSE.BulkCarrier,MSE.GeneralCargo,MSE.Overall))
colnames(MSE.Summary)<-"MSE"
MSE.Summary
        



##### LEAVE ONE OUT CROSS VALIDATIONS #####

# Leave One-Out Cross Validation - ANN Final Model

CrossValidationLeaveOneOutANN<-function(data,nofNeuron,algo="rprop+",AF="logistic",repl=1,thresh=0.4){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  k=nrow(data_unique)
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    ann<-neuralnet(Hull_Total_Res ~ Vs + AWS + CM + CB + LWL + HT + D, 
                   data = train1.data, hidden = nofNeuron,
                   threshold = thresh, lifesign = "minimal", rep = repl,
                   algorithm = algo,act.fct = AF,
                   lifesign.step = 10, linear.output = TRUE,stepmax=1e7)
    
    ahm<-data.frame(t(ann$result.matrix))
    pr.nn.test <- compute(ann,test1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_test <- pr.nn.test$net.result
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn.train <- compute(ann,train1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_train <- pr.nn.train$net.result
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train

    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
    colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test","Test_Hull_No","Test_Ship_Type")
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_ann_myv)
}

set.seed(1)
LoutANN<-CrossValidationLeaveOneOutANN(cargoships.norm,nofNeuron = 2,thresh = 0.5)
mean(LoutANN$MSE_Test)

# Leave One-Out Cross Validation - MELM Final Model

CrossValidationLeaveOneOutMELM<-function(data){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  k=nrow(data_unique)
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    melm<-lmer(log(Hull_Total_Res) ~ LWL + BWL + 
                                  AWS + CP + CM + Bulb + Vs + 
                                  I(Vs^2) + sqrt(Vs) + (1|Ind), 
                                data = train1.data)
  
    pr.nn_test <- data.frame(exp(predict(melm,test1.data, allow.new.levels = TRUE)))
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn_train <- data.frame(exp(predict(melm,train1.data, allow.new.levels = TRUE)))
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_ann_myv)
}

set.seed(2021)
LoutMELM<-CrossValidationLeaveOneOutMELM(cargoships.norm)
mean(LoutMELM$MSE_Test)


# Leave One-Out Cross Validation - GLM Final Model

CrossValidationLeaveOneOutGLM<-function(data){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  k=nrow(data_unique)
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    glm1<-glm(Hull_Total_Res ~ LWL + BWL + TA + AWS +
                AB + AT + CP + CM + CWP + 
                Vs + I(Vs^2) + I(Vs^3), 
              data=train1.data,family = Gamma(link = "log"))
    
    pr.nn_test <- data.frame(exp(predict(glm1,test1.data)))
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn_train <- data.frame(exp(predict(glm1,train1.data)))
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_ann_myv)
}

set.seed(2021)
LoutGLM<-CrossValidationLeaveOneOutGLM(cargoships.norm)
mean(LoutGLM$MSE_Test)




# Leave One-Out Cross Validation - GMELM Final Model
CrossValidationLeaveOneOutGMELM<-function(data){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  k=nrow(data_unique)
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    melm<-glmer(Hull_Total_Res ~ LWL + BWL + 
                  AWS + CB + CWP +
                  Bulb + Vs + sqrt(Vs) + (1|Ind), family = Gamma(link = "log"), 
                data = train1.data)
    
    pr.nn_test <- data.frame(exp(predict(melm,test1.data, allow.new.levels = TRUE)))
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn_train <- data.frame(exp(predict(melm,train1.data, allow.new.levels = TRUE)))
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(result_total_ann_myv)
}

set.seed(2021)
LoutGMELM<-CrossValidationLeaveOneOutGMELM(cargoships.norm)
mean(LoutGMELM$MSE_Test)





### PRODUCING FINAL DATASET with ESTIMATIONS for FINAL GRAPHS ###

set.seed(2021)
RESULTSCrossValidationLeaveOneOutGLM<-function(data){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  k=nrow(data_unique)
  glmResult<-data.frame()
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    glm1<-glm(Hull_Total_Res ~ LWL + BWL + TA + AWS +
                AB + AT + CP + CM + CWP + 
                Vs + I(Vs^2) + I(Vs^3), 
              data=train1.data,family = Gamma(link = "log"))
    
    pr.nn_test <- data.frame(exp(predict(glm1,test1.data)))
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn_train <- data.frame(exp(predict(glm1,train1.data)))
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
    glmResult<-rbind(glmResult,pr.nn_test)
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(glmResult)
} # For final results function, GLM
GLMFinalResult<-RESULTSCrossValidationLeaveOneOutGLM(cargoships.norm)
colnames(GLMFinalResult)<-"GLM"

set.seed(2021)
RESULTSCrossValidationLeaveOneOutMELM<-function(data){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  melmResult<-data.frame()
  k=nrow(data_unique)
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    melm<-lmer(log(Hull_Total_Res) ~ LWL + BWL + 
                 AWS + CP + CM + Bulb + Vs + 
                 I(Vs^2) + sqrt(Vs) + (1|Ind), 
               data = train1.data)
    
    pr.nn_test <- data.frame(exp(predict(melm,test1.data, allow.new.levels = TRUE)))
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn_train <- data.frame(exp(predict(melm,train1.data, allow.new.levels = TRUE)))
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
    melmResult<-rbind(melmResult,pr.nn_test)
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(melmResult)
} # For final results function, MELM
MELMFinalResult<-RESULTSCrossValidationLeaveOneOutMELM(cargoships.norm)
colnames(MELMFinalResult)<-"MELM"

set.seed(1)
RESULTSCrossValidationLeaveOneOutANN<-function(data,nofNeuron,algo="rprop+",AF="logistic",repl=1,thresh=0.4){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  annResults<-data.frame()
  k=nrow(data_unique)
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    ann<-neuralnet(Hull_Total_Res ~ Vs + AWS + CM + CB + LWL + HT + D, 
                   data = train1.data, hidden = nofNeuron,
                   threshold = thresh, lifesign = "minimal", rep = repl,
                   algorithm = algo,act.fct = AF,
                   lifesign.step = 10, linear.output = TRUE,stepmax=1e7)
    
    ahm<-data.frame(t(ann$result.matrix))
    pr.nn.test <- neuralnet::compute(ann,test1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_test <- pr.nn.test$net.result
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn.train <- neuralnet::compute(ann,train1.data,rep = which(ahm$error==min(ahm$error)))
    pr.nn_train <- pr.nn.train$net.result
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
    colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test","Test_Hull_No","Test_Ship_Type")
    annResults<-rbind(annResults,pr.nn_test)
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(annResults)
} # For final results function, ANN A MELM
ANNFinalResult<-RESULTSCrossValidationLeaveOneOutANN(cargoships.norm,nofNeuron = 2,thresh = 0.5)
colnames(ANNFinalResult)<-"ANN"

set.seed(2021)
RESULTSCrossValidationLeaveOneOutGMELM<-function(data){
  data_unique<-data
  data_unique$merged<-paste(data_unique$Model_No,data_unique$Ship_Type,sep = ",")
  data_unique<-data.frame(unique(data_unique$merged))
  colnames(data_unique)<-"merged"
  data_unique<-data.frame(str_split_fixed(data_unique$merged, ",", 2))
  colnames(data_unique)<-c("Model_No","Ship_Type")
  k=nrow(data_unique)
  gmelm<-data.frame()
  result_total_ann_myv<-data.frame(matrix(nrow = k,ncol = 3))
  colnames(result_total_ann_myv)<-c("One_Out","MSE_Train","MSE_Test")
  result_total_ann_myv$One_Out<-1:k
  
  for (i in 1:k) {
    train1.data<-data[data$Model_No!=data_unique[i,1],]
    test1.data<-data[data$Model_No==data_unique[i,1],]
    melm<-glmer(Hull_Total_Res ~ LWL + BWL + 
                  AWS + CB + CWP +
                  Bulb + Vs + sqrt(Vs) + (1|Ind), family = Gamma(link = "log"), 
                data = train1.data)
    
    pr.nn_test <- data.frame(exp(predict(melm,test1.data, allow.new.levels = TRUE)))
    #pr.nn_test<-exp(pr.nn_test)
    test.r <- test1.data$Hull_Total_Res
    
    pr.nn_train <- data.frame(exp(predict(melm,train1.data, allow.new.levels = TRUE)))
    #pr.nn_train<-exp(pr.nn_train)
    train.r <- train1.data$Hull_Total_Res
    
    MSE.nn.test <- sum((test.r - pr.nn_test)^2)/nrow(test1.data)
    result_total_ann_myv[i,3]<-MSE.nn.test
    
    MSE.nn.train <- sum((train.r - pr.nn_train)^2)/nrow(train1.data)
    result_total_ann_myv[i,2]<-MSE.nn.train
    
    result_total_ann_myv[i,4]<-data_unique[i,1]
    result_total_ann_myv[i,5]<-data_unique[i,2]
    gmelm<-rbind(gmelm,pr.nn_test)
  }
  #return(mean(result_total_ann_myv$MSE_Test))
  return(gmelm)
}
GMELMFinalResult<-RESULTSCrossValidationLeaveOneOutGMELM(cargoships.norm)
colnames(GMELMFinalResult)<-"GMELM"


##### Final Dataset for Final Graphs #####
final_cargoships<-cbind(cargoships,GLMFinalResult,ANNFinalResult,MELMFinalResult,GMELMFinalResult)
final_cargoships<-final_cargoships[,-c(45:53,43)]
final_cargoships<-final_cargoships[,c(1:39,41,42,40,43:47)]
final_cargoships$Ind<-paste(final_cargoships$Model_No,final_cargoships$Ship_Type,
                               final_cargoships$Loading_Condition,sep = "-")

### MEan Absolute Error Calculation & Column Production ###
final_cargoships$RMSE_GLMM<-0
indices<-unique(final_cargoships$Ind)
for (i in 1:length(indices)) {
  aa<-final_cargoships[final_cargoships$Ind==indices[i],]
  final_cargoships[final_cargoships$Ind==indices[i],49]<-mean(sqrt((aa$Hull_Total_Res-aa$GMELM)^2))
}
final_cargoships$RMSE_GLMM<-round(final_cargoships$RMSE_GLMM,3)

final_cargoships$MAE_GLM<-0
for (i in 1:length(indices)) {
  aa<-final_cargoships[final_cargoships$Ind==indices[i],]
  final_cargoships[final_cargoships$Ind==indices[i],50]<-mean(sqrt((aa$Hull_Total_Res-aa$GLM)^2))
}
final_cargoships$MAE_GLM<-round(final_cargoships$MAE_GLM,3)

final_cargoships$MAE_ANN<-0
for (i in 1:length(indices)) {
  aa<-final_cargoships[final_cargoships$Ind==indices[i],]
  final_cargoships[final_cargoships$Ind==indices[i],51]<-mean(sqrt((aa$Hull_Total_Res-aa$ANN)^2))
}
final_cargoships$MAE_ANN<-round(final_cargoships$MAE_ANN,3)

final_cargoships$MAE_MELM<-0
for (i in 1:length(indices)) {
  aa<-final_cargoships[final_cargoships$Ind==indices[i],]
  final_cargoships[final_cargoships$Ind==indices[i],52]<-mean(sqrt((aa$Hull_Total_Res-aa$MELM)^2))
}
final_cargoships$MAE_MELM<-round(final_cargoships$MAE_MELM,3)


cc<-final_cargoships[final_cargoships$Ship_Type=="Tanker" &
                       final_cargoships$Loading_Condition=="Design",]
mean(cc$RMSE_GLMM)

# Bulk Carrier
setwd("C:/Users/ahmet.pala/Desktop/Ahmet Pala - Personal/M.S. Thesis/Final_Plots_Last/Bulk_Carrier")

data<-final_cargoships
data<-data[data$Ship_Type=="Bulk Carrier",]
Labels<-data.frame(unique(data$Ind))
for (i in 1:nrow(Labels)){
  aa<-data[data$Ind==Labels[i,1],]
  # Producing plot
  main1<-unique(aa$Ind)
  main2<-unique(aa$RMSE_GLMM)
  main<-paste(paste(main1,"MAE ",sep = "-"),main2)
  png(paste(main1,"png",sep = "."), width = 1000, height = 684)
  par(mar=c(5,6,4,1)+.1)
  plot(aa$Vs,(aa$Hull_Total_Res), ylab = "Hull Total Resistance (N)",xlab = "Speed (m/s)", main = main,
       ylim=range( c((aa$Hull_Total_Res), (aa$Holtrop),(aa$MELM), (aa$ANN),(aa$GMELM)) ),
       type = "p", pch = 20,
       col = "black", lty = 1, lwd = 1, cex.main=2, cex.lab=2.5, cex.axis=2.3)
  # Holtrop
  lines(aa$Vs,(aa$Holtrop), pch = 3, col = "red", type = "b", 
        lty = 2, lwd = 1)
  # ANN
  lines(aa$Vs,(aa$ANN), pch = 5, col = "orange", type = "b", 
        lty = 3, lwd = 1)
  # MELM
  lines(aa$Vs,(aa$MELM), pch = 4, col = "brown", type = "b", 
        lty = 3, lwd = 1)
  # GMELM
  lines(aa$Vs,(aa$GMELM), pch = 2, col = "blue", type = "b", 
        lty = 3, lwd = 1)
  # 4. Add a legend to the plot and set legend lty
  legend("topleft", legend = c("Experimental Results", "Holtrop","ANN","MELM","GLMM"),
         col = c("black","red","orange", "brown","blue"),pch = c(20,3,5,4,2), lty = 1:3, cex = 2)
  dev.off()
}

# Container Ship
setwd("C:/Users/ahmet.pala/Desktop/Ahmet Pala - Personal/M.S. Thesis/Final_Plots_Last/Container_Ship")

data<-final_cargoships
data<-data[data$Ship_Type=="Container Ship",]
Labels<-data.frame(unique(data$Ind))
for (i in 1:nrow(Labels)){
  aa<-data[data$Ind==Labels[i,1],]
  # Producing plot
  main1<-unique(aa$Ind)
  main2<-unique(aa$RMSE_GLMM)
  main<-paste(paste(main1,"MAE ",sep = "-"),main2)
  png(paste(main1,"png",sep = "."), width = 1000, height = 684)
  par(mar=c(5,6,4,1)+.1)
  plot(aa$Vs,(aa$Hull_Total_Res), ylab = "Hull Total Resistance (N)",xlab = "Speed (m/s)", main = main,
       ylim=range( c((aa$Hull_Total_Res), (aa$Holtrop),(aa$MELM), (aa$ANN),(aa$GMELM)) ),
       type = "p", pch = 20,
       col = "black", lty = 1, lwd = 1 , cex.main=2, cex.lab=2.5, cex.axis=2.3)
  # Holtrop
  lines(aa$Vs,(aa$Holtrop), pch = 3, col = "red", type = "b", 
        lty = 2, lwd = 1)
  # ANN
  lines(aa$Vs,(aa$ANN), pch = 5, col = "orange", type = "b", 
        lty = 3, lwd = 1)
  # MELM
  lines(aa$Vs,(aa$MELM), pch = 4, col = "brown", type = "b", 
        lty = 3, lwd = 1)
  # GMELM
  lines(aa$Vs,(aa$GMELM), pch = 2, col = "blue", type = "b", 
        lty = 3, lwd = 1)
  # 4. Add a legend to the plot and set legend lty
  legend("topleft", legend = c("Experimental Results", "Holtrop","ANN","MELM","GLMM"),
         col = c("black","red","orange", "brown","blue"),pch = c(20,3,5,4,2), lty = 1:3, cex = 2)
  dev.off()
}


# General Cargo Ship
setwd("C:/Users/ahmet.pala/Desktop/Ahmet Pala - Personal/M.S. Thesis/Final_Plots_Last/General_Cargo_Ship")

data<-final_cargoships
data<-data[data$Ship_Type=="General Cargo Ship",]
Labels<-data.frame(unique(data$Ind))
for (i in 1:nrow(Labels)){
  aa<-data[data$Ind==Labels[i,1],]
  # Producing plot
  main1<-unique(aa$Ind)
  main2<-unique(aa$RMSE_GLMM)
  main<-paste(paste(main1,"MAE ",sep = "-"),main2)
  png(paste(main1,"png",sep = "."), width = 1000, height = 684)
  par(mar=c(5,6,4,1)+.1)
  plot(aa$Vs,(aa$Hull_Total_Res), ylab = "Hull Total Resistance (N)",xlab = "Speed (m/s)", main = main,
       ylim=range( c((aa$Hull_Total_Res), (aa$Holtrop),(aa$MELM), (aa$ANN),(aa$GMELM)) ),
       type = "p", pch = 20,
       col = "black", lty = 1, lwd = 1, cex.main=2, cex.lab=2.5, cex.axis=2.3)
  # Holtrop
  lines(aa$Vs,(aa$Holtrop), pch = 3, col = "red", type = "b", 
        lty = 2, lwd = 1)
  # ANN
  lines(aa$Vs,(aa$ANN), pch = 5, col = "orange", type = "b", 
        lty = 3, lwd = 1)
  # MELM
  lines(aa$Vs,(aa$MELM), pch = 4, col = "brown", type = "b", 
        lty = 3, lwd = 1)
  # GMELM
  lines(aa$Vs,(aa$GMELM), pch = 2, col = "blue", type = "b", 
        lty = 3, lwd = 1)
  # 4. Add a legend to the plot and set legend lty
  legend("topleft", legend = c("Experimental Results", "Holtrop","ANN","MELM","GLMM"),
         col = c("black","red","orange", "brown","blue"),pch = c(20,3,5,4,2), lty = 1:3, cex = 2)
  dev.off()
}


# Tanker
setwd("C:/Users/ahmet.pala/Desktop/Ahmet Pala - Personal/M.S. Thesis/Final_Plots_Last/Tanker")

data<-final_cargoships
data<-data[data$Ship_Type=="Tanker",]
Labels<-data.frame(unique(data$Ind))
for (i in 1:nrow(Labels)){
  aa<-data[data$Ind==Labels[i,1],]
  # Producing plot
  main1<-unique(aa$Ind)
  main2<-unique(aa$RMSE_GLMM)
  main<-paste(paste(main1,"MAE ",sep = "-"),main2)
  png(paste(main1,"png",sep = "."), width = 1000, height = 684)
  par(mar=c(5,6,4,1)+.1)
  plot(aa$Vs,(aa$Hull_Total_Res), ylab = "Hull Total Resistance (N)",xlab = "Speed (m/s)", main = main,
       ylim=range( c((aa$Hull_Total_Res), (aa$Holtrop),(aa$MELM), (aa$ANN),(aa$GMELM)) ),
       type = "p", pch = 20,
       col = "black", lty = 1, lwd = 1, cex.main=2, cex.lab=2.5, cex.axis=2.3)
  # Holtrop
  lines(aa$Vs,(aa$Holtrop), pch = 3, col = "red", type = "b", 
        lty = 2, lwd = 1)
  # ANN
  lines(aa$Vs,(aa$ANN), pch = 5, col = "orange", type = "b", 
        lty = 3, lwd = 1)
  # MELM
  lines(aa$Vs,(aa$MELM), pch = 4, col = "brown", type = "b", 
        lty = 3, lwd = 1)
  # GMELM
  lines(aa$Vs,(aa$GMELM), pch = 2, col = "blue", type = "b", 
        lty = 3, lwd = 1)
  # 4. Add a legend to the plot and set legend lty
  legend("topleft", legend = c("Experimental Results", "Holtrop","ANN","MELM","GLMM"),
         col = c("black","red","orange", "brown","blue"),pch = c(20,3,5,4,2), lty = 1:3, cex = 2)
  dev.off()
}


# mean(final_cargoships[final_cargoships$Ship_Type=="Tanker",49])

### ABLINE of GMELM Results ###
plot(final_cargoships$Hull_Total_Res,final_cargoships$GMELM, ylab = "GLMM Results",xlab = "Ata Nutku Experiment Results",
     type = "p", pch = 20,
     col = "black", lty = 1, lwd = 1)
abline(0,1)

### Histogram of the errors in GMELM ###

Errors<-data.frame(final_cargoships$Hull_Total_Res - final_cargoships$MELM, title="Histogram of GLM Errors")
colnames(Errors)<-"MELM Errors"


ggplot(Errors, aes(x=`MELM Errors`)) + 
  geom_histogram(binwidth = 0.07,aes(y=..density..), colour="black", fill="gray")+
  geom_density(alpha=.2, fill="#FF6666") +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15,face="bold"))

ggplot(cargoships, aes(x=Hull_Total_Res)) + 
  geom_histogram(binwidth = 0.7,aes(y=..density..), colour="black", fill="gray")+
  geom_density(alpha=.2, fill="#FF6666")

######### Final Table for Thesis Production #############
FinalTable<-data.frame(matrix(ncol = 7,nrow = 12))
colnames(FinalTable)<-c("Ship_Type","Loading_Condition", "Holtrop", "MELM", "ANN", "GLM", "GMELM")
FinalTable$Loading_Condition<-c("Design", "Ballast", "Heavy Loaded")
FinalTable$Ship_Type<-rep(c("Bulk Carrier", "General Cargo Ship", "Container Ship", "Tanker"),each=3)

for (i in 1:nrow(FinalTable)){
  data<-final_cargoships[final_cargoships$Ship_Type==FinalTable[i,1] & 
                           final_cargoships$Loading_Condition==FinalTable[i,2],]
  FinalTable[i,3]<- mean(sqrt((data$Holtrop - data$Hull_Total_Res)^2)/data$Hull_Total_Res)
  FinalTable[i,4]<- mean(sqrt((data$MELM - data$Hull_Total_Res)^2)/data$Hull_Total_Res)
  FinalTable[i,5]<- mean(sqrt((data$ANN - data$Hull_Total_Res)^2)/data$Hull_Total_Res)
  FinalTable[i,6]<- mean(sqrt((data$GLM - data$Hull_Total_Res)^2)/data$Hull_Total_Res)
  FinalTable[i,7]<- mean(sqrt((data$GMELM - data$Hull_Total_Res)^2)/data$Hull_Total_Res)
}

colMeans(FinalTable[,c(3:7)])


cc<-final_cargoships[final_cargoships$Ship_Type=="Bulk Carrier" & 
                       final_cargoships$Loading_Condition=="Design",]
cc$ABS_Error<-sqrt((cc$Holtrop - cc$Hull_Total_Res)^2)
cc$Percentage<-cc$ABS_Error/cc$Hull_Total_Res
mean(cc$Percentage)


cc<-final_cargoships
cc$MAE_Holtrop<-0
for (i in 1:length(indices)) {
  aa<-cc[cc$Ind==indices[i],]
  cc[cc$Ind==indices[i],53]<-mean(sqrt((aa$Hull_Total_Res-aa$Holtrop)^2))
}
cc$MAE_Holtrop<-round(cc$MAE_Holtrop,3)
cc<-cc[,c(48:53)]
cc<-unique(cc)



aa<-data.frame((final_cargoships$GMELM - final_cargoships$Hull_Total_Res)/final_cargoships$Hull_Total_Res)

colnames(aa)<-"Relative_Errors"


ggplot(aa, aes(x=`Relative_Errors`)) + 
  geom_histogram(binwidth = 0.004,aes(y=..density..), colour="black", fill="gray")+
  geom_density(alpha=.2, fill="#FF6666") +
  theme(axis.text=element_text(size=15),axis.title=element_text(size=15,face="bold"))



######## SHIP TYPE AND LOADING CONDITION RANDOM INTERCEPT MODEL ##############


new_cargoships.norm<-cargoships.norm[cargoships.norm$Loading_Condition!="Ballast (Lighter)",]
new_cargoships.norm$Ship_Loading<-paste(new_cargoships.norm$Ship_Type,new_cargoships.norm$Loading_Condition,sep = "-")
new_train.data<-train.data
new_train.data$Ship_Loading<-paste(new_train.data$Ship_Type,new_train.data$Loading_Condition,sep = "-")
new_test.data<-test.data[test.data$Loading_Condition!="Ballast (Lighter)",]
new_test.data$Ship_Loading<-paste(new_test.data$Ship_Type,new_test.data$Loading_Condition,sep = "-")



deneme<-glmer(Hull_Total_Res ~ LWL + BWL + Ts + TA + TF + N + D + AWS + AR + AA + AB + HB + AT + HT + 
                 CB + CP + CM + CWP + LCB + LCF + Bulb + Number_of_Propeller +Vs 
                 + (1|Ind), family = Gamma(link = "log"), 
             data = train.data)
summary(deneme)

deneme<-glm(Hull_Total_Res ~ LWL + BWL + Ts + TA + TF + N + D + AWS + AR + AA + AB + HB + AT + HT + 
                CB + CP + CM + CWP + LCB + LCF + Bulb + Number_of_Propeller + Vs, family = Gamma(link = "identity"), 
              data = new_train.data)
summary(deneme)


aa<-glm(deneme +1~1,family=Gamma(link = "log"))
summary(aa)

deneme<-glmer(Hull_Total_Res ~ LWL + BWL + AWS + AT + HT +
                CB + LCB + Number_of_Propeller + Vs + sqrt(Vs)
                + (1|Ship_Loading), family = Gamma(link = "log"),
            data = new_train.data)
summary(deneme)

AIC(deneme)
library(blmeco)

dispersion_glmer <- function(modelglmer){
  # computing  estimated scale  ( binomial model) following  D. Bates :
  # That quantity is the square root of the penalized residual sum of
  # squares divided by n, the number of observations, evaluated as:
  n <- length(resid(modelglmer))
  return(  sqrt( sum(c(resid(modelglmer),modelglmer@u) ^2) / n ) ) 
} 

cf<-(coef(aa))
cf<-cf$Ind
colnames(cf)<-c("int","Vs")

tr<-glm(int~1,family = Gamma,data = cf)
summary()

glmer.gaussian<-glmer(Hull_Total_Res ~ LWL + BWL + 
                          AWS + CB + CWP +
                          Bulb + Vs + sqrt(Vs) + (1|Ind), family = gaussian(link = "log"),
                        data = train.data)
summary(glmer.gaussian)
summary(glmer.cargoships)


#### Examples ####

## lmer with 4 different ship type ##

exp_lmer<-cargoships.norm[cargoships$Model_No==424 |
                            cargoships$Model_No==409 |
                            cargoships$Model_No==405 |
                            cargoships$Model_No==289 ,]
lmer_exp<-lmer(Hull_Total_Res~ Vs + (1|Ship_Type),data = exp_lmer)
summary(lmer_exp)


## glmer with 4 different ship type ##

exp_glmer<-exp_lmer

glmer_exp<-glmer(Hull_Total_Res~ Vs + (1|Ship_Type),family=Gamma(),data = exp_glmer)
summary(glmer_exp)

aa<-final_cargoships[final_cargoships$Model_No==278 & final_cargoships$Loading_Condition=="Heavy Loaded",]
plot(aa$Vs,(aa$Hull_Total_Res), ylab = "Hull Total Resistance (N)",xlab = "Speed (m/s)", main = "278-Container Ship-Heavy Loaded-MAE 2.137",
     ylim=range( c((aa$Hull_Total_Res), (aa$Holtrop),(aa$MELM), (aa$ANN),(aa$GMELM)) ),
     type = "p", pch = 20,
     col = "black", lty = 1, lwd = 2, cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
# Holtrop
lines(aa$Vs,(aa$Holtrop), pch = 3, col = "red", type = "b", 
      lty = 2, lwd = 2)
# ANN
lines(aa$Vs,(aa$ANN), pch = 5, col = "orange", type = "b", 
      lty = 3, lwd = 2)
# MELM
lines(aa$Vs,(aa$MELM), pch = 4, col = "brown", type = "b", 
      lty = 3, lwd = 2)
# GMELM
lines(aa$Vs,(aa$GMELM), pch = 2, col = "blue", type = "b", 
      lty = 3, lwd = 2)
# 4. Add a legend to the plot and set legend lty
legend("topleft", legend = c("Experimental Results", "Holtrop","ANN","MELM","GLMM"),
       col = c("black","red","orange", "brown","blue"),pch = c(20,3,5,4,2), lty = 1:3, cex = 1.25)


