# Load some of the package libraries we will need
# All packages are free from the R "CRAN" website
library(gdata) # for some data manipulation (like "subset")
library(foreign) # for importing from Stata format
library(rpart) # for CART trees
library(rpart.plot) # for pretty CART trees
library(randomForest) # for random forests (duh!)
library(nnet) # for ANNs
library(pROC) # for ROC curves
library(gbm) # for boosting regression tree predictor
library(adabag) # for boosting classification tree predictor
load(url("https://github.com/bangecon/Growth-MachineLearning/raw/master/GrowthDataImputed.RData"))
GrowthData.Full <- subset(GrowthData.rfImpute, Year > 1970)
GrowthData.Full <- subset(GrowthData.Full, !is.na(GDPpcGrowthMA))
GrowthData.Full <- subset(GrowthData.Full, !is.na(LagGDPpcGrowth))
GrowthData.Full <- subset(GrowthData.Full, !is.na(LagConflict))
GrowthData.Full$Recession <- factor(GrowthData.Full$Recession, levels = c(0,1), 
                                    labels = c("NoRecession", "Recession"))
GrowthData.Full$Recession5 <- factor(GrowthData.Full$Recession5, levels = c(0,1), 
                                     labels = c("NoRecession", "Recession"))

# Subset into learning ("in") sample and test ("out") sample
set.seed(8976) 
GrowthData.Full$rand <- runif(nrow(GrowthData.Full))
GrowthData.Full$randbar <- with(GrowthData.Full, ave(rand, wbcode, FUN = mean)) # Get the average by group wbcode (country)
GrowthData.Full$randbarp <- rank(GrowthData.Full$randbar)/length(GrowthData.Full$randbar) # Rank the random average by the length of total
GrowthData.LS <- subset(GrowthData.Full, randbarp <= 0.70) # Take 70% of data for training sample
GrowthData.TS <- subset(GrowthData.Full, randbarp >  0.70)
attach(GrowthData.LS)
Growth.lm <- lm(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                  LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                  LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                  LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                  LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                  LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                  LagLaborForceParticipation + LagFemaleLaborForce + 
                  LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                  LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                  LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                  LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                  LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                  LagRegimeInstab + LagTerror + LagProtest + LagSys_Parliamentary + 
                  LagSys_AssemblyElectedPresident + LagSys_Presidential + LagExec_Military + 
                  LagExec_Monarch + LagExec_Other + LagExec_Premier + LagExec_Premier + 
                  LagExec_President + LagReg_Civilian + LagReg_Military + 
                  LagReg_MilitaryCivilian + LagReg_Other)
set.seed(8976)
Growth.nnet <- nnet(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                      LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                      LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                      LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                      LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                      LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                      LagLaborForceParticipation + LagFemaleLaborForce + 
                      LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                      LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                      LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                      LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                      LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                      LagRegimeInstab + LagTerror + LagProtest + LagSys_Parliamentary + 
                      LagSys_AssemblyElectedPresident + LagSys_Presidential + LagExec_Military + 
                      LagExec_Monarch + LagExec_Other + LagExec_Premier + LagExec_Premier + 
                      LagExec_President + LagReg_Civilian + LagReg_Military + LagReg_Other + 
                      LagReg_MilitaryCivilian, size = 20, linout = T, MaxNWts = 1200)
Growth.tree <- rpart(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                       LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                       LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                       LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                       LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                       LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                       LagLaborForceParticipation + LagFemaleLaborForce + 
                       LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                       LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                       LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                       LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                       LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                       LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                       LagRegime, na.action = na.roughfix, model = TRUE, x = TRUE, y = TRUE, 
                     control = rpart.control(cp = 0.001, minsplit = 10))
set.seed(8976)
Growth.bag <- randomForest(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                             LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                             LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                             LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                             LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                             LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                             LagLaborForceParticipation + LagFemaleLaborForce + 
                             LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                             LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                             LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                             LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                             LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                             LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                             LagRegime, na.action = na.roughfix, mtry = 39, proximity = TRUE,  
                           nodesize = 10, localImp = TRUE, keep.forest = TRUE, sampsize = 3325)
set.seed(8976)
Growth.boost <- gbm(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                      LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                      LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                      LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                      LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                      LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                      LagLaborForceParticipation + LagFemaleLaborForce + 
                      LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                      LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                      LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                      LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                      LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                      LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                      LagRegime, distribution = "gaussian", n.trees = 500, interaction.depth = 3)
set.seed(8976)
Growth.rf <- randomForest(GDPpcGrowthMA ~ LagGDPpcGrowth + LagGDPpc + LagConsumptionGDP + LagInvestmentGDP + 
                            LagNetGovernmentGDP + LagMilitaryGDP + LagImportsGDP + 
                            LagIndustryGDP + LagAidAssistGDP + LagTradeGDP + LagExportsGDP + 
                            LagPrimCommodExports + LagTermsOfTrade + LagExportPrices + 
                            LagImportPrices + LagFDI_In_GDP + LagFDI_Out_GDP + LagPopulation + 
                            LagPopulationGrowth + LagLifeExpectancy + LagDependency + 
                            LagLaborForceParticipation + LagFemaleLaborForce + 
                            LagLifeExpectancy + LagSecEnrollRatePCT + LagSchoolExpendGDP + 
                            LagRuralPopulationPCT + LagMoneyGDP + LagMoneyGrowth + LagInflationCPI + 
                            LagRealInterestRate + LagLendingInterestRate + LagInterestSpread + 
                            LagPhonesPC + LagGini + LagEthnicConflict + LagNonethnicConflict + 
                            LagDemocracy + LagTransparency + LagCredibility + LagWithinInstab + 
                            LagRegimeInstab + LagTerror + LagProtest + LagSystem + LagExecutive + 
                            LagRegime, na.action = na.roughfix, proximity = TRUE, nodesize = 10,  
                          localImp = TRUE, keep.forest = TRUE, mtry = 3)
# Predicted Values
GrowthData.LS$GDPpcGrowthMA.lm <- predict(Growth.lm)
GrowthData.TS$GDPpcGrowthMA.lm <- predict(Growth.lm, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.nnet <- predict(Growth.nnet)
GrowthData.TS$GDPpcGrowthMA.nnet <- predict(Growth.nnet, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.tree <- predict(Growth.tree)
GrowthData.TS$GDPpcGrowthMA.tree <- predict(Growth.tree, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.bag <- predict(Growth.bag)
GrowthData.TS$GDPpcGrowthMA.bag <- predict(Growth.bag, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.boost <- predict(Growth.boost, n.trees = 500)
GrowthData.TS$GDPpcGrowthMA.boost <- predict(Growth.boost, newdata = GrowthData.TS, n.trees = 500)
GrowthData.LS$GDPpcGrowthMA.rf <- predict(Growth.rf)
GrowthData.TS$GDPpcGrowthMA.rf <- predict(Growth.rf, newdata = GrowthData.TS)
GrowthData.LS$GDPpcGrowthMA.ave <- (GrowthData.LS$GDPpcGrowthMA.lm + 
                                      GrowthData.LS$GDPpcGrowthMA.bag + 
                                      GrowthData.LS$GDPpcGrowthMA.boost + 
                                      GrowthData.LS$GDPpcGrowthMA.rf)/4 
GrowthData.TS$GDPpcGrowthMA.ave <- (GrowthData.TS$GDPpcGrowthMA.lm + 
                                      GrowthData.TS$GDPpcGrowthMA.bag + 
                                      GrowthData.TS$GDPpcGrowthMA.boost + 
                                      GrowthData.TS$GDPpcGrowthMA.rf)/4 
# Mean Squared Errors
MSE.LS <- mean((GDPpcGrowthMA-mean(GDPpcGrowthMA))^2)
MSE.TS <- mean((GrowthData.TS$GDPpcGrowthMA-mean(GrowthData.TS$GDPpcGrowthMA))^2)
MSE.lm.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.lm )^2)
MSE.lm.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.lm)^2)
MSE.nnet.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.nnet)^2)
MSE.nnet.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.nnet)^2)
MSE.tree.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.tree)^2)
MSE.tree.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.tree)^2)
MSE.bag.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.bag)^2)
MSE.bag.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.bag)^2)
MSE.boost.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.boost)^2)
MSE.boost.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.boost)^2)
MSE.rf.LS <- mean((GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.rf)^2)
MSE.rf.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.rf)^2)
MSE.ave.LS <- mean((GrowthData.LS$GDPpcGrowthMA-GrowthData.LS$GDPpcGrowthMA.ave)^2)
MSE.ave.TS <- mean((GrowthData.TS$GDPpcGrowthMA-GrowthData.TS$GDPpcGrowthMA.ave)^2)
MSE.Table <- cbind(rbind(MSE.lm.LS, MSE.nnet.LS, MSE.tree.LS, MSE.bag.LS, 
                         MSE.boost.LS, MSE.rf.LS, MSE.ave.LS, MSE.LS),
                   rbind(MSE.lm.TS, MSE.nnet.TS, MSE.tree.TS, MSE.bag.TS, 
                         MSE.boost.TS, MSE.rf.TS, MSE.ave.TS, MSE.TS))
colnames(MSE.Table) <- c("Learning Sample", "Test Sample")
# Importance
Importance.tree<-100*as.data.frame(Growth.tree$variable.importance)/
  sum(Growth.tree$variable.importance)
Importance.rf<-100*as.data.frame(Growth.rf$importance[,1])/ sum(Growth.rf$importance[,1])
Importance.bag<-as.data.frame(Growth.bag$importance[,1])
Importance.boost<-as.data.frame(summary(Growth.boost)[,2])
Importance.tree$id<-rownames(Importance.tree)
Importance.rf$id<-rownames(Importance.rf)
Importance.bag$id<-rownames(Importance.bag)
Importance.boost$id<-rownames(summary(Growth.boost))
Importance<-merge(Importance.tree, Importance.rf, by = "id", all = T)
Importance<-merge(Importance, Importance.bag, by = "id", all = T)
Importance<-merge(Importance, Importance.boost, by = "id", all = T)
Importance[is.na(Importance)]<-0
colnames(Importance)<-c("id", "Tree", "Forest", "Bagging", "Boosting")
rownames(Importance)<-Importance$id
Importance$Average<-(Importance$Tree + Importance$Forest + Importance$Bagging + 
                       Importance$Boosting)/4
Importance<-Importance[order(-Importance$Average),]
# Partial Dependence Plots
Categoricals<-c("LagEthnicConflict", "LagNonethnicConflict", "System", "Executive", "Regime")
Numericals<-rownames(Importance)[!rownames(Importance) %in% Categoricals]
source("partialPlotGBM.R")
for (i in seq_along(Numericals)) {
  pdp.bag<-partialPlot(Growth.bag, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "blue")
  lim.bag<-par('usr')
  pdp.boost<-partialPlotGBM(Growth.boost, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "green", n.trees = 500)
  lim.boost<-par('usr')
  pdp.rf<-partialPlot(Growth.rf, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), plot = T, col = "red")
  lim.rf<-par('usr')
  yLimMin<-min(lim.bag[3], lim.boost[3], lim.rf[3])
  yLimMax<-max(lim.bag[4], lim.boost[4], lim.rf[4])
  jpeg(filename = paste(Numericals[i], ".jpg"))
  partialPlot(Growth.bag, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), add = F, col = "blue",  ylim = c(yLimMin, yLimMax))  
  partialPlotGBM(Growth.boost, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), add = T, col = "green", n.trees = 500)
  partialPlot(Growth.rf, pred.data = GrowthData.LS, Numericals[i], xlab=Numericals[i], main = paste("Partial Dependence on", Numericals[i]), add = T, col = "red") 
  dev.off()
  legend("bottomright", c("Forest", "Boosting", "Bagging"), lty = c(1,1), lwd = 1, col = c("red","green","blue"))}
