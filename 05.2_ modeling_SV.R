##################### Now Snow Vole Model  ############################

library(flexsdm)
require(dplyr)


#loading model dataframe

df_SV <- read.csv("Data/to_model/df_SV_V3+bivars.csv")


#splitting the data frame into training and testing set
#indexes of the data.frame (80% of it)
set.seed(187)

ind <- sample(1:nrow(df_SV), 0.8*nrow(df_SV))

train <- df_SV[ind,]

test <- df_SV[-ind,]

write.csv(train, "Data/to_model/training_SV_V3.csv")

write.csv(test, "Data/to_model/testing_SV_V3.csv")


#### now models with the training set:

#creating the partition, which can be used to train a series of models

part_train <- part_random(
  data = train,
  pr_ab = "pr_ab",
  method = c(method = "rep_kfold", folds = 5, replicates = 10))




#### GBM fit ####

GBM_fit1 <- fit_gbm(
  part_train,
  response = "pr_ab",
  predictors= c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
  partition = ".part",
  thr= c("max_sens_spec"))


perf_result_gbm <- GBM_fit1$performance



#### RAF Fit ####

RAF_fit1 <- fit_raf(part_train,
                    response = "pr_ab",
                    predictors = c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_raf <- RAF_fit1$performance




#### SVM fit ####

SVM_fit1 <- fit_svm(part_train,
                    response = "pr_ab",
                    predictors = c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec")
)

perf_result_svm <- SVM_fit1$performance


##### Tuning of models #### 

##gbm ###

#grid for  GBM model tuning

tune_grid_gbm <- expand.grid(n.trees = c(20, 50, 100),
                             shrinkage = c(0.1, 0.5, 1),
                             n.minobsinnode = c(1, 3, 5, 7, 9)
)



gbm_tune <-
  tune_gbm(
    data = part_train,
    response = "pr_ab",
    predictors = c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                   "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                   "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
    partition = ".part",
    grid = tune_grid_gbm,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )

tuneperf_gbm <- gbm_tune$performance


hyper_gbmtune <- gbm_tune$hyper_performance


### random forest

tune_grid_raf <-
  expand.grid(mtry = seq(1, 7, 1))

rf_tune <-
  tune_raf(
    data = part_train,
    response = "pr_ab",
    predictors = c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                   "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                   "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
    partition = ".part",
    grid = tune_grid_raf,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )


hyper_rftune <- rf_tune$hyper_performance



### svm tune ###

tune_grid_svm <-
  expand.grid(
    C = c(2, 4, 8, 16, 20),
    sigma = c(0.01, 0.1, 0.2, 0.3, 0.4)
  )

svm_tune <-
  tune_svm(
    data = part_train,
    response = "pr_ab",
    predictors = c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                   "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                   "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
    partition = ".part",
    grid = tune_grid_svm,
    thr = "max_sens_spec",
    metric = "TSS",
    n_cores = 1
  )

hyper_svmtune <- svm_tune$hyper_performance



### GBM 2

hyper_gbmtune[which(hyper_gbmtune$TSS_mean == max(hyper_gbmtune$TSS_mean)),]

#tree:100
#shrink:1
#minobs: 1

GBM_fit2 <- fit_gbm(
  part_train,
  response = "pr_ab",
  predictors= c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
  partition = ".part",
  thr= c("max_sens_spec"),
  n_trees = 100,
  n_minobsinnode = 1,
  shrinkage = 1
)

perf_res_gbm2 <- GBM_fit2$performance

perf_res_gbm2$TSS_mean
perf_result_gbm$TSS_mean



### Random forest

hyper_rftune[which(hyper_rftune$TSS_mean == max(hyper_rftune$TSS_mean)),]

#

RAF_fit2 <- fit_raf(part_train,
                    response = "pr_ab",
                    predictors = c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    mtry = 2
)


perf_res_rf2 <- RAF_fit2$performance

perf_res_rf2$TSS_mean
perf_result_raf$TSS_mean


###SVM 


hyper_svmtune[which(hyper_svmtune$TSS_mean == max(hyper_svmtune$TSS_mean)),]

#c=20
#sigma=0.01

SVM_fit2 <- fit_svm(part_train,
                    response = "pr_ab",
                    predictors = c("BIO4",                    "BIO8",                     "BIO15",                    "BIO18" ,                   "BIO19" ,                  
                                   "aridityIndexThornthwaite",  "PETColdestQuarter"  ,      "PETDriestQuarter"  ,      
                                   "PETseasonality" ,          "PETWettestQuarter" ,       "WC_alt_lonlat" ,           "merged_LC"),
                    partition = ".part",
                    thr = c("max_sens_spec"),
                    sigma = 0.01,
                    C = 20
)

perf_res_svm2 <- SVM_fit2$performance

perf_res_svm2$TSS_mean
perf_result_svm$TSS_mean


###### tuning performances #### 

tuned_performance <- rbind(perf_res_gbm2, perf_res_rf2, perf_res_svm2)


####### Ensemble ##### 


SV_ensembled_v3 <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit2, SVM_fit2),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 

perf_ensemble <- SV_ensembled_v3$performance

write.csv(perf_ensemble, "Data/Models/perf_ensemble_SV_V3.csv")

save(SV_ensembled_v3, file = "Data/Models/SV_ensembled_V3.Rda")




## trying wo SVM

SV_ensembled_v3_woSVM <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit2),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 

perf_ensemble_woSVM <- SV_ensembled_v3_woSVM$performance


SV_ensembled_v3_SVM1 <- fit_ensemble(
  models = list(GBM_fit2, RAF_fit2, SVM_fit1),
  ens_method = "mean",
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS"
) 

perf_ensemble_SVM1 <- SV_ensembled_v3_SVM1$performance


#### best with SVM1




