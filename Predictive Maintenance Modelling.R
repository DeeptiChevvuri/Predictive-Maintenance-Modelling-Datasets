# Environment Setup
#install.packages("AzureML")
library("AzureML") # Connect to Azure Machine Learning

#install.packages("dplyr")
library("dplyr") # Data munging functions

#install.packages("zoo")
library("zoo")   # Feature engineering rolling aggregates

#install.packages("data.table")
library("data.table") # Feature engineering

#install.packages("scales")
library("scales") # For time formatted axis

#install.packages("ggplot2")
library("ggplot2") # Graphics

#import telemetry data
telemetry = read.csv("C:/Deepti Chevvuri/Projects/Predictive Maintenance Modelling Dataset/telemetry.csv")

# format datetime field which comes in as.character
telemetry$datetime <- as.POSIXct(telemetry$datetime,
                                 format="%m/%d/%Y %I:%M:%S %p",
                                 tz="UTC")

cat("Total Number of telemetry records:", nrow(telemetry))
range(telemetry$datetime)
head(telemetry,10)
tail(telemetry,10)
summary(telemetry)


theme_set(theme_bw())  # theme for figures
options(repr.plot.width = 8, repr.plot.height = 6)

# plot to understand variation of voltage over time (sample machine 1 and machine 2)
ggplot(data = telemetry %>% filter(machineID %in% 1:2, 
                                   datetime > as.POSIXct("2015-01-01"),
                                   datetime < as.POSIXct("2015-02-01")),
       aes(x = datetime, y = volt, col = factor(machineID))) +
  geom_line(alpha = 0.5) +
  labs(y = "voltage", color = "machineID") +
  facet_wrap(~machineID, ncol=1)

# import errors dataset
errors <- read.csv("C:/Deepti Chevvuri/Projects/Predictive Maintenance Modelling Dataset/errors.csv")
# format datetime and errorID fields
errors$datetime <- as.POSIXct(errors$datetime,
                              format="%m/%d/%Y %I:%M:%S %p", 
                              tz="UTC")
errors$errorID <- as.factor(errors$errorID)

cat("Total Number of error records:",nrow(errors))
errors[c(1:5, nrow(errors)-4:1),]

# to understand the count of different  error types, to understand which error accurs more
options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(errors, aes(x = errorID)) + 
  geom_histogram(fill = "orange", stat="count") + 
  labs(title = "Errors by type", x = "error types")

# for thr first three machines, show the count of different errors
options(repr.plot.width = 6, repr.plot.height = 5)
ggplot(errors %>% filter(machineID < 4), 
       aes(x = errorID, fill = factor(machineID))) + 
  geom_histogram(color = "black", stat="count") + 
  labs(title = "MachineID errors by type", x = "error types", fill="MachineID")+
  facet_wrap(~machineID, ncol = 1)

# plot the different erros in machine 4 in the year 2015
ptions(repr.plot.width = 7, repr.plot.height = 5)
ggplot(errors %>% filter(machineID == 4), 
       aes(y = errorID, x = datetime)) + 
  geom_point(color = "black", alpha = 0.5) + 
  labs(title = "MachineID 4 errors", x = "Date")


# import  maintenance dataset
maint <- read.csv("C:/Deepti Chevvuri/Projects/Predictive Maintenance Modelling Dataset/maint.csv")
# format datetime and comp fields
maint$datetime <- as.POSIXct(maint$datetime,
                             format="%m/%d/%Y %I:%M:%S %p", 
                             tz="UTC")
maint$comp <- as.factor(maint$comp)

cat("Total number of maintenance records:", nrow(maint))
range(maint$datetime)
maint[c(1:5, nrow(maint)-4:0),]

# count of maintanace works done on each component
options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(maint, aes(x = comp)) + 
  geom_histogram(fill= "magenta", stat="count") +
  labs(title = "Component replacements", x = "component types")


# variation of number of maintanance works on each component in individual machines (sample upto machine 3)
options(repr.plot.width = 6, repr.plot.height = 8)
ggplot(maint %>% filter(machineID < 4), 
       aes(x = comp, fill = factor(machineID))) + 
  geom_histogram(color = "black", stat="count") +
  labs(title = "Component replacements", x = "component types", fill = "Machine ID")+
  facet_wrap(~machineID, ncol = 1)

#maintanace works done on machine 4 over a period of 2014 and 2015
options(repr.plot.width = 7, repr.plot.height = 5)
ggplot(maint %>% filter(machineID == 4), 
       aes(y = comp, x = datetime)) + 
  geom_point(color = "black", alpha = 0.5) + 
  labs(title = "MachineID 4 component replacements", x = "Date")

# import machines dataset
machines <-  read.csv("C:/Deepti Chevvuri/Projects/Predictive Maintenance Modelling Dataset/machines.csv")
# format model field
machines$model <- as.factor(machines$model)

cat("Total number of machines:", nrow(machines))
machines[c(1:5, nrow(machines)-4:0),]
summary(machines)

# the count of machines of a particular model and age
options(repr.plot.width = 8, repr.plot.height = 6)
ggplot(machines, aes(x = age, fill = model)) + 
  geom_histogram(color = "black") + 
  labs(title = "Machines", x = "age (years)") +
  facet_wrap(~model)

# import failures dataset
failures <- read.csv("C:/Deepti Chevvuri/Projects/Predictive Maintenance Modelling Dataset/failures.csv")

# format datetime and failure fields
failures$datetime <- as.POSIXct(failures$datetime,
                                format="%m/%d/%Y %I:%M:%S %p", 
                                tz="UTC")
failures$failure <- as.factor(failures$failure)

cat("Total number of failures:", nrow(failures))
failures[c(1:5, nrow(failures)-4:0),]

# couunt of failures of different component types
options(repr.plot.width = 5, repr.plot.height = 3)
ggplot(failures, aes(x = failure)) + 
  geom_histogram(fill = "red", stat="count") + 
  labs(title = "Failure distribution", x = "component type")

# count of failure in a particular component in machines (sample until machine 4)
options(repr.plot.width = 6, repr.plot.height = 6)
ggplot(failures %>% filter(machineID < 4),
       aes(x = failure, fill = factor(machineID))) + 
  geom_histogram(color = "black", stat="count") + 
  labs(title = "Failure distribution", x = "component type", fill = "MachineID") +
  facet_wrap(~machineID, ncol=1)










# calculate the rolling mean and rolling standard deviation 
# on the last 3 hour lag window (width=3), for every 3 hours (by=3)
# for each machine ID.
telemetrymean <- telemetry %>%
  arrange(machineID, datetime) %>% 
  group_by(machineID) %>%
  mutate(voltmean = rollapply(volt, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean = rollapply(rotate, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean = rollapply(pressure, width = 3, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean = rollapply(vibration, width = 3, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean, rotatemean, pressuremean, vibrationmean) %>%
  filter(!is.na(voltmean))%>% 
  ungroup()

head(telemetrymean)

telemetrysd <- telemetry %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd = rollapply(volt, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd = rollapply(rotate, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd = rollapply(pressure, width = 3, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd = rollapply(vibration, width = 3, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd, rotatesd, pressuresd, vibrationsd) %>%
  filter(!is.na(voltsd)) %>%
  ungroup()

head(telemetrysd)


# calculate the rolling mean and rolling standard deviation 
# on the last 24 hour lag window (width=24), for every 3 hours (by=3)
# for each machine ID.
telemetrymean_24hrs <- telemetry %>%
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltmean_24hrs = rollapply(volt, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         rotatemean_24hrs = rollapply(rotate, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         pressuremean_24hrs = rollapply(pressure, width = 24, FUN = mean, align = "right", fill = NA, by = 3),
         vibrationmean_24hrs = rollapply(vibration, width = 24, FUN = mean, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltmean_24hrs, rotatemean_24hrs, pressuremean_24hrs, vibrationmean_24hrs) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()

head(telemetrymean_24hrs)

telemetrysd_24hrs <- telemetry %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(voltsd_24hrs = rollapply(volt, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         rotatesd_24hrs = rollapply(rotate, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         pressuresd_24hrs = rollapply(pressure, width = 24, FUN = sd, align = "right", fill = NA, by = 3),
         vibrationsd_24hrs = rollapply(vibration, width = 24, FUN = sd, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, voltsd_24hrs, rotatesd_24hrs, pressuresd_24hrs, vibrationsd_24hrs) %>%
  filter(!is.na(voltsd_24hrs)) %>%
  ungroup()

head(telemetrysd_24hrs)


# merge columns of feature sets created earlier
telemetryfeat <- data.frame(telemetrymean, telemetrysd[,-c(1:2)]) 
telemetryfeat_24hrs <- data.frame(telemetrymean_24hrs, telemetrysd_24hrs[,-c(1:2)])
telemetryfeat <- telemetryfeat %>%
  left_join(telemetryfeat_24hrs, by = c("datetime", "machineID")) %>%
  filter(!is.na(voltmean_24hrs)) %>% 
  ungroup()

head(telemetryfeat)
summary(telemetryfeat)




# create a column for each error type
errorcount <- errors %>% select(datetime, machineID, errorID) %>% 
  mutate(error1 = as.integer(errorID == "error1"), 
         error2 = as.integer(errorID == "error2"),
         error3 = as.integer(errorID == "error3"),
         error4 = as.integer(errorID == "error4"),
         error5 = as.integer(errorID == "error5"))

# sum the duplicate errors in an hour
errorcount <- errorcount %>% 
  group_by(machineID,datetime)%>%
  summarise(error1sum = sum(error1), 
            error2sum = sum(error2), 
            error3sum = sum(error3), 
            error4sum = sum(error4), 
            error5sum = sum(error5)) %>%
  ungroup()

head(errorcount)

# align errors with telemetry datetime field
errorfeat <- telemetry %>% 
  select(datetime, machineID) %>%
  left_join(errorcount, by = c("datetime", "machineID"))

# replace missing values
errorfeat[is.na(errorfeat)] <- 0

head(errorfeat)
summary(errorfeat)


# count the number of errors of different types in the last 24 hours,  for every 3 hours
errorfeat <- errorfeat %>% 
  arrange(machineID, datetime) %>%
  group_by(machineID) %>%
  mutate(error1count = rollapply(error1sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error2count = rollapply(error2sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error3count = rollapply(error3sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error4count = rollapply(error4sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3),
         error5count = rollapply(error5sum, width = 24, FUN = sum, align = "right", fill = NA, by = 3)) %>%
  select(datetime, machineID, error1count, error2count, error3count, error4count, error5count) %>%
  filter(!is.na(error1count)) %>% 
  ungroup()

head(errorfeat)


# create a binary column for each component. 1 if replacement occured, 0 if not.
comprep <- maint %>% 
  select(datetime, machineID, comp) %>% 
  mutate(comp1 = as.integer(comp == "comp1"), 
         comp2 = as.integer(comp == "comp2"),
         comp3 = as.integer(comp == "comp3"),
         comp4 = as.integer(comp == "comp4")) %>%
  select(-comp)

head(comprep)



comprep <- as.data.table(comprep)
setkey(comprep, machineID, datetime)

# seperate different component type replacements into different tables
comp1rep <- comprep[comp1 == 1, .(machineID, datetime, lastrepcomp1 = datetime)]# component 1 replacements
comp2rep <- comprep[comp2 == 1, .(machineID, datetime, lastrepcomp2 = datetime)]# component 2 replacements
comp3rep <- comprep[comp3 == 1, .(machineID, datetime, lastrepcomp3 = datetime)]# component 3 replacements
comp4rep <- comprep[comp4 == 1, .(machineID, datetime, lastrepcomp4 = datetime)]# component 4 replacements

# use telemetry feature table datetime and machineID to be matched with replacements
compdate <- as.data.table(telemetryfeat[,c(1:2)]) 
setkey(compdate, machineID, datetime)

# data.table rolling match will attach the latest record from the component replacement tables 
# to the telemetry date time and machineID
comp1feat <- comp1rep[compdate[,.(machineID, datetime)],roll = TRUE] 
comp1feat$sincelastcomp1 <- as.numeric(difftime(comp1feat$datetime, comp1feat$lastrepcomp1, units = "days"))
comp2feat <- comp2rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp2feat$sincelastcomp2 <- as.numeric(difftime(comp2feat$datetime, comp2feat$lastrepcomp2, units = "days"))
comp3feat <- comp3rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp3feat$sincelastcomp3 <- as.numeric(difftime(comp3feat$datetime, comp3feat$lastrepcomp3, units="days"))
comp4feat <- comp4rep[compdate[,.(machineID, datetime)], roll = TRUE] 
comp4feat$sincelastcomp4 <- as.numeric(difftime(comp4feat$datetime, comp4feat$lastrepcomp4, units = "days"))

# merge all tables
compfeat <-data.frame(compdate, comp1feat[,.(sincelastcomp1)], comp2feat[,.(sincelastcomp2)],
                      comp3feat[,.(sincelastcomp3)],comp4feat[,.(sincelastcomp4)])

head(compfeat,10)




# telemetry and error features have the same datetime 
finalfeat <- data.frame(telemetryfeat, errorfeat[,-c(1:2)])

# merge with component features and machine features lastly
finalfeat <- finalfeat %>% 
  left_join(compfeat, by = c("datetime","machineID")) %>% 
  left_join(machines, by = c("machineID"))

head(finalfeat, 10)
cat("The final set of features are:",paste0(names(finalfeat), ","))


# left join final features with failures on machineID then mutate a column for datetime difference
# filter date difference for the prediction horizon which is 24 hours
labeled <- left_join(finalfeat, failures, by = c("machineID")) %>%
  mutate(datediff = difftime(datetime.y, datetime.x, units = "hours")) %>%
  filter(datediff <= 24, datediff >= 0)

# left join labels to final features and fill NA's with "none" indicating no failure
labeledfeatures <- left_join(finalfeat, 
                             labeled %>% select(datetime.x, machineID, failure),
                             by = c("datetime" = "datetime.x", "machineID")) %>%
  arrange(machineID,datetime)

levels(labeledfeatures$failure) <- c(levels(labeledfeatures$failure), "none")
labeledfeatures$failure[is.na(labeledfeatures$failure)]<-"none"
head(labeledfeatures)


head(labeledfeatures[labeledfeatures$failure == "comp4",], 16)





# split at 2015-08-01 01:00:00, to train on the first 8 months and test on last 4 months
# labelling window is 24 hours so records within 24 hours prior to split point are left out
trainingdata1 <- labeledfeatures[labeledfeatures$datetime < "2015-07-31 01:00:00",] 
testingdata1 <- labeledfeatures[labeledfeatures$datetime > "2015-08-01 01:00:00",]

tail(trainingdata1)
head(testingdata1)



# split at 2015-09-01 01:00:00, to train on the first 9 months and test on last 3 months
# labelling window is 24 hours so records within 24 hours prior to split point are left out
trainingdata2 <- labeledfeatures[labeledfeatures$datetime < "2015-08-31 01:00:00",] 
testingdata2 <- labeledfeatures[labeledfeatures$datetime > "2015-09-01 01:00:00",]

tail(trainingdata2)
head(testingdata2)



# split at 2015-10-01 01:00:00, to train on the first 10 months and test on last 2 months
# labelling window is 24 hours so records within 24 hours prior to split point are left out
trainingdata3 <- labeledfeatures[labeledfeatures$datetime < "2015-09-30 01:00:00",] 
testingdata3 <- labeledfeatures[labeledfeatures$datetime > "2015-10-01 01:00:00",]

tail(trainingdata3)
head(testingdata3)


#install.packages("gbm")
library(gbm)
# create the training formula 
trainformula <- as.formula(paste('failure',
                                 paste(names(labeledfeatures)[c(3:29)],collapse=' + '),
                                 sep=' ~ '))
trainformula

# train model on 3 splits
set.seed(1234)
gbm_model1 <- gbm(formula = trainformula, data = trainingdata1, 
                  distribution = "multinomial", n.trees = 50,
                  interaction.depth = 5, shrinkage = 0.1)
gbm_model2 <- gbm(formula = trainformula, data = trainingdata2, 
                  distribution = "multinomial", n.trees = 50,
                  interaction.depth = 5, shrinkage = 0.1)
gbm_model3 <- gbm(formula = trainformula, data = trainingdata3,
                  distribution = "multinomial", n.trees = 50,
                  interaction.depth = 5, shrinkage = 0.1)
# print relative influence of variables for 1st model as an example
summary(gbm_model1)



# label distribution after features are labeled - the class imbalance problem
ggplot(labeledfeatures, aes(x=failure)) + 
  geom_bar(fill="red") + 
  labs(title = "label distribution", x = "labels")

# define evaluate function
Evaluate<-function(actual=NULL, predicted=NULL, cm=NULL){
  if(is.null(cm)) {
    actual = actual[!is.na(actual)]
    predicted = predicted[!is.na(predicted)]
    f = factor(union(unique(actual), unique(predicted)))
    actual = factor(actual, levels = levels(f))
    predicted = factor(predicted, levels = levels(f))
    cm = as.matrix(table(Actual=actual, Predicted=predicted))
  }
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the classes
  q = colsums / n # distribution of instances over the predicted classes
  
  #accuracy
  accuracy = sum(diag) / n
  
  #per class
  recall = diag / rowsums
  precision = diag / colsums
  f1 = 2 * precision * recall / (precision + recall)
  
  #macro
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  
  #1-vs-all matrix
  oneVsAll = lapply(1 : nc,
                    function(i){
                      v = c(cm[i,i],
                            rowsums[i] - cm[i,i],
                            colsums[i] - cm[i,i],
                            n-rowsums[i] - colsums[i] + cm[i,i]);
                      return(matrix(v, nrow = 2, byrow = T))})
  
  s = matrix(0, nrow=2, ncol=2)
  for(i in 1:nc){s=s+oneVsAll[[i]]}
  
  #avg accuracy
  avgAccuracy = sum(diag(s))/sum(s)
  
  #micro
  microPrf = (diag(s) / apply(s,1, sum))[1];
  
  #majority class
  mcIndex = which(rowsums==max(rowsums))[1] # majority-class index
  mcAccuracy = as.numeric(p[mcIndex]) 
  mcRecall = 0*p;  mcRecall[mcIndex] = 1
  mcPrecision = 0*p; mcPrecision[mcIndex] = p[mcIndex]
  mcF1 = 0*p; mcF1[mcIndex] = 2 * mcPrecision[mcIndex] / (mcPrecision[mcIndex] + 1)
  
  #random accuracy
  expAccuracy = sum(p*q)
  #kappa
  kappa = (accuracy - expAccuracy) / (1 - expAccuracy)
  
  #random guess
  rgAccuracy = 1 / nc
  rgPrecision = p
  rgRecall = 0*p + 1 / nc
  rgF1 = 2 * p / (nc * p + 1)
  
  #rnd weighted
  rwgAccurcy = sum(p^2)
  rwgPrecision = p
  rwgRecall = p
  rwgF1 = p
  
  classNames = names(diag)
  if(is.null(classNames)) classNames = paste("C",(1:nc),sep="")
  
  return(list(
    ConfusionMatrix = cm,
    Metrics = data.frame(
      Class = classNames,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      MacroAvgPrecision = macroPrecision,
      MacroAvgRecall = macroRecall,
      MacroAvgF1 = macroF1,
      AvgAccuracy = avgAccuracy,
      MicroAvgPrecision = microPrf,
      MicroAvgRecall = microPrf,
      MicroAvgF1 = microPrf,
      MajorityClassAccuracy = mcAccuracy,
      MajorityClassPrecision = mcPrecision,
      MajorityClassRecall = mcRecall,
      MajorityClassF1 = mcF1,
      Kappa = kappa,
      RandomGuessAccuracy = rgAccuracy,
      RandomGuessPrecision = rgPrecision,
      RandomGuessRecall = rgRecall,
      RandomGuessF1 = rgF1,
      RandomWeightedGuessAccurcy = rwgAccurcy,
      RandomWeightedGuessPrecision = rwgPrecision,
      RandomWeightedGuessRecall= rwgRecall,
      RandomWeightedGuessWeightedF1 = rwgF1)))
}



# evaluation metrics for first split
pred_gbm1 <- as.data.frame(predict(gbm_model1, testingdata1, 
                                   n.trees = 50,type = "response"))

names(pred_gbm1) <- gsub(".50", "", names(pred_gbm1))
pred_gbm1$failure <- as.factor(colnames(pred_gbm1)[max.col(pred_gbm1)])

eval1 <- Evaluate(actual=testingdata1$failure,predicted=pred_gbm1$failure)
eval1$ConfusionMatrix
t(eval1$Metrics)


# evaluation metrics for second split
pred_gbm2 <- as.data.frame(predict(gbm_model2, testingdata2,  
                                   n.trees = 50,type = "response"))

names(pred_gbm2) <- gsub(".50", "", names(pred_gbm2))
pred_gbm2$failure <- as.factor(colnames(pred_gbm2)[max.col(pred_gbm2)])

eval2 <- Evaluate(actual=testingdata2$failure,predicted=pred_gbm2$failure)
eval2$ConfusionMatrix
t(eval2$Metrics)

# evaluation metrics for third split
pred_gbm3 <- as.data.frame(predict(gbm_model3, testingdata3,  
                                   n.trees = 50,type = "response"))

names(pred_gbm3)<-gsub(".50", "", names(pred_gbm3))
pred_gbm3$failure <- as.factor(colnames(pred_gbm3)[max.col(pred_gbm3)])

eval3 <- Evaluate(actual=testingdata3$failure,predicted=pred_gbm3$failure)
eval3$ConfusionMatrix
t(eval3$Metrics)




# report the recall rates for the models
rownames <- c("comp1","comp2","comp3","comp4","none")
rownames
data.frame(cbind(failure = rownames,
                 gbm_model1_Recall = eval1$Metrics$Recall,
                 gbm_model2_Recall = eval2$Metrics$Recall,
                 gbm_model3_Recall = eval3$Metrics$Recall))

