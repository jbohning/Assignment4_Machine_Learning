setwd( "/Users/JessicaBohning/Documents/Data Science/Coursera_Course_Work/Course 8- Machine Learnings/PROJECT")


modelFit<-train(classe~.,data=nsv)
varImp(modelFit)
library(gridExtra)
plot(training)
p1<-qplot(training$var_roll_forearm,training$pitch_arm,col=training$classe)
p2<-qplot(training$var_roll_forearm,training$stddev_yaw_arm,col=training$classe)
p3<-qplot(training$var_roll_forearm,training$var_yaw_arm,col=training$classe)
p4<-qplot(training$var_roll_forearm,training$stddev_roll_forearm,col=training$classe)
p5<-qplot(training$var_roll_forearm,training$avg_pitch_forearm,col=training$classe)
p6<-qplot(training$var_roll_forearm,training$var_pitch_forearm,col=training$classe)
p7<-qplot(training$var_roll_forearm,training$avg_yaw_forearm,col=training$classe)
p8<-qplot(training$var_roll_forearm,training$stddev_yaw_forearm,col=training$classe)
p9<-qplot(training$var_roll_forearm,training$var_yaw_forearm,col=training$classe)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol=3)

modelFit<-train(classe~nsv_topten$rn[1]+nsv_topten$rn[2]+nsv_topten$rn[3]+
                        nsv_topten$rn[4]+nsv_topten$rn[5]+nsv_topten$rn[6]+
                        nsv_topten$rn[7]+nsv_topten$rn[8]+nsv_topten$rn[9]+
                        nsv_topten$rn[10],
                data=training_raw,
                na.action=na.pass)

#training<-training_raw[ , which(names(training_raw) %in% nsv_topten$rn)]
#training$classe<-training_raw$classe

number_of_nas<-data.frame(columns_names=c("pitch_arm","stddev_roll_forearm",
                                          "var_roll_forearm","stddev_yaw_forearm",
                                          "var_yaw_forearm","avg_pitch_forearm",
                                          "var_pitch_forearm","avg_yaw_forearm",
                                          "stddev_yaw_arm","var_yaw_arm"),
                          NA_Count=c(sum(is.na(training_raw$pitch_arm)),
                                     sum(is.na(training_raw$stddev_roll_forearm)),
                                     sum(is.na(training_raw$var_roll_forearm)),
                                     sum(is.na(training_raw$stddev_yaw_forearm)),
                                     sum(is.na(training_raw$var_yaw_forearm)),
                                     sum(is.na(training_raw$avg_pitch_forearm)),
                                     sum(is.na(training_raw$var_pitch_forearm)),
                                     sum(is.na(training_raw$avg_yaw_forearm)),
                                     sum(is.na(training_raw$stddev_yaw_arm)),
                                     sum(is.na(training_raw$var_yaw_arm))))


modelFit<-train(classe~pitch_arm+stddev_roll_forearm+var_roll_forearm+
                        stddev_yaw_forearm+var_yaw_forearm+avg_pitch_forearm+
                        var_pitch_forearm+avg_yaw_forearm+stddev_yaw_arm+
                        var_yaw_arm,
                data=training_raw,
                na.action=na.pass)        



na_count_original_test<-sapply(testing_raw, function (x) sum(is.na(x)))
na_count_test<-table(na_count_original_test)
na_count_test<-as.data.frame(na_count_test)                
#Turn NA counts into data frames
na_columns_test<-data.frame(na_count_original_test)
#Set row names as a column
na_columns_test<-setDT(na_columns_test, keep.rownames = TRUE)[]
#Eliminate non-zero rows
na_columns_test<-na_columns_test[na_columns_test$na_count_original==0,]
training<-training_raw[ , which(names(training_raw) %in% na_columns$rn)]                




Sys.time()            
test<-train(classe~pitch_arm+pitch_forearm+roll_arm+yaw_arm+yaw_forearm+
                    roll_forearm+pitch_dumbbell,data=training_data,method="rf",
            trControl = fitControl)            
Sys.time()