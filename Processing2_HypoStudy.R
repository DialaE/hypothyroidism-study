## Code Dima Hypothyroidism Study
#-------------------------------------------------#
# Title: Hypothyroidism of Pregnant Women
# Dev:   Diala Ezzeddine
# Date:  23 February 2016
# Desc: Tester Hypo and not Hypo cases for pregnant women
# ChangeLog: (Who, When, What)
# <DEzzeddine, 23/2/2016, compare between the 3 trimester, use the (> 4 mIU/L) 
as cutoff above which all women are Hypo, compute the correlation among 
all variables>
#-------------------------------------------------#


source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/FINAL_codeHypoStudy.r");


#-- Presentation (I/O) --#
###############################################################################################
################################ Table7 ######################################################
###############################################################################################
#Compare between the 3 trimesters
#find the num of hypo and not hypo in each on one of 3 trimesters

Table7<-matrix(nrow=3, ncol=5,0)
rownames(Table7)<-c("First Trim","Second Trim", "Third Trim");
colnames(Table7)<-c("Hypothyroid group (num)", " Hypothyroid group (%)", "Control group (num)", "Control group (%)", "Total");


#Table7
#Num
 Table7[1,1]<-length(which(HypoData$ThreeTrim==1));
 Table7[1,3]<-length(which(NotHypoData$ThreeTrim==1));
 Table7[1,2]<-length(which(HypoData$ThreeTrim==1))/n*100;
 Table7[1,4]<-length(which(NotHypoData$ThreeTrim==1))/n*100;
 Table7[1,5]<-length(which(data1$ThreeTrim==1));
 
 
 Table7[2,1]<-length(which(HypoData$ThreeTrim==2));
 Table7[2,3]<-length(which(NotHypoData$ThreeTrim==2));
 Table7[2,2]<-length(which(HypoData$ThreeTrim==2))/n*100;
 Table7[2,4]<-length(which(NotHypoData$ThreeTrim==2))/n*100;
 Table7[2,5]<-length(which(data1$ThreeTrim==2));
 
 
 Table7[3,1]<-length(which(HypoData$ThreeTrim==3));
 Table7[3,3]<-length(which(NotHypoData$ThreeTrim==3));
 Table7[3,2]<-length(which(HypoData$ThreeTrim==3))/n*100;
 Table7[3,4]<-length(which(NotHypoData$ThreeTrim==3))/n*100;
 Table7[3,5]<-length(which(data1$ThreeTrim==3));
 
 Table7;
 
 write.csv(Table7, file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Table7.csv");
 #-- Presentation (I/O) --#
###############################################################################################
################################ Correlations######################################################
###############################################################################################

#correlation between all the variables (removing all the twins variables, GA's )
NumericData<-data1[,-c(1,2,9,10,16:20,22,23,25,26,28,29,31,32,38,39,44,45,47,52)];
cor(NumericData);
#or
#NumericVar<-sapply(data1,is.numeric);
#cor(data1[,NumericVar]);
#or
#cor(data1[,unlist(lapply(data1, is.numeric))]) 


#correlation between all the variables when Hypo (removing all the twins variables, GA's and the all 0 varaiables)
NumericHypoData<-HypoData[,-c(1,2,9,10,16:20,22,23,25,26,28,29,31,32,38,39,44,45,47,48,49,52)];
cor(NumericHypoData);

#correlation between all the variables when NotHypo (removing all the twins variables, GA's )
NumericNotHypoData<-NotHypoData[,-c(1,2,9,10,16:20,22,23,25,26,28,29,31,32,38,39,44,45,47,52)];
cor(NumericNotHypoData);



#Export data Excel/csv
write.csv(cor(NumericData), file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/correlation.csv");
write.csv(cor(NumericHypoData), file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Hypocorrelation.csv");
write.csv(cor(NumericNotHypoData), file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/NotHypocorrelation.csv")




#-- Presentation (I/O) --#
###############################################################################################
################################ Correlations per trimester####################################
###############################################################################################

Trim1Data<-data1[data1$ThreeTrim==1,];
NumericTrim1Data<-Trim1Data[,-c(1,2,9,10:12,16:20,22,23,25,26,28,29,31,32,38,39,44,45,47,52)];
cor(NumericTrim1Data);

Trim2Data<-data1[data1$ThreeTrim==2,];
NumericTrim2Data<-Trim2Data[,-c(1,2,9,10:12,16:20,22,23,25,26,28,29,30:32,38,39,44,45,47,49,52)];
cor(NumericTrim2Data);


Trim3Data<-data1[data1$ThreeTrim==3,];
NumericTrim3Data<-Trim3Data[,-c(1,2,9,10:12,16:22,23,25,26,28,29,30:32,37,38,39,44,45,47,49,52)];
cor(NumericTrim3Data);



#Export data Excel/csv
write.csv(cor(NumericTrim1Data), file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Trim1correlation.csv");
write.csv(cor(NumericTrim2Data), file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Trim2correlation.csv");
write.csv(cor(NumericTrim3Data), file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Trim3correlation.csv")





