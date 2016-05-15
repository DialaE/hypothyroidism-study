## Code Dima Hypothyroidism Study
#-------------------------------------------------#
# Title: Hypothyroidism of Pregnant Women
# Dev:   Diala Ezzeddine
# Date:  30 March 2015
# Desc: Tester Hypo and not Hypo cases for pregnant women
# ChangeLog: (Who, When, What)
# <DEzzeddine,8 March 2016 , Study the Twins cases >
#-------------------------------------------------#


# data with Twins 
	
dataTwins=read.csv(file="/Users/dialaezzeddine/Desktop/Dima_Study/dataTABLEtwins.csv", head=TRUE);
colnames(dataTwins)=c("medical_record","study_num","age","Gravida","Parra","Abborta","Living","TSH","G_Age","G_Age_days" ,"Trim","ThreeTrim", "FT3","FT4", "T4_trt", "dose", "re_TSH", "re_TSH_level","re_TSH_GA", "re_TSH_GA_days","Abortion", "Ab_GA", "Ab_GA_days", "Pri_Term_Labor", "PTL_GA", "PTL_GA_days", "Pri_Term_Birth", "PTB_GA", "PTB_GA_days", "Intra_Uterine_Fetal_Death", "IUFD_GA" ,"IUFD_GA_days", "G_HyperTension", "G_Diabetes_Medical", "Normal_Vaginal_Delivery", "C_Section", "Induction", "GA_delivery", "GA_delivery_days", "Weight_gain_Kg", "BMI_LMP", "Apgar_1min", "Apgar_5min","Apgar_1minTwins", "Apgar_5minTwins", "New_Born_Weight", "New_Born_WeightTwins","PersonalHistory_Diabetes", "PersonalHistory_AI", "FamilyHistory_Diabetes", "FamilyHistory_TD", "Comments");

n1=nrow(dataTwins);
p1=ncol(dataTwins);

# without the comments variables and all the empty lignes and columns
dataTwins1<-data.frame(dataTwins[1:n1,1:(p1-4)]);

####################################################################################	
####################### Processing the dataTwins1 ##################################	
############Create the class Column(hypo not_hypo) in twins cases###################
 
# table formed of the TSH values and the pregnancy trimester (0=first trimester, 1= 2nd and 3rd trimester)
 Tab2<-matrix(nrow=n1,ncol=3,0);
 Tab2[,1]<-dataTwins1$TSH;
 Tab2[,2]<-dataTwins1$Trim;
 #Tab 
 colnames(Tab2)<-c("TSH","Trim","class")
 Tab3<-data.frame(Tab2);
 
for(i in 1:n1){
 	if(Tab3$TSH[i]>2.5 && Tab3$Trim[i]==0 || Tab3$TSH[i]>3 && Tab3$Trim[i]==1 ) Tab3$class[i]="Hypo" 
 	else Tab3$class[i]="Not_Hypo"
 }



#################Add the class column to dataTwins1 directly#################################
 #add an empty column to dataTwins1 
 dataTwins1$class<-0;
 #replace 0 in the class column with the real classification (hypo, not_hypo)
 dataTwins1$class<-Tab3$class;

 lTwins<-length(which(dataTwins1$class=="Hypo"))
 #number of hypo case with twins : 162
  
 lTwins1<-length(which(dataTwins1$class!="Hypo"))
 #number of not hypo with twins : 765

############################### Hypo and control data with twins#################################
# we use this data with twins just for the Birth weight and the apgar
# Hypo data adding the twins data
 HypoDataWithTwins<-dataTwins1[dataTwins1$class=="Hypo",];
 
#Control: Not_Hypo data adding the twins data
NotHypoDataWithTwins<-dataTwins1[dataTwins1$class!="Hypo",];


# number of hypo case with the twins
 l2<-nrow(HypoDataWithTwins);
 
# number of control case with twins 
 l3<-nrow(NotHypoDataWithTwins);
