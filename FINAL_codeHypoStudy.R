## Code Dima Hypothyroidism Study
#-------------------------------------------------#
# Title: Hypothyroidism of Pregnant Women
# Dev:   Diala Ezzeddine
# Date:  30 March 2015
# Desc: Tester Hypo and not Hypo cases for pregnant women
# ChangeLog: (Who, When, What)
# <DEzzeddine, , >
#-------------------------------------------------#


#-- Data --# 
# declare variables and constants
# Read data
################################################################################
################################ Read data######################################
################################################################################

source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/funct.r");
#source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/FINAL_codeHypoStudyTwins.r");


data=read.csv(file="/Users/dialaezzeddine/Desktop/Dima_Study/dataTABLE.csv", head=TRUE);
colnames(data)=c("medical_record","study_num","age","Gravida","Parra","Abborta","Living","TSH","G_Age","G_Age_days" ,"Trim","ThreeTrim", "FT3","FT4", "T4_trt", "dose", "re_TSH", "re_TSH_level","re_TSH_GA", "re_TSH_GA_days","Abortion", "Ab_GA", "Ab_GA_days", "Pri_Term_Labor", "PTL_GA", "PTL_GA_days", "Pri_Term_Birth", "PTB_GA", "PTB_GA_days", "Intra_Uterine_Fetal_Death", "IUFD_GA" ,"IUFD_GA_days", "G_HyperTension", "G_Diabetes_Medical", "Normal_Vaginal_Delivery", "C_Section", "Induction", "GA_delivery", "GA_delivery_days", "Weight_gain_Kg", "BMI_LMP", "Apgar_1min", "Apgar_5min","Apgar_1minTwins", "Apgar_5minTwins", "New_Born_Weight", "New_Born_WeightTwins","PersonalHistory_Diabetes", "PersonalHistory_AI", "FamilyHistory_Diabetes", "FamilyHistory_TD", "Comments");

n=nrow(data);
p=ncol(data);

# without the comments variables and all the empty lignes and columns
data1<-data.frame(data[1:n,1:(p-4)]);

length(which(data1$Trim==0))
#[1] 749
length(which(data1$Trim==1))
#[1] 171


# # for (i in 1:n){
	# # if(data1$G_Age_days[i]<=91){
		 # # data1$Trim[i]=0;
	   # # }
	# # else {
		# # data1$Trim[i]=1;
		  # # }
# # }


# #replace 1 by 0 and 2 by 1 of the trim variable in the data
 
 # for(i in 1:n){
 	# if(data1$Trim[i]==1) data1$Trim[i]=0 
 	# else data1$Trim[i]=1 
 	# }
 
 
 ##################################################################################	
############################ Processing the data1 ################################	
##############################Create the class Column(hypo not_hypo)##############
 	
# table formed of the TSH values and the pregnancy trimester (0=first trimester, 1= 2nd and 3rd trimester)
 Tab<-matrix(nrow=n,ncol=3,0);
 Tab[,1]<-data1$TSH;
 Tab[,2]<-data1$Trim;
 #Tab 
 colnames(Tab)<-c("TSH","Trim","class")
 Tab1<-data.frame(Tab);
 
for(i in 1:n){
 	if(Tab1$TSH[i]>2.5 && Tab1$Trim[i]==0 || Tab1$TSH[i]>3 && Tab1$Trim[i]==1 ) Tab1$class[i]="Hypo" 
 	else Tab1$class[i]="Not_Hypo"
 }
  # Tab1 now has a class column (hypo, not_hypo)

  
 #################Add the class column to data1 directly#################################
 
 #add an empty column to data1 
 data1$class<-0;
 #replace 0 in the class column with the real classification (hypo, not_hypo)
 data1$class<-Tab1$class;
 
  
 l<-length(which(data1$class=="Hypo"))
 #number of hypo case : 157
 
 l1<-length(which(data1$class!="Hypo"))
 #number of not hypo : 763



############################### Hypo data and control data#################################

#Hypo data
HypoData<-data1[data1$class=="Hypo",];
#Control: Not_Hypo data:
NotHypoData <-data1[data1$class!="Hypo",];
F<-HypoData$TSH;
F1<-NotHypoData$TSH;

	

