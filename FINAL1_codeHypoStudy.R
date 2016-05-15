## Code Dima Hypothyroidism Study
#-------------------------------------------------#
# Title: Hypothyroidism of Pregnant Women
# Dev:   Diala Ezzeddine
# Date:  23 February 2016
# Desc: Tester Hypo and not Hypo cases for pregnant women
# ChangeLog: (Who, When, What)
# <DEzzeddine, 23/2/2016, add the variable ThreeTrim to the data in order to study the 3 trimester and find out if the classification "class" is different based on 2 trimesters or 3 trimesters>
#-------------------------------------------------#
######### NO NEED TO RUN THIS CODE###############################################
###################### CODE BUILT FOR VERIFICATION ONLY##########################


#-- Data --# 

source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/FINAL_codeHypoStudy.r");
source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/funct.r");

# # seperate the 3 trimesters (1=first trimester, 2= 2nd trimester and 3= 3rd trimester)
# we already have the 3 trimesters separated in the dataTABLE Excel sheet. the variable called ThreeTrim 

# for (i in 1:n){
	# if(data1$G_Age_days[i]<=91){
		 # data1$Trim[i]=1;
		 # }
	# else {
		# if (data1$G_Age_days[i]>= 92 && data1$G_Age_days[i]<=182) {
		# data1$Trim[i]=2;
		  # }
	    # else {
		# data1$Trim[i]=3;
		  # }
		# }
# }

# number of cases
length(which(data1$ThreeTrim==1));
#[1] 749
length(which(data1$ThreeTrim==2))
#[1] 128
length(which(data1$ThreeTrim==3))
#[1] 43



#-- Processing --#
# perform tasks

##################################################################################	
############################ Processing the data1 based on the 3 trimesters ######	
##############################Create the class Column(hypo not_hypo)##############
 	
# table formed of the TSH values and the pregnancy 3 trimesters 
 TabT<-matrix(nrow=n,ncol=3,0);
 TabT[,1]<-data1$TSH;
 TabT[,2]<-data1$ThreeTrim;
 #Tab 
 colnames(TabT)<-c("TSH","ThreeTrim","class")
 TabT1<-data.frame(TabT);
 
for(i in 1:n){
 	if(TabT1$TSH[i]>2.5 && TabT1$ThreeTrim[i]==1 || TabT1$TSH[i]>3 && TabT1$ThreeTrim[i]==2 || TabT1$TSH[i]>3 && TabT1$ThreeTrim[i]==3  ) TabT1$class[i]="Hypo" 
 	else TabT1$class[i]="Not_Hypo"
 }
  # TabT1 now has a class column (hypo, not_hypo)

  
 #################Add the class column to data1 directly#################################
 
 #add an empty column to data1 
 data1$class1<-0;
 #replace 0 in the class1 column with the real classification (hypo, not_hypo)
 data1$class1<-TabT1$class;
 
  
 lT<-length(which(data1$class1=="Hypo"))
 #number of hypo case : 157
 
 lT1<-length(which(data1$class1!="Hypo"))
 #number of not hypo : 763

#####################verification##############################
#add an empty column to data1 
 data1$verif<-0;
for(i in 1:n){
	if(data1$class==data1$class1){
		data1$verif=0;
		}
	else {
		data1$verif=1;
		}
}

sum(data1$verif)
[1] 0

#that means that class and class1 are the same. which means that based on 2 trimesters #or the 3 trimesters the class is the same
#from now and on we will be using just the "class" variable
