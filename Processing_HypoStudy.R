## Code Dima Hypothyroidism Study
#-------------------------------------------------#
# Title: Hypothyroidism of Pregnant Women
# Dev:   Diala Ezzeddine
# Date:  30 March 2015
# Desc: Tester Hypo and not Hypo cases for pregnant women
# ChangeLog: (Who, When, What)
# <DEzzeddine, 8 March 2016 , Results: fill the values in the 3 tables >
#-------------------------------------------------#



#-- Processing --#
# perform tasks

source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/funct.r");
source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/FINAL_codeHypoStudyTwins.r");
source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/FINAL_codeHypoStudy.r");



 
#-- Presentation (I/O) --#
###############################################################################################
################################ Table1 ######################################################
###############################################################################################

Table1<-matrix(nrow=16, ncol=9,0)
rownames(Table1)<-c("N","Age<=18 years", "Age>=35 years", "Nulligravidity", "Multigravidity", "Hx of >=1 abortion" ,"GDM" , "GHTN" ,"BMI at LMP<25" , "BMI at LMP between 25 and 30" , "BMI at LMP between 30 and 35" , "BMI at LMP>=35", "Hx of DM", "Hx of AI", "FHx of TD", "FHx of DM");
colnames(Table1)<-c("Hypothyroid group (num)", " Hypothyroid group (%)", "Control group (num)", "Control group (%)", " pearson's Chi-square test(p-value)","Fisher test(p-value)", "odd Ratio","Lower","Upper");


#Table1
#Num
 Table1[1,1]<-l;
 Table1[1,3]<-l1;
 Table1[1,2]<-l/n*100;
 Table1[1,4]<-l1/n*100;
 #age
 Table1[2,1]<-length(which(HypoData$age<=18));
 Table1[2,3]<-length(which(NotHypoData$age<=18));
 Table1[2,2]<-length(which(HypoData$age<=18))/l*100;
 Table1[2,4]<-length(which(NotHypoData$age<=18))/l1*100;
 
 
 Table1[3,1]<-length(which(HypoData$age>=35));
 Table1[3,3]<-length(which(NotHypoData$age>=35));
 Table1[3,2]<-length(which(HypoData$age>=35))/l*100;
 Table1[3,4]<-length(which(NotHypoData$age>=35))/l1*100;
#nulliparous
 Table1[4,1]<-length(which(HypoData$Gravida==1));
 Table1[4,3]<-length(which(NotHypoData$Gravida==1));
 Table1[4,2]<-length(which(HypoData$Gravida==1))/l*100;
 Table1[4,4]<-length(which(NotHypoData$Gravida==1))/l1*100;
 #multiparous
 Table1[5,1]<-length(which(HypoData$Gravida!=1));
 Table1[5,3]<-length(which(NotHypoData$Gravida!=1));
 Table1[5,2]<-length(which(HypoData$Gravida!=1))/l*100;
 Table1[5,4]<-length(which(NotHypoData$Gravida!=1))/l1*100;
#Abborta
 Table1[6,1]<-length(which(HypoData$Abborta>=1));
 Table1[6,3]<-length(which(NotHypoData$Abborta>=1));
 Table1[6,2]<-length(which(HypoData$Abborta>=1))/l*100;
 Table1[6,4]<-length(which(NotHypoData$Abborta>=1))/l1*100;
#G_Diabetes_Medical
 Table1[7,1]<-length(which(HypoData$G_Diabetes_Medical==1));
 Table1[7,3]<-length(which(NotHypoData$G_Diabetes_Medical==1));
 Table1[7,2]<-length(which(HypoData$G_Diabetes_Medical==1))/l*100;
 Table1[7,4]<-length(which(NotHypoData$G_Diabetes_Medical==1))/l1*100;
#G_HyperTension
 Table1[8,1]<-length(which(HypoData$G_HyperTension==1));
 Table1[8,3]<-length(which(NotHypoData$G_HyperTension==1));
 Table1[8,2]<-length(which(HypoData$G_HyperTension==1))/l*100;
 Table1[8,4]<-length(which(NotHypoData$G_HyperTension==1))/l1*100;
#BMI_LMP
 Table1[9,1]<-length(which(HypoData$BMI_LMP<25));
 Table1[9,3]<-length(which(NotHypoData$BMI_LMP<25));
 Table1[9,2]<-length(which(HypoData$BMI_LMP<25))/l*100;
 Table1[9,4]<-length(which(NotHypoData$BMI_LMP<25))/l1*100;
 
 BMImat<-matrix(nrow=l,ncol=3,0);
  BMImat[,1]<-HypoData$BMI_LMP;
  
  for(i in 1:l){
    if(BMImat[i,1]>=25 && BMImat[i,1]<30){
  	 BMImat[i,2]=1;	
        }else{
        BMImat[i,2]=0; 	
        }	
  }
   
 for(i in 1:l){
    if(BMImat[i,1]>=30 && BMImat[i,1]<35){
  	      BMImat[i,3]=1;
  	      }else{
        BMImat[i,3]=0; 	
        }	
  }

 
 BMImat1<-matrix(nrow=l1,ncol=3,0);
  BMImat1[,1]<-NotHypoData$BMI_LMP;
  
  for(i in 1:l1){
    if(BMImat1[i,1]>=25 && BMImat1[i,1]<30){
  	 BMImat1[i,2]=1;	
        }else{
        BMImat1[i,2]=0; 	
        }	
  }
   
 for(i in 1:l1){
    if(BMImat1[i,1]>=30 && BMImat1[i,1]<35){
  	      BMImat1[i,3]=1;
  	      }else{
        BMImat1[i,3]=0; 	
        }	
  }

 
 Table1[10,1]<-length(which(BMImat[,2]==1));
 Table1[10,3]<-length(which(BMImat1[,2]==1));
 Table1[10,2]<-length(which(BMImat[,2]==1))/l*100;
 Table1[10,4]<-length(which(BMImat1[,2]==1))/l1*100;
 
 Table1[11,1]<-length(which(BMImat[,3]==1));
 Table1[11,3]<-length(which(BMImat1[,3]==1));
 Table1[11,2]<-length(which(BMImat[,3]==1))/l*100;
 Table1[11,4]<-length(which(BMImat1[,3]==1))/l1*100;

 Table1[12,1]<-length(which(HypoData$BMI_LMP>=35));
 Table1[12,3]<-length(which(NotHypoData$BMI_LMP>=35));
 Table1[12,2]<-length(which(HypoData$BMI_LMP>=35))/l*100;
 Table1[12,4]<-length(which(NotHypoData$BMI_LMP>=35))/l1*100;
 
 #PersonalHistory_Diabetes
 Table1[13,1]<-length(which(HypoData$	PersonalHistory_Diabetes==1));
 Table1[13,3]<-length(which(NotHypoData$PersonalHistory_Diabetes==1));
 Table1[13,2]<-length(which(HypoData$PersonalHistory_Diabetes==1))/l*100;
 Table1[13,4]<-length(which(NotHypoData$PersonalHistory_Diabetes==1))/l1*100;
 #PersonalHistory_AI
 Table1[14,1]<-length(which(HypoData$	PersonalHistory_AI==1));
 Table1[14,3]<-length(which(NotHypoData$PersonalHistory_AI==1));
 Table1[14,2]<-length(which(HypoData$PersonalHistory_AI==1))/l*100;
 Table1[14,4]<-length(which(NotHypoData$PersonalHistory_AI==1))/l1*100;
#FamilyHistory_TD
 Table1[15,1]<-length(which(HypoData$	FamilyHistory_TD==1));
 Table1[15,3]<-length(which(NotHypoData$FamilyHistory_TD==1));
 Table1[15,2]<-length(which(HypoData$	FamilyHistory_TD==1))/l*100;
 Table1[15,4]<-length(which(NotHypoData$FamilyHistory_TD==1))/l1*100;
 #FamilyHistory_Diabetes
 Table1[16,1]<-length(which(HypoData$	FamilyHistory_Diabetes==1));
 Table1[16,3]<-length(which(NotHypoData$FamilyHistory_Diabetes==1));
 Table1[16,2]<-length(which(HypoData$	FamilyHistory_Diabetes==1))/l*100;
 Table1[16,4]<-length(which(NotHypoData$FamilyHistory_Diabetes==1))/l1*100;

#tester la difference entre 2 proportions prop.test 
  Table1[1,5]<- prop.test(l,n)$p.value;
  
 # ##### pearson's Chi-square test
 for(i in 2:nrow(Table1)){
 	mym<-matrix(c(Table1[i,1],Table1[i,3],l-Table1[i,1],l1-Table1[i,3]),nrow=2,byrow=TRUE);
    #print(mym);
 	Table1[i,5]<-chisq.test(mym,simulate.p.value = TRUE,B=999)$p.value;
 }
 
 
 #tester la difference entre 2 proportions binom.test
Table1[1,6]<- binom.test(l,n)$p.value;

# Fisher Exact Test 
for(i in 2:nrow(Table1)){
 	mym1<-matrix(c(Table1[i,1],Table1[i,3],l-Table1[i,1],l1-Table1[i,3]),nrow=2,byrow=TRUE);
    #print(mym1);
 	Table1[i,6]<-fisher.test(mym1)$p.value;
 }
 
  # Odd Ratio 
  Table1[1,7]<- 0;
for(i in 2:nrow(Table1)){
 	mym2<-matrix(c(Table1[i,1],Table1[i,3],l-Table1[i,1],l1-Table1[i,3]),nrow=2,byrow=TRUE);
 	#print(mym2);
 	#Table1[i,7]<-OddRatio(mym2,alpha=0.05,quiet=F);
 	r<-OddRatio(mym2,alpha=0.05,quiet=F);
 	Table1[i,7]<-r[1];
 	Table1[i,8]<-r[2];
 	Table1[i,9]<-r[3];
 }
 

Table1;
write.csv(Table1, file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Table1.csv");

#############################################################################################
################################ Table2 ######################################################
##############################################################################################

Table2<-matrix(nrow=11, ncol=9,0)
rownames(Table2)<-c("Abortion 1st Trim","Abortion 2nd Trim", "IUFD", "PTL", "PTB", "C_Section" ,"Post-term Delivery>= 41 weeks" , "Birth Wt<10th%" ,"Birth Wt>90th%" , "Apgar score at 1 min<=7" , "Apgar score at 5 min<=7");
colnames(Table2)<-c("Hypothyroid group (num)", " Hypothyroid group (%)", "Control group (num)", "Control group (%)", " pearson's Chi-square test(p-value)","Fisher test(p-value)","Odd Ratio","Lower","Upper");


############################abbortion###################
Abb<-matrix(nrow=l,ncol=4,0);
Abb[,1]<-HypoData$Trim;
Abb[,2]<-HypoData$Abortion;
for(i in 1:l){
	if(Abb[i,1]==0 && Abb[i,2]!=0){
	  Abb[i,3]=1;
	}else{
		Abb[i,3]=0;
	}
	if(Abb[i,1]==1 && Abb[i,2]!=0){
	  Abb[i,4]=1;
	}else{
		Abb[i,4]=0;
	}

}
# abbortion Hypo
Table2[1,1]<-length(which(Abb[,3]==1));
Table2[2,1]<-length(which(Abb[,4]==1));

Table2[1,2]<-(length(which(Abb[,3]==1)))/l*100;
Table2[2,2]<-(length(which(Abb[,4]==1)))/l*100;

Abb1<-matrix(nrow=l1,ncol=4,0);
Abb1[,1]<-NotHypoData$Trim;
Abb1[,2]<-NotHypoData$Abortion;
for(i in 1:l1){
	if(Abb1[i,1]==0 && Abb1[i,2]!=0){
	  Abb1[i,3]=1;
	}else{
		Abb1[i,3]=0;
	}
	if(Abb1[i,1]==1 && Abb1[i,2]!=0){
	  Abb1[i,4]=1;
	}else{
		Abb1[i,4]=0;
	}

}
# Abbortion NotHypo
Table2[1,3]<-length(which(Abb1[,3]==1));
Table2[2,3]<-length(which(Abb1[,4]==1));

Table2[1,4]<-(length(which(Abb1[,3]==1)))/l1*100;
Table2[2,4]<-(length(which(Abb1[,4]==1)))/l1*100;

#####IUFD
 Table2[3,1]<-length(which(HypoData$Intra_Uterine_Fetal_Death==1));
 Table2[3,3]<-length(which(NotHypoData$Intra_Uterine_Fetal_Death==1));
 Table2[3,2]<-length(which(HypoData$Intra_Uterine_Fetal_Death==1))/l*100;
 Table2[3,4]<-length(which(NotHypoData$Intra_Uterine_Fetal_Death==1))/l1*100;

#####PTL
 Table2[4,1]<-length(which(HypoData$Pri_Term_Labor==1));
 Table2[4,3]<-length(which(NotHypoData$Pri_Term_Labor==1));
 Table2[4,2]<-length(which(HypoData$Pri_Term_Labor==1))/l*100;
 Table2[4,4]<-length(which(NotHypoData$Pri_Term_Labor==1))/l1*100;
 
 #####PTB
 Table2[5,1]<-length(which(HypoData$Pri_Term_Birth==1));
 Table2[5,3]<-length(which(NotHypoData$Pri_Term_Birth==1));
 Table2[5,2]<-length(which(HypoData$Pri_Term_Birth==1))/l*100;
 Table2[5,4]<-length(which(NotHypoData$Pri_Term_Birth==1))/l1*100;

 #####c_section
 Table2[6,1]<-length(which(HypoData$C_Section==1));
 Table2[6,3]<-length(which(NotHypoData$C_Section==1));
 Table2[6,2]<-length(which(HypoData$C_Section==1))/l*100;
 Table2[6,4]<-length(which(NotHypoData$C_Section==1))/l1*100;
 
  ##### Post-term Delivery>= 41 weeks
  PostD<-matrix(nrow=l,ncol=2,0);
  PostD[,1]<-HypoData$GA_delivery_days;
  for(i in 1:l){
  	if(PostD[i,1]>=287){
  		PostD[i,2]=1;
  	}else{
  		PostD[i,2]=0;
  	}
  }
  
  
  PostD1<-matrix(nrow=l1,ncol=2,0);
  PostD1[,1]<-NotHypoData$GA_delivery_days;
  for(i in 1:l1){
  	if(PostD1[i,1]>=287){
  		PostD1[i,2]=1;
  	}else{
  		PostD1[i,2]=0;
  	}
  }

  
 Table2[7,1]<-length(which(PostD[,2]==1));
 Table2[7,3]<-length(which(PostD1[,2]==1));
 Table2[7,2]<-length(which(PostD[,2]==1))/l*100;
 Table2[7,4]<-length(which(PostD1[,2]==1))/l1*100;

#####Apgar 1 min <=7
apgar1min<-matrix(nrow=l2,ncol=2,0);
apgar1min[,1]<-HypoDataWithTwins$Apgar_1min;
for(i in 1:l2){
  	if(apgar1min[i,1]<=7 && apgar1min[i,1]!=0){
  		apgar1min[i,2]=1;
  	}else{
  		apgar1min[i,2]=0;
  	}
  }

apgar1min1<-matrix(nrow=l3,ncol=2,0);
apgar1min1[,1]<-NotHypoDataWithTwins$Apgar_1min;
for(i in 1:l3){
  	if(apgar1min1[i,1]<=7 && apgar1min1[i,1]!=0){
  		apgar1min1[i,2]=1;
  	}else{
  		apgar1min1[i,2]=0;
  	}
  }

 Table2[10,1]<-length(which(apgar1min==1));
 Table2[10,3]<-length(which(apgar1min1==1));
 Table2[10,2]<-length(which(apgar1min==1))/l2*100;
 Table2[10,4]<-length(which(apgar1min1==1))/l3*100;
 
 #####Apgar 5 min <=7
apgar5min<-matrix(nrow=l2,ncol=2,0);
apgar5min[,1]<-HypoDataWithTwins$Apgar_5min;
for(i in 1:l2){
  	if(apgar5min[i,1]<=7 && apgar5min[i,1]!=0){
  		apgar5min[i,2]=1;
  	}else{
  		apgar5min[i,2]=0;
  	}
  }

apgar5min1<-matrix(nrow=l3,ncol=2,0);
apgar5min1[,1]<-NotHypoDataWithTwins$Apgar_5min;
for(i in 1:l3){
  	if(apgar5min1[i,1]<=7 && apgar5min1[i,1]!=0){
  		apgar5min1[i,2]=1;
  	}else{
  		apgar5min1[i,2]=0;
  	}
  }

 Table2[11,1]<-length(which(apgar5min==1));
 Table2[11,3]<-length(which(apgar5min1==1));
 Table2[11,2]<-length(which(apgar5min==1))/l2*100;
 Table2[11,4]<-length(which(apgar5min1==1))/l3*100;
 
 ##### NB Weight 
 #NB Weight for Hypo data with twins
 
 NBWt<-matrix(nrow=l2,ncol=5,0)
 NBWt[,1]<-HypoDataWithTwins$New_Born_Weight;
 NBWt[,2]<-HypoDataWithTwins$GA_delivery_days/7;
 NBWt[,3]<-as.integer(NBWt[,2]);
 
# 10th percentil case
for(i in 1:l2){
	if(NBWt[i,1]!=0 && (NBWt[i,1]<498 && NBWt[i,3]==24 || NBWt[i,1]<558 && NBWt[i,3]==25 || NBWt[i,1]<625 && NBWt[i,3]==26 || NBWt[i,1]<702 && NBWt[i,3]==27 || NBWt[i,1]<798 && NBWt[i,3]==28 || NBWt[i,1]<925 && NBWt[i,3]==29 || NBWt[i,1]<1085 && NBWt[i,3]==30 || NBWt[i,1]<1278 && NBWt[i,3]==31 || NBWt[i,1]<1495 && NBWt[i,3]==32 || NBWt[i,1]<1725 && NBWt[i,3]==33 || NBWt[i,1]<1950 && NBWt[i,3]==34 || NBWt[i,1]<2159 && NBWt[i,3]==35 || NBWt[i,1]<2354 && NBWt[i,3]==36 || NBWt[i,1]<2541 && NBWt[i,3]==37 || NBWt[i,1]<2714 && NBWt[i,3]==38 || NBWt[i,1]<2852 && NBWt[i,3]==39 || NBWt[i,1]<2929 && NBWt[i,3]==40 || NBWt[i,1]<2948 && NBWt[i,3]==41 || NBWt[i,1]<2948 && NBWt[i,3]==42)){
		NBWt[i,4]=1;
	}else{
	NBWt[i,4]=0;	
	}
}
 
 # 90th percentil case
 for(i in 1:l2){
	if(NBWt[i,1]!=0 && (NBWt[i,1]>977 && NBWt[i,3]==24 || NBWt[i,1]>1138 && NBWt[i,3]==25 || NBWt[i,1]>1362 && NBWt[i,3]==26 || NBWt[i,1]>1635 && NBWt[i,3]==27 || NBWt[i,1]>1977 && NBWt[i,3]==28 || NBWt[i,1]>2361 && NBWt[i,3]==29 || NBWt[i,1]>2710 && NBWt[i,3]==30 || NBWt[i,1]>2986 && NBWt[i,3]==31 || NBWt[i,1]>3200 && NBWt[i,3]==32 || NBWt[i,1]>3370 && NBWt[i,3]==33 || NBWt[i,1]>3502 && NBWt[i,3]==34 || NBWt[i,1]>3596 && NBWt[i,3]==35 || NBWt[i,1]>3668 && NBWt[i,3]==36 || NBWt[i,1]>3755 && NBWt[i,3]==37 || NBWt[i,1]>3867 && NBWt[i,3]==38 || NBWt[i,1]>3980 && NBWt[i,3]==39 || NBWt[i,1]>4060 && NBWt[i,3]==40 || NBWt[i,1]>4094 && NBWt[i,3]==41 || NBWt[i,1]>4098 && NBWt[i,3]==42)){
		NBWt[i,5]=1;
	}else{
	NBWt[i,5]=0;	
	}
}
 
#NB Weight for not Hypo data with twins 

 NBWt1<-matrix(nrow=l3,ncol=5,0)
 NBWt1[,1]<-NotHypoDataWithTwins$New_Born_Weight;
 NBWt1[,2]<-NotHypoDataWithTwins$GA_delivery_days/7;
 NBWt1[,3]<-as.integer(NBWt1[,2]);
 
# 10th percentil case
for(i in 1:l3){
	if(NBWt1[i,1]!=0 && (NBWt1[i,1]<498 && NBWt1[i,3]==24 || NBWt1[i,1]<558 && NBWt1[i,3]==25 || NBWt1[i,1]<625 && NBWt1[i,3]==26 || NBWt1[i,1]<702 && NBWt1[i,3]==27 || NBWt1[i,1]<798 && NBWt1[i,3]==28 || NBWt1[i,1]<925 && NBWt1[i,3]==29 || NBWt1[i,1]<1085 && NBWt1[i,3]==30 || NBWt1[i,1]<1278 && NBWt1[i,3]==31 || NBWt1[i,1]<1495 && NBWt1[i,3]==32 || NBWt1[i,1]<1725 && NBWt1[i,3]==33 || NBWt1[i,1]<1950 && NBWt1[i,3]==34 || NBWt1[i,1]<2159 && NBWt1[i,3]==35 || NBWt1[i,1]<2354 && NBWt1[i,3]==36 || NBWt1[i,1]<2541 && NBWt1[i,3]==37 || NBWt1[i,1]<2714 && NBWt1[i,3]==38 || NBWt1[i,1]<2852 && NBWt1[i,3]==39 || NBWt1[i,1]<2929 && NBWt1[i,3]==40 || NBWt1[i,1]<2948 && NBWt1[i,3]==41 || NBWt1[i,1]<2948 && NBWt1[i,3]==42)){
		NBWt1[i,4]=1;
	}else{
	NBWt1[i,4]=0;	
	}
}
 
# 90th percentil case 
 for(i in 1:l3){
	if(NBWt1[i,1]!=0 && (NBWt1[i,1]>977 && NBWt1[i,3]==24 || NBWt1[i,1]>1138 && NBWt1[i,3]==25 || NBWt1[i,1]>1362 && NBWt1[i,3]==26 || NBWt1[i,1]>1635 && NBWt1[i,3]==27 || NBWt1[i,1]>1977 && NBWt1[i,3]==28 || NBWt1[i,1]>2361 && NBWt1[i,3]==29 || NBWt1[i,1]>2710 && NBWt1[i,3]==30 || NBWt1[i,1]>2986 && NBWt1[i,3]==31 || NBWt1[i,1]>3200 && NBWt1[i,3]==32 || NBWt1[i,1]>3370 && NBWt1[i,3]==33 || NBWt1[i,1]>3502 && NBWt1[i,3]==34 || NBWt1[i,1]>3596 && NBWt1[i,3]==35 || NBWt1[i,1]>3668 && NBWt1[i,3]==36 || NBWt1[i,1]>3755 && NBWt1[i,3]==37 || NBWt1[i,1]>3867 && NBWt1[i,3]==38 || NBWt1[i,1]>3980 && NBWt1[i,3]==39 || NBWt1[i,1]>4060 && NBWt1[i,3]==40 || NBWt1[i,1]>4094 && NBWt1[i,3]==41 || NBWt1[i,1]>4098 && NBWt1[i,3]==42)){
		NBWt1[i,5]=1;
	}else{
	NBWt1[i,5]=0;	
	}
}
 
 #New Born Weight<10 percentile
 Table2[8,1]<-length(which(NBWt[,4]==1));
 Table2[8,3]<-length(which(NBWt1[,4]==1));
 Table2[8,2]<-length(which(NBWt[,4]==1))/l2*100;
 Table2[8,4]<-length(which(NBWt1[,4]==1))/l3*100;

#New Born Weight>90 percentile
 Table2[9,1]<-length(which(NBWt[,5]==1));
 Table2[9,3]<-length(which(NBWt1[,5]==1));
 Table2[9,2]<-length(which(NBWt[,5]==1))/l2*100;
 Table2[9,4]<-length(which(NBWt1[,5]==1))/l3*100;
 
 
 
##### pvalue using pearson's Chi-square test and Fisher Exact Test and odd Ratio
leng<-nrow(Table2)
 for(i in 1:(leng-4)){
 	mym3<-matrix(c(Table2[i,1],Table2[i,3],l-Table2[i,1],l1-Table2[i,3]),nrow=2,byrow=TRUE);
    #print(mym3);
 	Table2[i,5]<-chisq.test(mym3,simulate.p.value = TRUE,B=999)$p.value;
 	Table2[i,6]<-fisher.test(mym3)$p.value; 	
 	r1<-OddRatio(mym3,alpha=0.05,quiet=F);
 	Table2[i,7]<-r1[1];
 	Table2[i,8]<-r1[2];
 	Table2[i,9]<-r1[3];	
 }
 
 
 
 for(i in 1:4){
 	k<-leng-4;
 	mym4<-matrix(c(Table2[i+k,1],Table2[i+k,3],l2-Table2[i+k,1],l3-Table2[i+k,3]),nrow=2,byrow=TRUE);
    #print(mym4);
 	Table2[i+k,5]<-chisq.test(mym4,simulate.p.value = TRUE,B=999)$p.value;
 	Table2[i+k,6]<-fisher.test(mym4)$p.value;
 	r2<-OddRatio(mym4,alpha=0.05,quiet=F);
 	Table2[i+k,7]<-r2[1];
 	Table2[i+k,8]<-r2[2];
 	Table2[i+k,9]<-r2[3];	
 }

Table2; 

write.csv(Table2, file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Table2.csv");

#######################################################################################
######################################## Table3 #######################################
#######################################################################################
 
Table3<-matrix(nrow=4, ncol=9,0)
rownames(Table3)<-c("WtG(<18.5 BMI & >18.14 kg)","WtG(BMI between 18.5 and 25 & >15.87 kg)","WtG(BMI between 25 and 30 & >11.33 kg)","WtG(<=30 BMI & >9 kg)");
colnames(Table3)<-c("Hypothyroid group (num)", " Hypothyroid group (%)", "Control group (num)", "Control group (%)", " pearson's Chi-square test(p-value)","Fisher test(p-value)", "odd Ratio","Lower","Upper");

matt<-matrix(nrow=l,ncol=3,0);
matt[,1]<- HypoData$BMI_LMP;
matt[,2]<-HypoData$Weight_gain_Kg;
 for(i in 1:l){
 	if(matt[i,1]<18.5 && matt[i,2]>18.14){
 		matt[i,3]=1;
 	}
 	if(matt[i,1]>=18.5 && matt[i,1]<25 && matt[i,2]>15.87){
 		matt[i,3]=2;
 	}
   if(matt[i,1]>=25 && matt[i,1]<30 && matt[i,2]>11.33){
 		matt[i,3]=3;
 	}
   if(matt[i,1]>=30 && matt[i,2]>9){
 		matt[i,3]=4;
 	} 
}

Table3[1,1]<- length(which(matt[,3]==1));
Table3[2,1]<- length(which(matt[,3]==2));
Table3[3,1]<- length(which(matt[,3]==3));
Table3[4,1]<- length(which(matt[,3]==4));

Table3[1,2]<- length(which(matt[,3]==1))/l*100;
Table3[2,2]<- length(which(matt[,3]==2))/l*100;
Table3[3,2]<- length(which(matt[,3]==3))/l*100;
Table3[4,2]<- length(which(matt[,3]==4))/l*100;



matt1<-matrix(nrow=l1,ncol=3,0);
matt1[,1]<- NotHypoData$BMI_LMP;
matt1[,2]<-NotHypoData$Weight_gain_Kg;
 for(i in 1:l1){
 	if(matt1[i,1]<18.5 && matt1[i,2]>18.14){
 		matt1[i,3]=1;
 	}
 	if(matt1[i,1]>=18.5 && matt1[i,1]<25 && matt1[i,2]>15.87){
 		matt1[i,3]=2;
 	}
   if(matt1[i,1]>=25 && matt1[i,1]<30 && matt1[i,2]>11.33){
 		matt1[i,3]=3;
 	}
   if(matt1[i,1]>=30 && matt1[i,2]>9){
 		matt1[i,3]=4;
 	} 
}

Table3[1,3]<- length(which(matt1[,3]==1));
Table3[2,3]<- length(which(matt1[,3]==2));
Table3[3,3]<- length(which(matt1[,3]==3));
Table3[4,3]<- length(which(matt1[,3]==4));

Table3[1,4]<- length(which(matt1[,3]==1))/l1*100;
Table3[2,4]<- length(which(matt1[,3]==2))/l1*100;
Table3[3,4]<- length(which(matt1[,3]==3))/l1*100;
Table3[4,4]<- length(which(matt1[,3]==4))/l1*100;


#################test

# ##### pearson's Chi-square test, Fisher Exact Test, Odd Ratio and confidence intervall

 for(i in 1:nrow(Table3)){
 	mym5<-matrix(c(Table3[i,1],Table3[i,3],l-Table3[i,1],l1-Table3[i,3]),nrow=2,byrow=TRUE);
    #print(mym5);
 	Table3[i,5]<-chisq.test(mym5,simulate.p.value = TRUE,B=999)$p.value;
   	Table3[i,6]<-fisher.test(mym5)$p.value;
 	r3<-OddRatio(mym5,alpha=0.05,quiet=F);
 	Table3[i,7]<-r3[1];
 	Table3[i,8]<-r3[2];
 	Table3[i,9]<-r3[3];
 }
 
Table3;

write.csv(Table3, file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Table3.csv");


#### t-test: comparison between HypoData$Weight_gain_Kg and NotHypoData$Weight_gain_Kg

#t.test(HypoData$Weight_gain_Kg,NotHypoData$Weight_gain_Kg);



