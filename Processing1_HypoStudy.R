## Code Dima Hypothyroidism Study
#-------------------------------------------------#
# Title: Hypothyroidism of Pregnant Women
# Dev:   Diala Ezzeddine
# Date:  30 March 2015
# Desc: Tester Hypo and not Hypo cases for pregnant women
# ChangeLog: (Who, When, What)
# <DEzzeddine, 19/11/2015, Seperate all the Hypo cases That had treatment  >
#-------------------------------------------------#


#-- Data --# 
# declare variables and constants
# Read data
source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/funct.r");
source("/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/FINAL_codeHypoStudy.r");

HypoWithTrt<-HypoData[HypoData$T4_trt==1,];
HypoWithoutTrt<-HypoData[HypoData$T4_trt==0,];

TwinsHypoWithTrt<-HypoDataWithTwins[HypoDataWithTwins$T4_trt==1,];
TwinsHypoWithoutTrt<-HypoDataWithTwins[HypoDataWithTwins$T4_trt==0,];



l4<-length(which(HypoWithTrt$T4_trt==1));
# 27
l5<-length(which(HypoWithoutTrt$T4_trt==0));
# 131


l6<-length(which(TwinsHypoWithTrt$T4_trt==1));
# 27
l7<-length(which(TwinsHypoWithoutTrt$T4_trt==0));
# 131


#-- Presentation (I/O) --#
# compare between Hypothyroid cases that had treatment and that didn't have treatment
############################################################################################
###################################### Table4 ##############################################
#############################################################################################


Table4<-matrix(nrow=16, ncol=9,0)
rownames(Table4)<-c("N","Age<=18 years", "Age>=35 years", "Nulligravidity", "Multigravidity", "Hx of >=1 abortion" ,"GDM" , "GHTN" ,"BMI at LMP<25" , "BMI at LMP between 25 and 30" , "BMI at LMP between 30 and 35" , "BMI at LMP>=35", "Hx of DM", "Hx of AI", "FHx of TD", "FHx of DM");
colnames(Table4)<-c("Hypothyroid With Treatment (num)", " Hypothyroid With Treatment (%)", "Hypothyroid Without Treatment (num)", "Hypothyroid Without Treatment(%)", " pearson's Chi-square test(p-value)","Fisher test(p-value)", "odd Ratio","Lower","Upper");


#Table4
#Num
 Table4[1,1]<-l4;
 Table4[1,3]<-l5;
 Table4[1,2]<-l4/l*100;
 Table4[1,4]<-l5/l*100;
 #age
 Table4[2,1]<-length(which(HypoWithTrt$age<=18));
 Table4[2,3]<-length(which(HypoWithoutTrt$age<=18));
 Table4[2,2]<-length(which(HypoWithTrt$age<=18))/l4*100;
 Table4[2,4]<-length(which(HypoWithoutTrt$age<=18))/l5*100;
 
 Table4[3,1]<-length(which(HypoWithTrt$age>=35));
 Table4[3,3]<-length(which(HypoWithoutTrt$age>=35));
 Table4[3,2]<-length(which(HypoWithTrt$age>=35))/l4*100;
 Table4[3,4]<-length(which(HypoWithoutTrt$age>=35))/l5*100;
#nulliparous
 Table4[4,1]<-length(which(HypoWithTrt$Gravida==1));
 Table4[4,3]<-length(which(HypoWithoutTrt$Gravida==1));
 Table4[4,2]<-length(which(HypoWithTrt$Gravida==1))/l4*100;
 Table4[4,4]<-length(which(HypoWithoutTrt$Gravida==1))/l5*100;
 #multiparous
 Table4[5,1]<-length(which(HypoWithTrt$Gravida!=1));
 Table4[5,3]<-length(which(HypoWithoutTrt$Gravida!=1));
 Table4[5,2]<-length(which(HypoWithTrt$Gravida!=1))/l4*100;
 Table4[5,4]<-length(which(HypoWithoutTrt$Gravida!=1))/l5*100;
#Abborta
 Table4[6,1]<-length(which(HypoWithTrt$Abborta>=1));
 Table4[6,3]<-length(which(HypoWithoutTrt$Abborta>=1));
 Table4[6,2]<-length(which(HypoWithTrt$Abborta>=1))/l4*100;
 Table4[6,4]<-length(which(HypoWithoutTrt$Abborta>=1))/l5*100;
#G_Diabetes_Medical
 Table4[7,1]<-length(which(HypoWithTrt$G_Diabetes_Medical==1));
 Table4[7,3]<-length(which(HypoWithoutTrt$G_Diabetes_Medical==1));
 Table4[7,2]<-length(which(HypoWithTrt$G_Diabetes_Medical==1))/l4*100;
 Table4[7,4]<-length(which(HypoWithoutTrt$G_Diabetes_Medical==1))/l5*100;
#G_HyperTension
 Table4[8,1]<-length(which(HypoWithTrt$G_HyperTension==1));
 Table4[8,3]<-length(which(HypoWithoutTrt$G_HyperTension==1));
 Table4[8,2]<-length(which(HypoWithTrt$G_HyperTension==1))/l4*100;
 Table4[8,4]<-length(which(HypoWithoutTrt$G_HyperTension==1))/l5*100;
#BMI_LMP
 Table4[9,1]<-length(which(HypoWithTrt$BMI_LMP<25));
 Table4[9,3]<-length(which(HypoWithoutTrt$BMI_LMP<25));
 Table4[9,2]<-length(which(HypoWithTrt$BMI_LMP<25))/l4*100;
 Table4[9,4]<-length(which(HypoWithoutTrt$BMI_LMP<25))/l5*100;
 
 BMImat2<-matrix(nrow=l4,ncol=3,0);
  BMImat2[,1]<-HypoWithTrt$BMI_LMP;
  
  for(i in 1:l4){
    if(BMImat2[i,1]>=25 && BMImat2[i,1]<30){
  	 BMImat2[i,2]=1;	
        }else{
        BMImat2[i,2]=0; 	
        }	
  }
   
 for(i in 1:l4){
    if(BMImat2[i,1]>=30 && BMImat2[i,1]<35){
  	      BMImat2[i,3]=1;
  	      }else{
        BMImat2[i,3]=0; 	
        }	
  }

 
 BMImat3<-matrix(nrow=l5,ncol=3,0);
  BMImat3[,1]<-HypoWithoutTrt$BMI_LMP;
  
  for(i in 1:l5){
    if(BMImat3[i,1]>=25 && BMImat3[i,1]<30){
  	 BMImat3[i,2]=1;	
        }else{
        BMImat3[i,2]=0; 	
        }	
  }
   
 for(i in 1:l5){
    if(BMImat3[i,1]>=30 && BMImat3[i,1]<35){
  	      BMImat3[i,3]=1;
  	      }else{
        BMImat3[i,3]=0; 	
        }	
  }

 
 Table4[10,1]<-length(which(BMImat2[,2]==1));
 Table4[10,3]<-length(which(BMImat3[,2]==1));
 Table4[10,2]<-length(which(BMImat2[,2]==1))/l4*100;
 Table4[10,4]<-length(which(BMImat3[,2]==1))/l5*100;
 
 Table4[11,1]<-length(which(BMImat2[,3]==1));
 Table4[11,3]<-length(which(BMImat3[,3]==1));
 Table4[11,2]<-length(which(BMImat2[,3]==1))/l4*100;
 Table4[11,4]<-length(which(BMImat3[,3]==1))/l5*100;

 Table4[12,1]<-length(which(HypoWithTrt$BMI_LMP>=35));
 Table4[12,3]<-length(which(HypoWithoutTrt$BMI_LMP>=35));
 Table4[12,2]<-length(which(HypoWithTrt$BMI_LMP>=35))/l4*100;
 Table4[12,4]<-length(which(HypoWithoutTrt$BMI_LMP>=35))/l5*100;
 
 #PersonalHistory_Diabetes
 Table4[13,1]<-length(which(HypoWithTrt$PersonalHistory_Diabetes==1));
 Table4[13,3]<-length(which(HypoWithoutTrt$PersonalHistory_Diabetes==1));
 Table4[13,2]<-length(which(HypoWithTrt$PersonalHistory_Diabetes==1))/l4*100;
 Table4[13,4]<-length(which(HypoWithoutTrt$PersonalHistory_Diabetes==1))/l5*100;
 #PersonalHistory_AI
 Table4[14,1]<-length(which(HypoWithTrt$PersonalHistory_AI==1));
 Table4[14,3]<-length(which(HypoWithoutTrt$PersonalHistory_AI==1));
 Table4[14,2]<-length(which(HypoWithTrt$PersonalHistory_AI==1))/l4*100;
 Table4[14,4]<-length(which(HypoWithoutTrt$PersonalHistory_AI==1))/l5*100;
#FamilyHistory_TD
 Table4[15,1]<-length(which(HypoWithTrt$FamilyHistory_TD==1));
 Table4[15,3]<-length(which(HypoWithoutTrt$FamilyHistory_TD==1));
 Table4[15,2]<-length(which(HypoWithTrt$FamilyHistory_TD==1))/l4*100;
 Table4[15,4]<-length(which(HypoWithoutTrt$FamilyHistory_TD==1))/l5*100;
 #FamilyHistory_Diabetes
 Table4[16,1]<-length(which(HypoWithTrt$FamilyHistory_Diabetes==1));
 Table4[16,3]<-length(which(HypoWithoutTrt$FamilyHistory_Diabetes==1));
 Table4[16,2]<-length(which(HypoWithTrt$FamilyHistory_Diabetes==1))/l4*100;
 Table4[16,4]<-length(which(HypoWithoutTrt$FamilyHistory_Diabetes==1))/l5*100;

#tester la difference entre 2 proportions prop.test 
  Table4[1,5]<- prop.test(l4,l4+l5)$p.value;
  
 # ##### pearson's Chi-square test
 for(i in 2:nrow(Table4)){
 	mymatt<-matrix(c(Table4[i,1],Table4[i,3],l4-Table4[i,1],l5-Table4[i,3]),nrow=2,byrow=TRUE);
    #print(mymatt);
 	Table4[i,5]<-chisq.test(mymatt,simulate.p.value = TRUE,B=999)$p.value;
 }
 
 
 #tester la difference entre 2 proportions binom.test
Table4[1,6]<- binom.test(l4,l4+l5)$p.value;

# Fisher Exact Test 
for(i in 2:nrow(Table4)){
 	mymatt1<-matrix(c(Table4[i,1],Table4[i,3],l4-Table4[i,1],l5-Table4[i,3]),nrow=2,byrow=TRUE);
    #print(mymatt1);
 	Table4[i,6]<-fisher.test(mymatt1)$p.value;
 }
 
  # Odd Ratio 
  Table4[1,7]<- 0;
for(i in 2:nrow(Table4)){
 	mymatt2<-matrix(c(Table4[i,1],Table4[i,3],l4-Table4[i,1],l5-Table4[i,3]),nrow=2,byrow=TRUE);
 	#print(mymatt2);
 	#Table4[i,7]<-OddRatio(mymatt2,alpha=0.05,quiet=F);
 	r5<-OddRatio(mymatt2,alpha=0.05,quiet=F);
 	Table4[i,7]<-r5[1];
 	Table4[i,8]<-r5[2];
 	Table4[i,9]<-r5[3];
 }
 

Table4;

write.csv(Table4, file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Table4.csv");

#############################################################################################
################################ Table5 ######################################################
##############################################################################################

Table5<-matrix(nrow=11, ncol=9,0)
rownames(Table5)<-c("Abortion 1st Trim","Abortion 2nd Trim", "IUFD", "PTL", "PTB", "C_Section" ,"Post-term Delivery>= 41 weeks" , "Birth Wt<10th%" ,"Birth Wt>90th%" , "Apgar score at 1 min<=7" , "Apgar score at 5 min<=7");
colnames(Table5)<-c("Hypothyroid With Treatment (num)", " Hypothyroid With Treatment (%)", "Hypothyroid Without Treatment (num)", "Hypothyroid Without Treatment(%)", " pearson's Chi-square test(p-value)","Fisher test(p-value)","Odd Ratio","Lower","Upper");


############################abbortion###################
Abb2<-matrix(nrow=l4,ncol=4,0);
Abb2[,1]<-HypoWithTrt$Trim;
Abb2[,2]<-HypoWithTrt$Abortion;
for(i in 1:l4){
	if(Abb2[i,1]==0 && Abb2[i,2]!=0){
	  Abb2[i,3]=1;
	}else{
		Abb2[i,3]=0;
	}
	if(Abb2[i,1]==1 && Abb2[i,2]!=0){
	  Abb2[i,4]=1;
	}else{
		Abb2[i,4]=0;
	}

}
# abbortion Hypo
Table5[1,1]<-length(which(Abb2[,3]==1));
Table5[2,1]<-length(which(Abb2[,4]==1));

Table5[1,2]<-(length(which(Abb2[,3]==1)))/l4*100;
Table5[2,2]<-(length(which(Abb2[,4]==1)))/l4*100;

Abb3<-matrix(nrow=l5,ncol=4,0);
Abb3[,1]<-HypoWithoutTrt$Trim;
Abb3[,2]<-HypoWithoutTrt$Abortion;
for(i in 1:l5){
	if(Abb3[i,1]==0 && Abb3[i,2]!=0){
	  Abb3[i,3]=1;
	}else{
		Abb3[i,3]=0;
	}
	if(Abb3[i,1]==1 && Abb3[i,2]!=0){
	  Abb3[i,4]=1;
	}else{
		Abb3[i,4]=0;
	}

}
# Abbortion NotHypo
Table5[1,3]<-length(which(Abb3[,3]==1));
Table5[2,3]<-length(which(Abb3[,4]==1));

Table5[1,4]<-(length(which(Abb3[,3]==1)))/l5*100;
Table5[2,4]<-(length(which(Abb3[,4]==1)))/l5*100;

#####IUFD
 Table5[3,1]<-length(which(HypoWithTrt$Intra_Uterine_Fetal_Death==1));
 Table5[3,3]<-length(which(HypoWithoutTrt$Intra_Uterine_Fetal_Death==1));
 Table5[3,2]<-length(which(HypoWithTrt$Intra_Uterine_Fetal_Death==1))/l4*100;
 Table5[3,4]<-length(which(HypoWithoutTrt$Intra_Uterine_Fetal_Death==1))/l5*100;

#####PTL
 Table5[4,1]<-length(which(HypoWithTrt$Pri_Term_Labor==1));
 Table5[4,3]<-length(which(HypoWithoutTrt$Pri_Term_Labor==1));
 Table5[4,2]<-length(which(HypoWithTrt$Pri_Term_Labor==1))/l4*100;
 Table5[4,4]<-length(which(HypoWithoutTrt$Pri_Term_Labor==1))/l5*100;
 
 #####PTB
 Table5[5,1]<-length(which(HypoWithTrt$Pri_Term_Birth==1));
 Table5[5,3]<-length(which(HypoWithoutTrt$Pri_Term_Birth==1));
 Table5[5,2]<-length(which(HypoWithTrt$Pri_Term_Birth==1))/l4*100;
 Table5[5,4]<-length(which(HypoWithoutTrt$Pri_Term_Birth==1))/l5*100;

 #####c_section
 Table5[6,1]<-length(which(HypoWithTrt$C_Section==1));
 Table5[6,3]<-length(which(HypoWithoutTrt$C_Section==1));
 Table5[6,2]<-length(which(HypoWithTrt$C_Section==1))/l4*100;
 Table5[6,4]<-length(which(HypoWithoutTrt$C_Section==1))/l5*100;
 
  ##### Post-term Delivery>= 41 weeks
  PostD2<-matrix(nrow=l4,ncol=2,0);
  PostD2[,1]<-HypoWithTrt$GA_delivery_days;
  for(i in 1:l4){
  	if(PostD2[i,1]>=287){
  		PostD2[i,2]=1;
  	}else{
  		PostD2[i,2]=0;
  	}
  }
  
  
  PostD3<-matrix(nrow=l5,ncol=2,0);
  PostD3[,1]<-HypoWithoutTrt$GA_delivery_days;
  for(i in 1:l5){
  	if(PostD3[i,1]>=287){
  		PostD3[i,2]=1;
  	}else{
  		PostD3[i,2]=0;
  	}
  }

  
 Table5[7,1]<-length(which(PostD2[,2]==1));
 Table5[7,3]<-length(which(PostD3[,2]==1));
 Table5[7,2]<-length(which(PostD2[,2]==1))/l4*100;
 Table5[7,4]<-length(which(PostD3[,2]==1))/l5*100;

#####Apgar 1 min <=7
apgar1min2<-matrix(nrow=l6,ncol=2,0);
apgar1min2[,1]<-TwinsHypoWithTrt$Apgar_1min;
for(i in 1:l6){
  	if(apgar1min2[i,1]<=7 && apgar1min2[i,1]!=0){
  		apgar1min2[i,2]=1;
  	}else{
  		apgar1min2[i,2]=0;
  	}
  }

apgar1min3<-matrix(nrow=l7,ncol=2,0);
apgar1min3[,1]<-TwinsHypoWithoutTrt$Apgar_1min;
for(i in 1:l7){
  	if(apgar1min3[i,1]<=7 && apgar1min3[i,1]!=0){
  		apgar1min3[i,2]=1;
  	}else{
  		apgar1min3[i,2]=0;
  	}
  }

 Table5[10,1]<-length(which(apgar1min2==1));
 Table5[10,3]<-length(which(apgar1min3==1));
 Table5[10,2]<-length(which(apgar1min2==1))/l6*100;
 Table5[10,4]<-length(which(apgar1min3==1))/l7*100;
 
 #####Apgar 5 min <=7
apgar5min2<-matrix(nrow=l6,ncol=2,0);
apgar5min2[,1]<-TwinsHypoWithTrt$Apgar_5min;
for(i in 1:l6){
  	if(apgar5min2[i,1]<=7 && apgar5min2[i,1]!=0){
  		apgar5min2[i,2]=1;
  	}else{
  		apgar5min2[i,2]=0;
  	}
  }

apgar5min3<-matrix(nrow=l7,ncol=2,0);
apgar5min3[,1]<-TwinsHypoWithoutTrt$Apgar_5min;
for(i in 1:l7){
  	if(apgar5min3[i,1]<=7 && apgar5min3[i,1]!=0){
  		apgar5min3[i,2]=1;
  	}else{
  		apgar5min3[i,2]=0;
  	}
  }

 Table5[11,1]<-length(which(apgar5min2==1));
 Table5[11,3]<-length(which(apgar5min3==1));
 Table5[11,2]<-length(which(apgar5min2==1))/l6*100;
 Table5[11,4]<-length(which(apgar5min3==1))/l7*100;
 
 ##### NB Weight 
 #NB Weight for Hypo data with twins

 NBWt2<-matrix(nrow=l6,ncol=5,0)
 NBWt2[,1]<-TwinsHypoWithTrt$New_Born_Weight;
 NBWt2[,2]<-TwinsHypoWithTrt$GA_delivery_days/7;
 NBWt2[,3]<-as.integer(NBWt2[,2]);
 
# 10th percentil case
for(i in 1:l6){
	if(NBWt2[i,1]!=0 && (NBWt2[i,1]<498 && NBWt2[i,3]==24 || NBWt2[i,1]<558 && NBWt2[i,3]==25 || NBWt2[i,1]<625 && NBWt2[i,3]==26 || NBWt2[i,1]<702 && NBWt2[i,3]==27 || NBWt2[i,1]<798 && NBWt2[i,3]==28 || NBWt2[i,1]<925 && NBWt2[i,3]==29 || NBWt2[i,1]<1085 && NBWt2[i,3]==30 || NBWt2[i,1]<1278 && NBWt2[i,3]==31 || NBWt2[i,1]<1495 && NBWt2[i,3]==32 || NBWt2[i,1]<1725 && NBWt2[i,3]==33 || NBWt2[i,1]<1950 && NBWt2[i,3]==34 || NBWt2[i,1]<2159 && NBWt2[i,3]==35 || NBWt2[i,1]<2354 && NBWt2[i,3]==36 || NBWt2[i,1]<2541 && NBWt2[i,3]==37 || NBWt2[i,1]<2714 && NBWt2[i,3]==38 || NBWt2[i,1]<2852 && NBWt2[i,3]==39 || NBWt2[i,1]<2929 && NBWt2[i,3]==40 || NBWt2[i,1]<2948 && NBWt2[i,3]==41 || NBWt2[i,1]<2948 && NBWt2[i,3]==42)){
		NBWt2[i,4]=1;
	}else{
	NBWt2[i,4]=0;	
	}
}
 
 # 90th percentil case
 for(i in 1:l6){
	if(NBWt2[i,1]!=0 && (NBWt2[i,1]>977 && NBWt2[i,3]==24 || NBWt2[i,1]>1138 && NBWt2[i,3]==25 || NBWt2[i,1]>1362 && NBWt2[i,3]==26 || NBWt2[i,1]>1635 && NBWt2[i,3]==27 || NBWt2[i,1]>1977 && NBWt2[i,3]==28 || NBWt2[i,1]>2361 && NBWt2[i,3]==29 || NBWt2[i,1]>2710 && NBWt2[i,3]==30 || NBWt2[i,1]>2986 && NBWt2[i,3]==31 || NBWt2[i,1]>3200 && NBWt2[i,3]==32 || NBWt2[i,1]>3370 && NBWt2[i,3]==33 || NBWt2[i,1]>3502 && NBWt2[i,3]==34 || NBWt2[i,1]>3596 && NBWt2[i,3]==35 || NBWt2[i,1]>3668 && NBWt2[i,3]==36 || NBWt2[i,1]>3755 && NBWt2[i,3]==37 || NBWt2[i,1]>3867 && NBWt2[i,3]==38 || NBWt2[i,1]>3980 && NBWt2[i,3]==39 || NBWt2[i,1]>4060 && NBWt2[i,3]==40 || NBWt2[i,1]>4094 && NBWt2[i,3]==41 || NBWt2[i,1]>4098 && NBWt2[i,3]==42)){
		NBWt2[i,5]=1;
	}else{
	NBWt2[i,5]=0;	
	}
}
 
#NB Weight for not Hypo data with twins 

 NBWt3<-matrix(nrow=l7,ncol=5,0)
 NBWt3[,1]<-TwinsHypoWithoutTrt$New_Born_Weight;
 NBWt3[,2]<-TwinsHypoWithoutTrt$GA_delivery_days/7;
 NBWt3[,3]<-as.integer(NBWt3[,2]);
 
# 10th percentil case
for(i in 1:l7){
	if(NBWt3[i,1]!=0 && (NBWt3[i,1]<498 && NBWt3[i,3]==24 || NBWt3[i,1]<558 && NBWt3[i,3]==25 || NBWt3[i,1]<625 && NBWt3[i,3]==26 || NBWt3[i,1]<702 && NBWt3[i,3]==27 || NBWt3[i,1]<798 && NBWt3[i,3]==28 || NBWt3[i,1]<925 && NBWt3[i,3]==29 || NBWt3[i,1]<1085 && NBWt3[i,3]==30 || NBWt3[i,1]<1278 && NBWt3[i,3]==31 || NBWt3[i,1]<1495 && NBWt3[i,3]==32 || NBWt3[i,1]<1725 && NBWt3[i,3]==33 || NBWt3[i,1]<1950 && NBWt3[i,3]==34 || NBWt3[i,1]<2159 && NBWt3[i,3]==35 || NBWt3[i,1]<2354 && NBWt3[i,3]==36 || NBWt3[i,1]<2541 && NBWt3[i,3]==37 || NBWt3[i,1]<2714 && NBWt3[i,3]==38 || NBWt3[i,1]<2852 && NBWt3[i,3]==39 || NBWt3[i,1]<2929 && NBWt3[i,3]==40 || NBWt3[i,1]<2948 && NBWt3[i,3]==41 || NBWt3[i,1]<2948 && NBWt3[i,3]==42)){
		NBWt3[i,4]=1;
	}else{
	NBWt3[i,4]=0;	
	}
}
 
# 90th percentil case 
 for(i in 1:l7){
	if(NBWt3[i,1]!=0 && (NBWt3[i,1]>977 && NBWt3[i,3]==24 || NBWt3[i,1]>1138 && NBWt3[i,3]==25 || NBWt3[i,1]>1362 && NBWt3[i,3]==26 || NBWt3[i,1]>1635 && NBWt3[i,3]==27 || NBWt3[i,1]>1977 && NBWt3[i,3]==28 || NBWt3[i,1]>2361 && NBWt3[i,3]==29 || NBWt3[i,1]>2710 && NBWt3[i,3]==30 || NBWt3[i,1]>2986 && NBWt3[i,3]==31 || NBWt3[i,1]>3200 && NBWt3[i,3]==32 || NBWt3[i,1]>3370 && NBWt3[i,3]==33 || NBWt3[i,1]>3502 && NBWt3[i,3]==34 || NBWt3[i,1]>3596 && NBWt3[i,3]==35 || NBWt3[i,1]>3668 && NBWt3[i,3]==36 || NBWt3[i,1]>3755 && NBWt3[i,3]==37 || NBWt3[i,1]>3867 && NBWt3[i,3]==38 || NBWt3[i,1]>3980 && NBWt3[i,3]==39 || NBWt3[i,1]>4060 && NBWt3[i,3]==40 || NBWt3[i,1]>4094 && NBWt3[i,3]==41 || NBWt3[i,1]>4098 && NBWt3[i,3]==42)){
		NBWt3[i,5]=1;
	}else{
	NBWt3[i,5]=0;	
	}
}
 
 #New Born Weight<10 percentile
 Table5[8,1]<-length(which(NBWt2[,4]==1));
 Table5[8,3]<-length(which(NBWt3[,4]==1));
 Table5[8,2]<-length(which(NBWt2[,4]==1))/l6*100;
 Table5[8,4]<-length(which(NBWt3[,4]==1))/l7*100;

#New Born Weight>90 percentile
 Table5[9,1]<-length(which(NBWt2[,5]==1));
 Table5[9,3]<-length(which(NBWt3[,5]==1));
 Table5[9,2]<-length(which(NBWt2[,5]==1))/l6*100;
 Table5[9,4]<-length(which(NBWt3[,5]==1))/l7*100;
 
 
 
##### pvalue using pearson's Chi-square test and Fisher Exact Test and odd Ratio
leng1<-nrow(Table5)
 for(i in 1:(leng1-4)){
 	mym6<-matrix(c(Table5[i,1],Table5[i,3],l4-Table5[i,1],l5-Table5[i,3]),nrow=2,byrow=TRUE);
    #print(mym6);
 	Table5[i,5]<-chisq.test(mym6,simulate.p.value = TRUE,B=999)$p.value;
 	Table5[i,6]<-fisher.test(mym6)$p.value; 	
 	r6<-OddRatio(mym6,alpha=0.05,quiet=F);
 	Table5[i,7]<-r6[1];
 	Table5[i,8]<-r6[2];
 	Table5[i,9]<-r6[3];	
 }
 
 
 
 for(i in 1:4){
 	k1<-leng1-4;
 	mym7<-matrix(c(Table5[i+k1,1],Table5[i+k1,3],l6-Table5[i+k1,1],l7-Table5[i+k1,3]),nrow=2,byrow=TRUE);
    #print(mym7);
 	Table5[i+k1,5]<-chisq.test(mym7,simulate.p.value = TRUE,B=999)$p.value;
 	Table5[i+k1,6]<-fisher.test(mym7)$p.value;
 	r7<-OddRatio(mym4,alpha=0.05,quiet=F);
 	Table5[i+k1,7]<-r7[1];
 	Table5[i+k1,8]<-r7[2];
 	Table5[i+k1,9]<-r7[3];	
 }

Table5; 

write.csv(Table5, file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Table5.csv");
#######################################################################################
######################################## Table6 #######################################
#######################################################################################
 
Table6<-matrix(nrow=4, ncol=9,0)
rownames(Table6)<-c("WtG(<18.5 BMI & >18.14 kg)","WtG(BMI between 18.5 and 25 & >15.87 kg)","WtG(BMI between 25 and 30 & >11.33 kg)","WtG(<=30 BMI & >9 kg)");
colnames(Table6)<-c("Hypothyroid With Treatment (num)", " Hypothyroid With Treatment (%)", "Hypothyroid Without Treatment (num)", "Hypothyroid Without Treatment(%)", " pearson's Chi-square test(p-value)","Fisher test(p-value)", "odd Ratio","Lower","Upper");

matt2<-matrix(nrow=l4,ncol=3,0);
matt2[,1]<- HypoWithTrt$BMI_LMP;
matt2[,2]<-HypoWithTrt$Weight_gain_Kg;
 for(i in 1:l4){
 	if(matt2[i,1]<18.5 && matt2[i,2]>18.14){
 		matt2[i,3]=1;
 	}
 	if(matt2[i,1]>=18.5 && matt2[i,1]<25 && matt2[i,2]>15.87){
 		matt2[i,3]=2;
 	}
   if(matt2[i,1]>=25 && matt2[i,1]<30 && matt2[i,2]>11.33){
 		matt2[i,3]=3;
 	}
   if(matt2[i,1]>=30 && matt2[i,2]>9){
 		matt2[i,3]=4;
 	} 
}

Table6[1,1]<- length(which(matt2[,3]==1));
Table6[2,1]<- length(which(matt2[,3]==2));
Table6[3,1]<- length(which(matt2[,3]==3));
Table6[4,1]<- length(which(matt2[,3]==4));

Table6[1,2]<- length(which(matt2[,3]==1))/l4*100;
Table6[2,2]<- length(which(matt2[,3]==2))/l4*100;
Table6[3,2]<- length(which(matt2[,3]==3))/l4*100;
Table6[4,2]<- length(which(matt2[,3]==4))/l4*100;



matt3<-matrix(nrow=l5,ncol=3,0);
matt3[,1]<- HypoWithoutTrt$BMI_LMP;
matt3[,2]<-HypoWithoutTrt$Weight_gain_Kg;
 for(i in 1:l5){
 	if(matt3[i,1]<18.5 && matt3[i,2]>18.14){
 		matt3[i,3]=1;
 	}
 	if(matt3[i,1]>=18.5 && matt3[i,1]<25 && matt3[i,2]>15.87){
 		matt3[i,3]=2;
 	}
   if(matt3[i,1]>=25 && matt3[i,1]<30 && matt3[i,2]>11.33){
 		matt3[i,3]=3;
 	}
   if(matt3[i,1]>=30 && matt3[i,2]>9){
 		matt3[i,3]=4;
 	} 
}

Table6[1,3]<- length(which(matt3[,3]==1));
Table6[2,3]<- length(which(matt3[,3]==2));
Table6[3,3]<- length(which(matt3[,3]==3));
Table6[4,3]<- length(which(matt3[,3]==4));

Table6[1,4]<- length(which(matt3[,3]==1))/l5*100;
Table6[2,4]<- length(which(matt3[,3]==2))/l5*100;
Table6[3,4]<- length(which(matt3[,3]==3))/l5*100;
Table6[4,4]<- length(which(matt3[,3]==4))/l5*100;


#################test

# ##### pearson's Chi-square test, Fisher Exact Test, Odd Ratio and confidence intervall

 for(i in 1:nrow(Table6)){
 	mym8<-matrix(c(Table6[i,1],Table6[i,3],l4-Table6[i,1],l5-Table6[i,3]),nrow=2,byrow=TRUE);
    #print(mym8);
 	Table6[i,5]<-chisq.test(mym8,simulate.p.value = TRUE,B=999)$p.value;
   	Table6[i,6]<-fisher.test(mym8)$p.value;
 	r8<-OddRatio(mym8,alpha=0.05,quiet=F);
 	Table6[i,7]<-r8[1];
 	Table6[i,8]<-r8[2];
 	Table6[i,9]<-r8[3];
 }
 
Table6;

write.csv(Table6, file = "/Users/dialaezzeddine/Desktop/Dima_Study/FinalCODING/Results/Table6.csv");