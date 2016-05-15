
#alpha=0.05;
#RefRow=2;
######################################## odd Ratio#################################
OddRatio <- function(mat, alpha, quiet=F){
	
	   n<- nrow(mat);
	   matRowname<- rownames(mat);
			
		RowName<- matRowname[1];
		
		HypoUnexposed<- mat[n,1];
		ControlUnexposed<- mat[n,2];
		HypoExposed<- mat[n-1,1];
		ControlExposed<- mat[n-1,2];
		
		
		SumExposed<- HypoExposed + ControlExposed;
		SumUnexposed<- HypoUnexposed + ControlUnexposed;
			
		ProbHypoTelExposed<- HypoExposed / SumExposed;
		ProbHypoTelUnexposed<- HypoUnexposed / SumUnexposed;
		ProbControlTelExposed<- ControlExposed / SumExposed;
		ProbControlTelUnexposed<- ControlUnexposed / SumUnexposed;
		    
	#calculate the odd ratio
		oddsRatio<- (ProbHypoTelExposed*ProbControlTelUnexposed)/(ProbControlTelExposed*ProbHypoTelUnexposed);
		 
		    
		    # if(quiet == FALSE){	
		    	# print(paste("category=", RowName, ", odds Ratio=", oddsRatio))
		    # }
		    
    # calculate a confidence interval
		    
		 ConfLevel<- (1-alpha)*100;
		 sigma<- sqrt((1/HypoExposed)+(1/ControlExposed)+ (1/HypoUnexposed)+(1/ControlUnexposed))
		    
		 z<- qnorm(1-(alpha/2));
		 LowVal<- oddsRatio * exp(-z *sigma);
	     UppVal<- oddsRatio * exp(z *sigma);
	     
	     
	     x<-c(oddsRatio,LowVal,UppVal);
		    x;
 		   # if(quiet == FALSE){
		   	 #print(paste("category=", RowName, "," , ConfLevel, "% Confidence Interval=[", LowVal,",", UppVal, "]"))	
		   # }
		   	 		
	
	       # if(quiet==TRUE && n==2){	
		     # return(oddsRatio);
	       # }
		
}




