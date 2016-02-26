# Irwig Table 1 Code

# To match Begg & Greens:
# Column for test labeled "R", reference standard column labeled "D"
###Create a data frame that can be used to generate the table from Irwing
r     <- rep(1,70)
d     <- rep(1,70)
one   <- data.frame(r,d)
r     <- rep(1,99)
d     <- rep(0,99)
two   <- data.frame(r,d)
r     <- rep(0,30)
d     <- rep(1,30)
three <- data.frame(r,d)
r    <- rep(0,9801)
d    <- rep(0,9801)
four  <- data.frame(r,d)

tab1 <- rbind(one,two,three,four)

###########################################################################################
#(1) Most phenotypes are highly correlated to one another, in this simulation, we'd like
#to have 3 phenotypes (r) variables. In this next step, conditionally create two additional
#phenotypes based on tab1$r value (i.e. 0,1). 
#
#For the first new phenotype (r2), it should have prob(r2=1|r=1)=0.95, else prob(r2=1|r=0)=0.005
#For the second new phenotype (r3) it should have prob(r3=1|r=1)=0.85, else prob(r2=1|r=0)=0.01
#generate those values now and add them to the data.frame tab1
###########################################################################################


###########################################################################################
#(2) write a function called 'naive_sensitivity' and 'naive_specificity', call these functions
# 3 times using your new phenotypes, but the truth (d) always stays the same
###########################################################################################

###########################################################################################
#(2) Now, let's start 'project pretend mode' - this means we only have R, R1, R2
###########################################################################################
project_data <- tab1[,"r"]


#Stratify this sample based on "symptom_class" or strata. The strata in this case will be:
# (1) All three phenotypes said '1'
# (2) At least 1 phenotype said '1'
# (3) All phenotypes said '0'
# and take a simple random sample within each strata. What should the sample size be in each strata
#Take a minute and think about this hard! Do you sample from (1), (2), (3) the most? Write down
#a justification and then take your simple random sample based on intuition (for now). Only need 
#one sample


###########################################################################################
#(3) Add back D to your sample
###########################################################################################

###########################################################################################
#(4) Calculate the "naive" sensitivity and specificity estimator of each of the samples
#using the function(s) that you created above
###########################################################################################


###########################################################################################
#(5) Now, add the compliment of your sample back, this means that you should have 1 data.frame
# 10,000 rows and D is only present on your sample
###########################################################################################


###########################################################################################
#(6) With your 1 (10,000) row data set, calculate the Begg and Greenes estimator of
# sensitivity and specificity, do you need to modifiy your function at all? If so, remember
#in what ways.
###########################################################################################

BGsensspes <- function(sample){                                                             
  ### Function to calculate the estimated sensitivity and specificity given a sample of observations with verified cases. Specificity
  # only accurate if actual prevalence of disease is low. Sample must be two columns with labels r and d for test result and disease 
  # respectively.
  tot_r1 <- sum(sample['r']==1)                                                             # Count number of positive & negative test results for all
  tot_r0 <- sum(sample['r']==0)
  ver <- subset(sample, d!=999)                                                             # Subset for only verified samples
  ver_r1 <- sum(ver['r']==1)                                                                # COunt positive/negative tests for verified sample only
  ver_r0 <- sum(ver['r']==0)
  ver_r1_d1 <- sum(ver['r']==1&ver['d']==1)                                                 # Count disease prevalence given test result in verified sample
  ver_r0_d1 <- sum(ver['r']==0&ver['d']==1)
  
  num <- (tot_r1*ver_r1_d1)/ver_r1                                                          # Numerator of sensitivity estimate
  den <- (tot_r1*ver_r1_d1)/ver_r1+(tot_r0*ver_r0_d1)/ver_r0                                # Denominator of sensitivity estimate
  
  est_sn <- num/den
  est_sp <- (tot_r0/(dim(sample)[1]))                                                       # Specificity estimate given low prevalence (r-/N)
  
  return(c(est_sn,est_sp))
}

BG_est_one <- BGsensspes(tot_sample_one)                                                                  # Zero verified cases where r=0 & d=1 -> sensitivity = 1
BG_est_two <- BGsensspes(tot_sample_two)                                                                  # Same case as sample one
BG_est_three <- BGsensspes(tot_sample_three)
BG_est_all <- BGsensspes(tab1)


###########################################################################################
#(6) Create a "pretty" table to display your results. This time, I want you to use knitr
# and latex to generate this template. This means you'll need to create a new file in your
#repo (xxxx.Rnw) and somehow get the results generated above (i.e. estimates) into this new file
###########################################################################################






