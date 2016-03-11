# Irwig Table 1 Code

# To match Begg & Greens:
# Column for test labeled "R", reference standard column labeled "D"

# Assume sensitivity, specificty, and prevalence of disease known 

sens <- 0.70                                           # Known estimates
spes <- 0.99
prev <- 0.01

n <- 10000                                             # Sample size

Dpos <- n*prev                                         # Calculate ACTUAL disease counts  
Dneg <- n*(1-prev)  

counts <- matrix(c(Dpos*sens,Dneg*(1-spes),            # Create matrix with test results
                   Dpos*(1-sens),Dneg*(spes),
                   Dpos,Dneg),nrow=3,byrow=T)
counts<-cbind(counts,rowSums(counts))                  # Sum across rows for total
table1 <- data.frame(counts,
                     row.names=c('R+','R-','Total'))
names(table1)<-c('D+','D-','Total')                    # Add names to columns/rows
table1 


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
#(1) Based on the population see if you can verify the reported sensitivity and specifity here
###########################################################################################
real_sensitivity <- sum(tab1['r']==1&tab1['d']==1)/sum(tab1['d']==1)
# 0.7
real_specificity <- sum(tab1['r']==0&tab1['d']==0)/sum(tab1['d']==0)
# 0.99

###########################################################################################
#(2) Now, let's start 'project pretend mode' - this means we only have R
###########################################################################################
project_data <- tab1[,"r"]


#Stratify this sample based on "r" and take a simple random sample within each strata - do this 3 times
##make sure you set.seed on each sample so we can regenerate the results
#first time sample 10 r+ and 20 r-
set.seed(10)
samp_one_r <- c(sample(1:length(project_data[project_data==1]),10),
                sample(length(project_data[project_data==1]+1):length(project_data),20))
#second time sample 20 r+ and 50 r-
set.seed(20)
samp_two_r <- c(sample(1:length(project_data[project_data==1]),20),
                sample(length(project_data[project_data==1]+1):length(project_data),50))
#third time sample 40 r+ and 200 r-
set.seed(30)
samp_three_r <- c(sample(1:length(project_data[project_data==1]),40),
                  sample(length(project_data[project_data==1]+1):length(project_data),200))

###########################################################################################
#(3) Add back D to each of your samples, after this, you should have 3 data.frames of
#30, 70, and 240
###########################################################################################
sample_one <- tab1[samp_one_r,]
sample_two <- tab1[samp_two_r,]
sample_three <- tab1[samp_three_r,]
###########################################################################################
#(4) Calculate the "naive" sensitivity and specificity estimator of each of the samples
###########################################################################################
naive_sensspes_one <- c(sum(sample_one['r']==1&sample_one['d']==1)/sum(sample_one['d']==1), 
                        sum(sample_one['r']==0&sample_one['d']==0)/sum(sample_one['d']==0))
naive_sensspes_two <- c(sum(sample_two['r']==1&sample_two['d']==1)/sum(sample_two['d']==1), 
                        sum(sample_two['r']==0&sample_two['d']==0)/sum(sample_two['d']==0))
naive_sensspes_three <- c(sum(sample_three['r']==1&sample_three['d']==1)/sum(sample_three['d']==1), 
                          sum(sample_three['r']==0&sample_three['d']==0)/sum(sample_three['d']==0))

###########################################################################################
#(5) Now, add the compliment of your samples back, this means that you should have 3 data.frames
# 10,000 rows and D is only present on 30 rows for one, 70 rows for another, and 240 rows
# for another
###########################################################################################
project_data_full <- data.frame(project_data, rep(999,length(project_data)))                                    # Code 'missing' d as 999
names(project_data_full)<-c('r','d')
tot_sample_one <- project_data_full; tot_sample_two <- project_data_full; tot_sample_three <- project_data_full
tot_sample_one[samp_one_r,'d'] <- sample_one['d']                                                               # Replace samples with known d
tot_sample_two[samp_two_r,'d'] <- sample_two['d']
tot_sample_three[samp_three_r,'d'] <- sample_three['d']

###########################################################################################
#(6) With your 3 (10,000) row data sets, calculate the Begg and Greenes estimator of
# sensitivity and specificity
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
#(6) Create a "pretty" table to display your results, this should be a 4 row, 3 column table
# that displays the "sample name (i.e. size)", the naive estimator, and the begg and greenes
#estimator. You may need two tables, one for sensitivity and one for specificity.
###########################################################################################

onesn <- c(dim(sample_one)[1],naive_sensspes_one[1],BG_est_one[1])
twosn <- c(dim(sample_two)[1],naive_sensspes_two[1],BG_est_two[1])
threesn <- c(dim(sample_three)[1],naive_sensspes_three[1],BG_est_three[1])
allsn <- c(dim(tab1)[1],real_sensitivity,BG_est_all[1])
sngroup <- rbind(onesn,twosn,threesn,allsn)

sn_table <- data.frame(sngroup,row.names=c('First','Second','Third','All'))
names(sn_table) <- c('Sample Size','Naive Estimate','Begg & Greenes Est. (Sensitivity)')

onesp <- c(dim(sample_one)[1],naive_sensspes_one[2],BG_est_one[2])
twosp <- c(dim(sample_two)[1],naive_sensspes_two[2],BG_est_two[2])
threesp <- c(dim(sample_three)[1],naive_sensspes_three[2],BG_est_three[2])
allsp <- c(dim(tab1)[1],real_specificity,BG_est_all[2])
spgroup <- rbind(onesp,twosp,threesp,allsp)

sp_table <- data.frame(spgroup,row.names=c('First','Second','Third','All'))
names(sp_table) <- c('Sample Size','Naive Estimate','Begg & Greenes Est. (Specificity)')

sn_table
sp_table


