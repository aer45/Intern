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




###########################################################################################
#(2) Now, let's start 'project pretend mode' - this means we only have R
###########################################################################################
project_data <- tab1[,"r"]


#Stratify this sample based on "r" and take a simple random sample within each strata - do this 3 times
##make sure you set.seed on each sample so we can regenerate the results
#first time sample 10 r+ and 20 r-

#second time sample 20 r+ and 50 r-

#third time sample 40 r+ and 200 r-


###########################################################################################
#(3) Add back D to each of your samples, after this, you should have 3 data.frames of
#30, 70, and 240
###########################################################################################


###########################################################################################
#(4) Calculate the "naive" sensitivity and specificity estimator of each of the samples
###########################################################################################


###########################################################################################
#(5) Now, add the compliment of your samples back, this means that you should have 3 data.frames
# 10,000 rows and D is only present on 30 rows for one, 70 rows for another, and 420 rows
# for another
###########################################################################################

###########################################################################################
#(6) With your 3 (10,000) row data sets, calculate the Begg and Greenes estimator of
# sensitivity and specificity
###########################################################################################

###########################################################################################
#(6) Create a "pretty" table to display your results, this should be a 4 row, 3 column table
# that displays the "sample name (i.e. size)", the naive estimator, and the begg and greenes
#estimator. You may need two tables, one for sensitivity and one for specificity.
###########################################################################################



