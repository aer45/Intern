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
#For the second new phenotype (r3) it should have prob(r3=1|r=1)=0.85, else prob(r3=1|r=0)=0.01
#generate those values now and add them to the data.frame tab1
###########################################################################################
set.seed(5)
r2_1 <- sample(c(1,0),length(tab1$r[tab1$r==1]),replace=T,p=c(0.95,0.05))
r2_0 <- sample(c(1,0),length(tab1$r[tab1$r==0]),replace=T,p=c(0.005,0.995))
r2 <- c(r2_1,r2_0)

r3_1 <- sample(c(1,0),length(tab1$r[tab1$r==1]),replace=T,p=c(0.85,0.15))
r3_0 <- sample(c(1,0),length(tab1$r[tab1$r==0]),replace=T,p=c(0.01,0.99))
r3 <- c(r3_1,r3_0)

tab1 <- data.frame(r3,r2,tab1)
tab1 <- tab1[,c(4:1)]

###########################################################################################
#(2) write a function called 'naive_sensitivity' and 'naive_specificity', call these functions
# 3 times using your new phenotypes, but the truth (d) always stays the same
###########################################################################################
naive_sensitivity <- function(x,k){
  naiv_sn = sum(x[,k+1][x$d==1])/sum(tab1$d==1)
  return(naiv_sn)
}

naive_sensitivity(tab1,1)
naive_sensitivity(tab1,2)
naive_sensitivity(tab1,3)

# Naive sensitivity: r1 = 0.70, r2 = 0.64, r3 = 0.64

naive_specificity <- function(x,k){
  naiv_sp = 1 - (sum(x[,k+1][x$d==0])/sum(tab1$d==0))
  return(naiv_sp)
}

naive_specificity(tab1,1)
naive_specificity(tab1,2)
naive_specificity(tab1,3)

# Naive specificity: r1 = 0.99, r2 = 0.985, r3 = 0.980

###########################################################################################
#(2) Now, let's start 'project pretend mode' - this means we only have R, R1, R2
###########################################################################################
project_data <- tab1[,2:4]


#Stratify this sample based on "symptom_class" or strata. The strata in this case will be:
# (1) All three phenotypes said '1'
# (2) At least 1 phenotype said '1'
# (3) All phenotypes said '0'
# and take a simple random sample within each strata. What should the sample size be in each strata
#Take a minute and think about this hard! Do you sample from (1), (2), (3) the most? Write down
#a justification and then take your simple random sample based on intuition (for now). Only need 
#one sample


#### I want to sample more from (1) because that is my smallest group, but contains the event that
# I want to see (i.e. 'test positive'). Thus if I oversample my test positive group I can get more
# information on sensitivity than having equal groups which would require a larger sample in total.
# Also I would like to sample group two more havily than three because of the same reason above, but
# not as heavily as one.

strat_one <- project_data[project_data$r==1&project_data$r2==1&project_data$r3==1,]
# n = 141
strat_two <- project_data[project_data$r==1|project_data$r2==1|project_data$r3==1,]
# n = 336
strat_three <- project_data[project_data$r==0&project_data$r2==0&project_data$r3==0,]
# n = 9664


# Sampling proportion respect to strata: 0.25, 0.15, 0.05

set.seed(6)
strat_samp <- rbind(strat_one[sample(1:dim(strat_one)[1],35),],strat_two[sample(1:dim(strat_two)[1],50),]
                    ,strat_three[sample(1:dim(strat_three)[1],480),])

# n = 565


###########################################################################################
#(3) Add back D to your sample
###########################################################################################

strat_samp <- tab1[row.names(strat_samp),]

###########################################################################################
#(4) Calculate the "naive" sensitivity and specificity estimator of each of the samples
#using the function(s) that you created above
###########################################################################################

naive_sensitivity(strat_samp,1) # 0.23
naive_sensitivity(strat_samp,2) # 0.23
naive_sensitivity(strat_samp,3) # 0.22

naive_specificity(strat_samp,1) # 0.997
naive_specificity(strat_samp,2) # 0.996
naive_specificity(strat_samp,3) # 0.995

###########################################################################################
#(5) Now, add the compliment of your sample back, this means that you should have 1 data.frame
# 10,000 rows and D is only present on your sample
###########################################################################################
full <- project_data
full['d'] <- rep('NA',dim(project_data)[1])
full[row.names(strat_samp),'d'] <- strat_samp$d

###########################################################################################
#(6) With your 1 (10,000) row data set, calculate the Begg and Greenes estimator of
# sensitivity and specificity, do you need to modifiy your function at all? If so, remember
#in what ways.
###########################################################################################

BGest <- function(sample,x){
  
  subset <- sample[names(sample)!=c(x,'d')]
  Bx1p <- sum(subset[1])/length(subset[,1])
  Bx1n <- sum(subset[1]==0)/length(subset[,1])
  Bx2p <- sum(subset[2])/length(subset[,2])
  Bx2n <- sum(subset[2]==0)/length(subset[,2])
    
  thetax1p <- sum(sample[x]==1&subset[1]==1)/sum(subset[1])
  thetax1n <- sum(sample[x]==1&subset[1]==0)/sum(subset[1]==0)
  thetax2p <- sum(sample[x]==1&subset[2]==1)/sum(subset[2])
  thetax2n <- sum(sample[x]==1&subset[2]==0)/sum(subset[2]==0)
  
  versub <- subset(sample, d!='NA')
  versubx <- versub[names(versub)!=c(x,'d')]
  
  phix1p <- sum(versub['d']==1&versub[x]==1&versubx[1]==1)/sum(versub[x]==1&versubx[1]==1)
  phix1n <- sum(versub['d']==1&versub[x]==1&versubx[1]==0)/sum(versub[x]==1&versubx[1]==0)
  phix2p <- sum(versub['d']==1&versub[x]==1&versubx[2]==1)/sum(versub[x]==1&versubx[2]==1)
  phix2n <- sum(versub['d']==1&versub[x]==1&versubx[2]==0)/sum(versub[x]==1&versubx[2]==0)
  
  psix1p <- sum(versub['d']==1&versub[x]==0&versubx[1]==1)/sum(versub[x]==0&versubx[1]==1)
  psix1n <- sum(versub['d']==1&versub[x]==0&versubx[1]==0)/sum(versub[x]==0&versubx[1]==0)
  psix2p <- sum(versub['d']==1&versub[x]==0&versubx[2]==1)/sum(versub[x]==0&versubx[2]==1)
  psix2n <- sum(versub['d']==1&versub[x]==0&versubx[2]==0)/sum(versub[x]==0&versubx[2]==0)
  
  logorsen <- log(sum(Bx1p*thetax1p*phix1p,Bx1n*thetax1n*phix1n,Bx2p*thetax2p*phix2p,
                     Bx2n*thetax2n*phix2n,na.rm=T))-log(sum(Bx1p*(1-thetax1p)*psix1p,
                     Bx1n*(1-thetax1n)*psix1n,Bx2p*(1-thetax2p)*psix2p,Bx2n*(1-thetax2n)*psix2n,
                     na.rm=T))
  sen_est = exp(logorsen)/(1+exp(logorsen))
  spes_est <- sum(sample[x]==0)/length(sample[,x])
  
  return(c(sen_est,spes_est))
}


BGest2 <- function(sample,x){
  pr1 <- sum(sample[x]==1)/dim(sample)[1]
  sub <- subset(sample, d!='NA')
  phi1 <- sum(sub['d']==1&sub[x]==1)/sum(sub[x]==1)
  psi1 <- sum(sub['d']==1&sub[x]==0)/sum(sub[x]==0)
  lor <- log(pr1*phi1)-log((1-pr1)*psi1)
  return(exp(lor)/(1+exp(lor)))
}

R1 <- BGest(full,'r')
R2 <- BGest(full,'r2')
R3 <- BGest(full,'r3')
Actual <- c(0.7,0.99)

###########################################################################################
#(6) Create a "pretty" table to display your results. This time, I want you to use knitr
# and latex to generate this template. This means you'll need to create a new file in your
#repo (xxxx.Rnw) and somehow get the results generated above (i.e. estimates) into this new file
###########################################################################################

table <- t(data.frame(R1,R2,R3,Actual, row.names = c('Sensitivity','Specificty')))



