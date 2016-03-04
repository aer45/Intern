#program_name: 02_phenotype_generators.R
#date: 3/4/16

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