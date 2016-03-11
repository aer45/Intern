#program_name: 03_sample_draw.R
#date: 3/4/16
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

count <- rowSums(project_data)
strat_one <- project_data[count==3,]
# n = 141
strat_two <- project_data[count==1|count==2,]
# n = 195
strat_three <- project_data[count==0,]
# n = 9664


set.seed(6)
strat_samp <- rbind(strat_one[sample(1:dim(strat_one)[1],70),],strat_two[sample(1:dim(strat_two)[1],150),]
                    ,strat_three[sample(1:dim(strat_three)[1],300),])


###########################################################################################
#(3) Add back D to your sample
###########################################################################################

strat_samp <- tab1[row.names(strat_samp),]
full <- project_data
full['d'] <- rep(NA,dim(project_data)[1])
full[row.names(strat_samp),'d'] <- strat_samp$d
fuller <- full
fuller$sum <- rowSums(fuller[,c('r','r2','r3')])
fuller$strata <- ifelse(fuller$sum==3,'ALL POSITIVE',ifelse(fuller$sum==0,'ALL NEGATIVE',"ANY 1 TO 2"))
fuller$sample <- ifelse(is.na(fuller$d),0,1)
