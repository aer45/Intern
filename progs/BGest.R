#program_name: BGest.R
#date: 3/11/16

BGest <- function(sample,r){
  N <- dim(sample)[1] 
  
  Beta_1 <- sum(sample['strata']=='ALL POSITIVE')/N; Beta_1 <- ifelse(is.na(Beta_1),0,Beta_1)
  Beta_2 <- sum(sample['strata']=='ANY 1 TO 2')/N; Beta_2 <- ifelse(is.na(Beta_2),0,Beta_2)
  Beta_3 <- sum(sample['strata']=='ALL NEGATIVE')/N; Beta_3 <- ifelse(is.na(Beta_3),0,Beta_3)
  
  Theta_1 <- sum(sample[r]==1&sample['strata']=='ALL POSITIVE')/sum(sample['strata']=='ALL POSITIVE'); Theta_1 <- ifelse(is.na(Theta_1),0,Theta_1)
  Theta_2 <- sum(sample[r]==1&sample['strata']=='ANY 1 TO 2')/sum(sample['strata']=='ANY 1 TO 2'); Theta_2 <- ifelse(is.na(Theta_2),0,Theta_2) 
  Theta_3 <- sum(sample[r]==1&sample['strata']=='ALL NEGATIVE')/sum(sample['strata']=='ALL NEGATIVE'); Theta_3 <- ifelse(is.na(Theta_3),0,Theta_3)
  
  verf <- subset(sample, d!='NA')
  
  Phi_1 <- sum(verf['d']==1&verf[r]==1&verf['strata']=='ALL POSITIVE')/sum(verf[r]==1&verf['strata']=='ALL POSITIVE'); Phi_1 <- ifelse(is.na(Phi_1),0,Phi_1)
  Phi_2 <- sum(verf['d']==1&verf[r]==1&verf['strata']=='ANY 1 TO 2')/sum(verf[r]==1&verf['strata']=='ANY 1 TO 2'); Phi_2 <- ifelse(is.na(Phi_2),0,Phi_2)
  Phi_3 <- sum(verf['d']==1&verf[r]==1&verf['strata']=='ALL NEGATIVE')/sum(verf[r]==1&verf['strata']=='ALL NEGATIVE'); Phi_3 <- ifelse(is.na(Phi_3),0,Phi_3)
  
  Psi_1 <- sum(verf['d']==1&verf[r]==0&verf['strata']=='ALL POSITIVE')/sum(verf[r]==0&verf['strata']=='ALL POSITIVE'); Psi_1 <- ifelse(is.na(Psi_1),0,Psi_1)
  Psi_2 <- sum(verf['d']==1&verf[r]==0&verf['strata']=='ANY 1 TO 2')/sum(verf[r]==0&verf['strata']=='ANY 1 TO 2'); Psi_2 <- ifelse(is.na(Psi_2),0,Psi_2)
  Psi_3 <- sum(verf['d']==1&verf[r]==0&verf['strata']=='ALL NEGATIVE')/sum(verf[r]==0&verf['strata']=='ALL NEGATIVE'); Psi_3 <- ifelse(is.na(Psi_3),0,Psi_3)
  
  logitsen <- log(sum(Beta_1*Theta_1*Phi_1,Beta_2*Theta_2*Phi_2,Beta_3*Theta_3*Phi_3,na.rm=T))-
    log(sum(Beta_1*(1-Theta_1)*Psi_1,Beta_2*(1-Theta_2)*Psi_2,Beta_3*(1-Theta_3)*Psi_3,na.rm=T))
  
  sen_est  <- exp(logitsen)/(1+exp(logitsen))
  spes_est <- sum(sample[r]==0)/N
  
  return(c(sen_est,spes_est))
  
}