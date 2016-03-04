#program_name: BGest.R
#date: 3/4/16
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