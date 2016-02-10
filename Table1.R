# Irwig Table 1 Code

# To match Begg & Greens:
# Column for test labeled "R", reference standard column labeled "D"

# Assume sensitivity, specificty, and prevalence of disease known

sens <- 0.70
spes <- 0.99
prev <- 0.01

n <- 10000

Rpos <- n*prev*sens
Rneg <- n*prev(1-sens)

Dpos <- n*prev
Dneg <- n*(1-prev)  

counts <- matrix(c(Dpos*sens,Dneg*(1-spes),
                   Dpos*(1-sens),Dneg*(spes),
                   Dpos,Dneg),nrow=3,byrow=T)
counts<-cbind(counts,rowSums(counts))
table1 <- data.frame(counts,row.names=c('R+','R-','Total'))
names(table1)<-c('D+','D-','Total')
table1 
