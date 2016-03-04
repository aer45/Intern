#program_name: 01_data_generator.R
#date: 3/4/16
gendat <- function() {
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
return(tab1)
}

tab1 <- gendat()