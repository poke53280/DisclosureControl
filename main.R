
library(synthpop)

### selection of variables
vars <- c("sex","age","marital","income","ls","smoke")
ods  <- SD2011[1:1000,vars]

### default synthesis
s1 <- syn(ods)
s1


