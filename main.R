
library("synthpop")

### selection of variables

vars <- c("edu", "ls", "wkabint")

ods <- SD2011[, vars]

my.seed <- 17914709


sds.default <- syn(ods, seed = my.seed)


sds.default$call
sds.default$method
sds.default$syn
sds.default$visit.sequence
sds.default$predictor.matrix


### Create data frame with categorical columns

set.seed(123)

s = seq(from = 0, to = 3)

sa = sample(s, size = 19, replace = TRUE)
sa_f <- factor(sa)


dat <- data.frame(id = letters[1:3], x = sa[1:3], y = letters[11:13])

sapply(dat, class)

dat$x <- factor(dat$x)


dat










