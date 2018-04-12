
library("synthpop")




syn_run <- function(nRows, nNumCatA, nNumCatB) {

  
  
  cat_a = seq(from = 0, to = nNumCatA - 1)
  
  sa = sample(cat_a, size = nRows, replace = TRUE)
  sa_f <- factor(sa)
  
  
  cat_b = seq(from = 0, to = nNumCatB - 1)
  
  sb = sample(cat_b, size = nRows, replace = TRUE)
  sb_f <- factor(sb)
  
  
  NUMERIC_MAX = 5 * nRows
  
  cat_c = seq(from = 0, to = NUMERIC_MAX -1)
  sc = sample(cat_c, size = nRows, replace = TRUE)
  
  
  cat_d = seq(from = 0, to = NUMERIC_MAX -1)
  sd = sample(cat_d, size = nRows, replace = TRUE)
  
  
  dat <- data.frame(a = sa_f, b = sb_f, c = sc, d = sd)
  
  
  start_time = Sys.time()
  
  sds.default <- syn(dat, seed = my.seed, maxfaclevels = max(nNumCatA, nNumCatB))
  
  end_time = Sys.time()
  
  dT = as.numeric(end_time - start_time)
  
  return (dT)
  
}


acRows <- c(5000, 25000, 50000)
acColsA <- c(12, 16, 17, 20, 30)
acColsB <- c(6, 9, 15, 11, 21, 30)

nRUNS = 10

lcRows =  sample(acRows, nRUNS, replace = TRUE)
lcColsA =  sample(acColsA, nRUNS, replace = TRUE)
lcColsB = sample(acColsB, nRUNS, replace = TRUE)

lcTime = vector(length = nRUNS)


for (i in 1:(nRUNS)) {

  
  nRow = lcRows[i]
  nColsA = lcColsA[i]
  nColsB = lcColsB[i]
  
  t = syn_run(nRow, nColsA, nColsB)
  
  lcTime[i] = t

}


res <- data.frame(n = lcRows, a = lcColsA, b = lcColsB, t = lcTime)


