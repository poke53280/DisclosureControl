
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


acRows <- c(3300, 3500, 3700, 4000)
acColsA <- c(12, 14, 16, 17, 19, 20, 21, 22 )
acColsB <- c(6, 8, 9, 10, 11, 12, 13, 14, 15 )

nRUNS = 1000

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


