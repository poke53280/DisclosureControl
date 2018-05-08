
library("synthpop")




get_cat_column <- function (nRows, nNumFactors) {
  
  cat_values = seq(from = 0, to = nNumFactors - 1)
  
  s = sample(cat_values, size = nRows, replace = TRUE)
  
  s_f = factor(s)
  
  return (s_f)

}


syn_run <- function(nRows, nNumCatA, nNumCatB) {
  
  
  message(sprintf("syn_run: (%d, %d, %d)\n", nRows, nNumCatA, nNumCatB))
  
  NUMERIC_MAX = 10 * nRows

  free_value_sequence  = seq(from = 0, to = NUMERIC_MAX -1)
  
  
  dat <- data.frame(c_0 = get_cat_column(nRows, nNumCatA), 
                    c_1 = get_cat_column(nRows, nNumCatB),
                    c_2 = get_cat_column(nRows, 4),
                    c_3 = get_cat_column(nRows, 7),
                    c_4 = get_cat_column(nRows, 8),
                    c_5 = get_cat_column(nRows, 8),
                    
                    c = sample(free_value_sequence, size = nRows, replace = TRUE), 
                    d = sample(free_value_sequence, size = nRows, replace = TRUE))
  
  
  start_time = Sys.time()
  
  sds.default <- syn(dat, seed = 2123, maxfaclevels = max(nNumCatA, nNumCatB))
  
  end_time = Sys.time()
  
  dT = as.numeric(end_time - start_time)
  
  return (dT)
  
}


acRows <- c(9000, 15000, 19000)
acColsA <- c(9, 9, 9, 9, 9, 9, 9, 9)
acColsB <-  c(9, 9, 9, 9, 9)

nRUNS = 30



lcRows =  sample(acRows, nRUNS, replace = TRUE)
lcColsA =  sample(acColsA, nRUNS, replace = TRUE)
lcColsB = sample(acColsB, nRUNS, replace = TRUE)

lcTime = vector(length = nRUNS)


for (i in 1:(nRUNS)) {

  cat(i, nRUNS)
  nRow = lcRows[i]
  nColsA = lcColsA[i]
  nColsB = lcColsB[i]
  
  
  cat ("nRows=", nRow, ",nColA=", nColsA, ", nColsB=", nColsB)
  
  t = syn_run(nRow, nColsA, nColsB)
  
  lcTime[i] = t

}


res <- data.frame(n = lcRows, a = lcColsA, b = lcColsB, t = lcTime)


