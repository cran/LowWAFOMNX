##' Low WAFOM Niederreiter-Xing Sequence
##'
##' Description: R implementation of Low Walsh Figure of Merit Sequence
##' based on Niederreiter-Xing Sequence.
##'
##' Porting to R by Mutsuo Saito.
##' The R version does not returns cordinate value zero,
##' but returns value very near to zero, 2^-64.
##'
##'@section Acknowledgment:
##' The development of this code is partially supported
##' by JST CREST.
##'
##'@examples
##' srange <- lowWAFOMNX.dimMinMax()
##' mrange <- lowWAFOMNX.dimF2MinMax(srange[1])
##' points <- lowWAFOMNX.points(dimR=srange[1], dimF2=mrange[1], count=10000)
##' points <- lowWAFOMNX.points(dimR=srange[1], dimF2=mrange[1], count=10000,
##'                             digitalShift=TRUE)
##'@section Reference:
##' * Shinsuke Mori,
##'   "Suuchi Sekibun no tameno QMC Ten Shuugou no Sekkei, Tansaku,
##'   oyobi sono Yuukousei",
##'   Master's Thesis, 2017,
##' * Ryuichi Ohori,
##'   "Efficient Quasi Monte Carlo Integration by Adjusting the
##'   Derivation-sensitivity Parameter of Walsh Figure of Merit",
##'   Master's Thesis, 2015.
##' * S. Harase and R. Ohori,
##'   "A search for extensible low-WAFOM point sets",
##'   arXiv preprint, arXiv:1309.7828, (2013),
##'   https://arxiv.org/abs/1309.7828.
##' * M. Matsumoto and R. Ohori,
##'   "Walsh Figure of Merit for Digital Nets: An Easy Measure
##'   for Higher Order Convergent QMC",
##'   Springer International Publishing, Cham, 2016, pp. 143-160.
##' * M. Matsumoto, M. Saito, and K. Matoba,
##'   "A computable figure of merit for quasi-Monte Carlo point sets",
##'   Mathematics of Computation, 83 (2014), pp. 1233-1250.
##' * G. Pirsic,
##'   "A software implementation of Niederreiter-Xing sequences",
##'   in Monte Carlo and Quasi-Monte Carlo Methods 2000,
##'   Springer, 2002, pp. 434-445.
##'   https://sites.google.com/site/isabelpirsic/nxlegacy.
##' * C. P. Xing and H. Niederreiter,
##'   "A construction of low-discrepancy sequences using global
##'   function fields",
##'   ACTA ARITHMETICA, 73 (1995), pp. 87-102.
##'
##'@name LowWAFOMNX-package
##'@aliases LowWAFOMNX-package LowWAFOMNX
##'@docType package
##'@import Rcpp
##'@import RSQLite
##'@importFrom stats runif
##'@useDynLib LowWAFOMNX
NULL

##' get minimum and maximum dimension number of Low WAFOM Niederreiter-Xing
##' Sequence
##'
##'@return supportd minimum and maximum dimension number.
##'@export
lowWAFOMNX.dimMinMax <- function() {
  fname <- "nxlw64.sqlite3"
  sql <- "select min(dimr) as min, max(dimr) as max from digitalnet;"
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = system.file("extdata",
                                             fname, package = "LowWAFOMNX"))
  a <- dbGetQuery(con, sql)
  dbDisconnect(con)
  c(a[1, 1], a[1, 2])
}

##' get minimum and maximum F2 dimension number.
##'
##'@param dimR dimention.
##'@return supportd minimum and maximum F2 dimension number
##'@export
lowWAFOMNX.dimF2MinMax <- function(dimR) {
  fname <- "nxlw64.sqlite3"
  fmt <- "select min(dimf2) as min, max(dimf2) as max from digitalnet where dimr = %d;"
  sql <- sprintf(fmt, dimR)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = system.file("extdata",
                                             fname, package = "LowWAFOMNX"))
  a <- dbGetQuery(con, sql)
  dbDisconnect(con)
  c(a[1, 1], a[1, 2])
}

##' get points from Low WAFOM Niederreiter-XingSobolSequence
##'
##' This R version does not returns cordinate value zero,
##' but returns value very near to zero, 2^-64.
##'@param dimR dimention.
##'@param dimF2 F2-dimention of each element.
##'@param count number of points.
##'@param digitalShift use digital shift or not.
##'@return matrix of points where every row contains dimR dimensional point.
##'@export
lowWAFOMNX.points <- function(dimR,
                              dimF2 = 10,
                              count,
                              digitalShift = FALSE) {
  smax = lowWAFOMNX.dimMinMax()
  if (dimR < smax[1] || dimR > smax[2]) {
    stop(sprintf("dimR should be an integer %d <= dimR <= %d", smax[1], smax[2]))
  }
  if (missing(dimF2)) {
    dimF2 = max(dimF2, ceiling(log2(count)))
  }
  mmax = lowWAFOMNX.dimF2MinMax(dimR)
  if (dimF2 < mmax[1] || dimF2 > mmax[2]) {
    stop(sprintf("dimF2 should be an integer %d <= dimF2 <= %d", mmax[1], mmax[2]))
  }
  fname <- "nxlw64.sqlite3"
  fmt <- paste("select dimr, dimf2, wafom, tvalue, quote(data) as data from digitalnet ",
               "where dimr = %d and dimf2 = %d;")
  sql <- sprintf(fmt, dimR, dimF2)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname = system.file("extdata",
                                             fname, package = "LowWAFOMNX"))
  df <- dbGetQuery(con, sql)
  dbDisconnect(con)
  if (digitalShift) {
    sv <- runif(2*dimR, min=-2^31, max=2^31-1)
  } else {
    sv <- numeric(1)
  }
  #  print(sv)
  return(rcppLowWAFOMNXPoints(df, dimR, dimF2, count, sv))
}