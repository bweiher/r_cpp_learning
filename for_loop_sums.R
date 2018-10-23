library(microbenchmark)
library(reticulate)
library(Rcpp)


x <- runif(1e6)

# for loop sums in 3 languages

# rstats way
rway <- function(x){
  total <- 0
  for(g in seq_along(x)){
    total <- total + x[g]
  }
  total  
}


rway(x)
rway(x) == sum(x)

# python way

np <- import("numpy")
source_python("py_sum.py")

pyway(x)
pyway(x) == sum(x)

# c++ way

cppFunction('double cway(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i];
  }
  return total;
}')

cway(x)
cway(x) == sum(x)


microbenchmark(
  rway(x),  
  pyway(x),  
  cway(x), 
  sum(x)
)

