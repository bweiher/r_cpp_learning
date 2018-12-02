# rcpp questions

library(Rcpp)


cppFunction("double f1(NumericVector x) {
  int n = x.size();
  double y = 0;

  for(int i = 0; i < n; ++i) {
    y += x[i] / n;
  }
  return y;
}")

x <- 1:10

for(g in seq_along(x)){
  y = x[g] / length(x)
  print(y)
}
f1(x)
median(x)
f1(c(1,2,3))



#  Vector input, vector output
cppFunction('NumericVector pdistC(double x, NumericVector ys) {
  int n = ys.size();
  NumericVector out(n);

  for(int i = 0; i < n; ++i) {
    out[i] = sqrt(pow(ys[i] - x, 2.0));
  }
  return out;
}')

pdistC(0.5, runif(10))



cppFunction('NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);

  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}')
set.seed(1014)
x <- matrix(sample(100), 10)
rowSums(x)

# examples -----

# += addition operator
# mean function
cppFunction('double f1(NumericVector x) {
  int n = x.size();
  double y = 0;
  
  for(int i = 0; i < n; ++i) {
    y += x[i] ;
  }
  y = y / n;
  return y;
}')

x <- 1:10
f1(x)
sum(x / length(x))
mean(x)



# cumsum  ~ 
cppFunction('NumericVector f2(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for(int i = 1; i < n; ++i) {
    out[i] = out[i - 1] + x[i];
  }
  return out;
}'
)

f2(x)
cumsum(x)


# does the bool x contain a TRUE value
# bool f3(LogicalVector x) {
#   int n = x.size();
#   
#   for(int i = 0; i < n; ++i) {
#     if (x[i]) return true;
#   }
#   return false;
# }


cppFunction('int f3(LogicalVector x) {
  int n = x.size();
  
  for(int i = 0; i < n; ++i) {
    if (x[i]) return i;
  }
  return 99;
}')

f3(c(F,F,F))
f3(c(T,T,F))
f3(c(F,F,F,F,T))

f33(1:3)


cppFunction('int f4(Function pred, List x) {
  int n = x.size();

  for(int i = 0; i < n; ++i) {
    LogicalVector res = pred(x[i]);
    if (res[0]) return i + 1;
  }
  return 0;
}')






# NumericVector f5(NumericVector x, NumericVector y) {
#   int n = std::max(x.size(), y.size());
#   NumericVector x1 = rep_len(x, n);
#   NumericVector y1 = rep_len(y, n);
#   
#   NumericVector out(n);
#   
#   for (int i = 0; i < n; ++i) {
#     out[i] = std::min(x1[i], y1[i]);
#   }
#   
#   return out;
# }