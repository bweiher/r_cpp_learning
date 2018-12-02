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




# implement the following R functions in cpp
# all()

all(c(T,F))
all(c(T,T,T))
all(c(T,T,T,F))

# vector input and scalar output
# need to track if there is any not TRUE statement

cppFunction('bool cppAll(LogicalVector x){
            int n = x.size();
            double y = 0;
            
            for(int i = 0; i < n; ++i){
              if(x[i]) y += 1;
            }
            return y == n;
            }')

cppAll(c(T,F,F,F))
cppAll(c(F,F,F,T))
cppAll(c(T,F))
cppAll(c(T,T,T))
cppAll(c(T,T,T,F))
cppAll(c(T,T))




# cumprod() 
# vector input, vector output
cumprod(x)
cumprod(1:3)


cppFunction('NumericVector cppcumProd(NumericVector x){
          int n = x.size();
          NumericVector out(n);
          
          out[0] = x[0];
          for(int i = 0; i < n; ++i){
            if(i > 0) out[i] = x[i] * out[i - 1];
          }
            return out;
}')


cppcumProd(1:3)
cppcumProd(x)
cumprod(x)



# cummin(), 

cummin(x)
cummin(5:1)
cummin(sample(1:100, 10))

# return the lowest value up to taht point
cppFunction('NumericVector cppCumIn(NumericVector x){
          int n = x.size();
          NumericVector out(n);
          out[0] = x[0];
          for(int i = 0; i < n; ++i){
             if(i > 0){
               if(x[i] <= out[i - 1]) out[i] = x[i];
               else out[i] = out[i - 1];
             }
          }
          return out;
}')

cummin(c(5,4,6))
cppCumIn(x)
cppCumIn(3:1)
cppCumIn(5:1)

# cummax(). ~ skip since similar to cummax 


# 
# diff(). Start by assuming lag 1, and then generalise for lag n.
diff(c(1,3,5,7), lag = 1)
diff(c(1:10), lag = 1)

# find the difference in values between subsequent numbers
cppFunction('NumericVector cppDiff(NumericVector x, int lag){
            int n = x.size();
            NumericVector out(n);
            
            for(int i = 0; i < n; ++i){
               if(i == 0) out[i] = NA_REAL ;
               else out[i] = x[i] - x[i - lag];
            }
            
            return out[ !is_na(out) ];
}')


cppDiff(x, lag = 1)
diff(x, lag = 1)

cppDiff(rev(x), lag = 1)
diff(rev(x), lag =1)

# 
# range. 

range(x)
range(c(1,5))
range(c(1,3,5))

# loop through X and find the max and min numbers 
cppFunction('NumericVector cppRange(NumericVector x){
       int n = x.size();
       NumericVector out(n);
       
       out[0] = x[0];
       for(int i = 0; i < n; ++i){
         if(i > 0) {
          if(out[i - 1] > x[i]) 
         }
       }
       return min;
            
}')

cppRange(c(1,2,3))
range(c(1,2,3,4,5,6))


# 
# var. Read about the approaches you can take on wikipedia. Whenever implementing a numerical algorithm, it's always good to check what is already known about the problem.




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