library(Rcpp)
library(tidyverse)
library(microbenchmark)
x <- 1:10
# implement the following R functions in cpp
# all()

scenarios <- list(
  c(T,F),
  c(T,T,T),
  c(T,T,T,T,F),
  c(F,T,F,T),
  c(T,T,T,F),
  c(T,T)
)

# vector input and scalar output
# need to track if there is any not TRUE statement

cppFunction('bool cpp_all(LogicalVector x){
            int n = x.size();
            double y = 0;
            
            for(int i = 0; i < n; ++i){
              if(x[i]) y += 1;
            }
            return y == n;
            }')

map_lgl(scenarios, all) == map_lgl(scenarios, cpp_all)



microbenchmark(
  map_lgl(scenarios, all) ,
  map_lgl(scenarios, cpp_all)
)




# cumprod() 
# vector input, vector output
cumprod(x)
cumprod(1:3)


cppFunction('NumericVector cpp_cumprod(NumericVector x){
          int n = x.size();
          NumericVector out(n);
          
          out[0] = x[0];
          for(int i = 0; i < n; ++i){
            if(i > 0) out[i] = x[i] * out[i - 1];
          }
            return out;
}')


cpp_cumprod(1:3)
cpp_cumprod(x)
cumprod(x)

c(1, -10, 20) %>% cumprod
c(1, -10, 20) %>% cpp_cumprod

cpp_cumprod(5:1)
cumprod(5:1)

# grouping comes for free in dplyr, at least
tibble(x=x) %>% 
  group_by(grp = ifelse(x %% 2 == 0, "even", "odd")) %>% 
  mutate(cprod = cpp_cumprod(x)) %>% 
  filter(grp == "odd")


# return the lowest value up to taht point
cppFunction('NumericVector cpp_cumin(NumericVector x){
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
cpp_cumin(x)
cpp_cumin(3:1)
cpp_cumin(5:1)

# cummax(). ~ skip since similar to cummax 
cppFunction('NumericVector cpp_cummax(NumericVector x){
          int n = x.size();
          NumericVector rout(n);
          rout[0] = x[0]; /* deals with */
          
          for(int i = 0; i < n; ++i){
             if(i > 0) {
             if(x[i] > rout[i - 1]) rout[i] = x[i];
             else rout[i] = rout[i - 1];
             }
          }
          return rout;
}')


cpp_cummax(1:5)
cpp_cummax(5:1)

# 
# diff(). Start by assuming lag 1, and then generalise for lag n.
diff(c(1,3,5,7), lag = 1)
diff(c(1:10), lag = 1)

# find the difference in values between subsequent numbers
cppFunction('NumericVector cpp_diff(NumericVector x, int lag){
            int n = x.size();
            NumericVector out(n);
            
            for(int i = 0; i < n; ++i){
               if(i == 0) out[i] = NA_REAL ;
               else out[i] = x[i] - x[i - lag];
            }
            
            return out[ !is_na(out) ];
}')


cppFunction('int cpp_diff2(double x){
    double y;
    y = x - NA_REAL;
    return y;
}')

cpp_diff(1)

cpp_diff(x, lag = 1)
diff(x, lag = 1)

cpp_diff(rev(x), lag = 1)
diff(rev(x), lag =1)

cpp_diff(rev(x), lag = 2) # fuck ; still buggy
diff(rev(x), lag =2)


microbenchmark(
  cpp_diff(x, lag = 1),
  diff(x, lag = 1)
)


# 
# range. 

range(x)
range(c(1,5))
range(c(1,3,5))

# loop through X and find the max/min, and then return them as a vector
cppFunction('NumericVector cpp_range(NumericVector x){
       int n = x.size();
       int max;
       int min;
       max = x[0];
       min = x[0];
       
       for(int i = 0; i < n; ++i){
          if(i > 0){
            if(x[i] > max) max = x[i];
            if(x[i] < min) min = x[i];
          }
       } 
       
       NumericVector xx = NumericVector::create(min,max);
       return xx;
            
}')

cpp_range(1:10)
cpp_range(c(3,2,8))
cpp_range(c(3,2))
cpp_range(c(2,3))
cpp_range(c(1,2,3))
cpp_range(c(-10, -15, -100))
range(c(1,2,3,4,5,6))



runif(n = 1e6, 1, 1e7) %>% round -> x2
microbenchmark(
  range(x2 ),
  cpp_range(x2)
)

# 
# var. Read about the approaches you can take on wikipedia. Whenever implementing a numerical algorithm, it's always good to check what is already known about the problem.

# two pass algorithm 
# get the mean first, and then generate 

cppFunction('double cpp_var(NumericVector x){
            int n = x.size();
            double sum = 0;
            
            /* get the mean */
            for(int i = 0; i < n; ++i){
            sum += x[i];
            }
            double mean = sum/n;
            
            /* get the sum of the squared deviation */
            double devs_sum = 0;
            for(int i = 0; i < n; ++i){
             devs_sum = devs_sum += pow( x[i] - mean, 2.0) / (n - 1);
            }
            return devs_sum;
}')

var(x)
cpp_var(x)

microbenchmark(
  var(x),
  cpp_var(x)
)

cpp_var(x2)
var(x2)

x3 <- runif(n = 1e6, -10, 10) 
near(cpp_var(x3),var(x3))

# Rewrite any of the functions from the first exercise to deal with missing values. If na.rm is true, ignore the missing values. If na.rm is false, return a missing value if the input contains any missing values. Some good functions to practice with are min(), max(), range(), mean(), and var().


# try out min
x <- 1:10

# report on SIZE if or not NA
cppFunction('int cpp_length(NumericVector x, bool rm_na = false){
  if(rm_na == false){
  int n = x.size();
  return(n);
  } else {
  x = x[!is_na(x)];
  int n = x.size();
  return(n);
  }
}')

cpp_length(1:5)
cpp_length(c(1:5, NA), rm_na = FALSE)
cpp_length(c(1:5, NA), rm_na = T)

# get min w/ NA.RM
# if we detect an NA 
cppFunction('double cpp_min(NumericVector x, bool na_rm = false){
       if(na_rm == true) x = x[!is_na(x)] ;
       int n = x.size();
       double min;
       min = x[0];
       for(int i = 0; i < n; ++i){
          if(i > 0){
            if(x[i] < min) min = x[i];
          }
       } 
       return min;
            
}')

cpp_min(x) # works
cpp_min(x, na_rm = T) # works
cpp_min(c(NA,x), na_rm = T) # works
cpp_min(c(NA,x)) # default, works by accident ?
cpp_min(c(NA,x), na_rm = F) # works, but by accident ?
cpp_min(c(1:99, NA))
cpp_min(c(1,NA))
cpp_min(1:5)
cpp_min(5:1)
cpp_min(c(1:3),na_rm = T)

cpp_min(c(1,2,3,NA),rm_na = TRUE)
# Rewrite cumsum() and diff() so they can handle missing values. Note that these functions have slightly more complicated behaviour. 


# na solution
# immediately count NAs -> if count > 0 -> NA 
# ELSE actual solution 