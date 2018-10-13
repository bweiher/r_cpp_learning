library(Rcpp)

# connects an R function to compiled C++ code
cppFunction('int add(int x, int y, int z) {
  int sum = x + y + z;
  return sum;
}')


#  works like a regular R function
add