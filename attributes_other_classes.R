library(Rcpp)

cppFunction('NumericVector attribs() {
  NumericVector out = NumericVector::create(1, 2, 3);
  
  out.names() = CharacterVector::create("a", "b", "c");
  out.attr("my-attr") = "my-value";
  out.attr("class") = "my-classss";
  
  return out;
}')

# argumentless-function
attribs()
class(attribs()) # retrievee classes
attr(attribs(), 'my-attr') # retrieve attributes names


cppFunction('List missing_sampler() {
  return List::create(
    NumericVector::create(NA_REAL),
    IntegerVector::create(NA_INTEGER),
    LogicalVector::create(NA_LOGICAL),
    CharacterVector::create(NA_STRING));
}')

missing_sampler() 



# Lists and data frames
cppFunction('double mpe(List mod) {
  if (!mod.inherits("lm")) stop("Input must be a linear model");
  
  NumericVector resid = as<NumericVector>(mod["residuals"]);
  NumericVector fitted = as<NumericVector>(mod["fitted.values"]);
  
  int n = resid.size();
  double err = 0;
  for(int i = 0; i < n; ++i) {
    err += resid[i] / (fitted[i] + resid[i]);
  }
  return err / n;
}
')


lm_output <- lm(mpg ~ ., data = mtcars)
class(lm_output)
inherits(lm_output, "lm")

  
lm_output['residuals']
lm_output['fitted.values']



# pass an R function to cpp

cppFunction('RObject callWithOne(Function f, double x) {
  return f(x);
}')

timestwo <- function(x){
  x * 2
}

callWithOne(f = timestwo, x = 3)
callWithOne(f = sqrt, x = 9)
callWithOne(f = sqrt, x = 16)


# lapply c++ version
# still applies an R function here.
cppFunction('List lapply1(List input, Function f) {
  int n = input.size();
  List out(n);

  for(int i = 0; i < n; i++) {
    out[i] = f(input[i]);
  }

  return out;
}')


list(
  c(1:10),
  c(9:45)
) %>% 
  lapply1(input = ., f = mean)
