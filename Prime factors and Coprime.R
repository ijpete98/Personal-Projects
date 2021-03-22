#Function to prime factorize an integer input.
primes <- function(x=1) {
  if(x %% 1 != 0 | x < 1) {
    # Output an error for non-integers and numbers less than 1.
    stop("Input must be a natural number.")
  }
  if(x == 1){
    # We treat the prime factorization of 1 to be 1.
    return(c(1))
  }
  factors <- c()
  r <- x # r is a value that will change as we divide factors out of it.
  i <- 2 # i is the iterating integer that we will try to divide by
  while(prod(factors) != x & i <= sqrt(r)){
    # While the elements of the `factors` vector do not multiply to the input,
    # and the testing divisor is less than or equal to the shrinking value of r,
    # continue to test for divisibility by i and the iteration of i.
    if(r %% i == 0){
      # if i is a factor, add it to the factors vector and redefine r.
      factors <- append(factors, i)
      r <- r/i
    }else{
      # if i was not a factor, iterate it up to the next odd number (this is not as efficient
      # as it might be, but even for large primes the computation is quick.)
      if(i %% 2 == 0){
        i <- i+1
      }else{
        i <- i+2}
    }
  }
  if(prod(factors) != x){
    # This if statement should never have to run, but if the factors vector is incomplete
    # (does not multiply to x), append the factor that is missing.
    factors <- append(factors, x/prod(factors))
  }
  return(factors)
}


# Function to take two integer inputs and compare their prime factorizations
  # to determine if they are coprime (i.e. they share no factors.) 
coprime <- function(a,b){
  a.factors <- unique(primes(a))
  b.factors <- unique(primes(b))
  # Duplicate primes are irrelevant, so unique makes the loop run slightly faster.
  
  for(i in 1:length(a.factors)){
    # If any factors are common to the inputs, stop the loop and return FALSE
    if(a.factors[i] %in% b.factors){
      return(FALSE)
    }
  }
  # If FALSE was never returned, the inputs must be coprime, so TRUE is returned.
  return(TRUE)
}