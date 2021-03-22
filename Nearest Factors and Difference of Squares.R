# This function takes an integer input and computes the two numbers closest together that multiply to get the input.
  # There is an option to make sure that both numbers of the output have the same parity (both are even or both are odd).
  # i.e. the output of NearestFactors(12) would be c(3, 4),
    # but the output with sameParity = TRUE would be c(2, 6).
NearestFactors <- function(number, sameParity = FALSE){
  if(number %% 1 != 0 | number < 0){
    #Print an error for non-integers or negative numbers.
    stop("Input must be a positive integer.")
  }
  if(number %% 2 == 1 | sameParity == FALSE){
    # This runs if the number is odd, or the number is even but we don't care about parity
    
    # We test for divisibility starting with the largest integer smaller than the square root of the input.
    testNum <- floor(sqrt(number))
    
    while(number %% testNum != 0){
      # Until the number we're testing for divisibility is a factor of the input, we iterate it down by 1.
        # If the number is prime, the testNum will iterate all the way down to 1 and stop there.
      testNum <- testNum - 1 
    }
    
    return(c(testNum, number/testNum)) # Return a vector of the two closest factors.
    
  }else{
    # The value of sameParity doesn't matter unless the input is even (because odd numbers have only odd factors)
    if(number %% 4 == 2){
      stop('If "sameParity" argment is TRUE, input must be odd or a multiple of four.')
    }else{
      # If input is a multiple of 4 and sameParity = TRUE, the correct output is
        # just two times the output for input/4, regardless of that number's parity.
        # It should be noted that because the sameParity argument is set to FALSE,
          #this else statement will only run the NearestFactors function again ONCE.
      2 * NearestFactors(number/4, sameParity=FALSE)
    }
  }
}


# Any odd number or multiple of 4 can be expressed as the difference of two perfect squares.
  # This function calculates the smallest set of positive integers whose squares subtract to a given input.
  # See also: "The Difference of Two Squares" by Matt Parker and James Grime: https://www.youtube.com/watch?v=LkIK8f4yvOU
DifferenceofSquares <- function(x){
  if(x %% 4 == 2){
    stop("Multiples of two cannot be expressed as a difference of squares unless they are also divisible by four.")
  }
  # The larger integer is the mean of the factors; the smaller is their difference divided by two.
  factors <- NearestFactors(x, sameParity = TRUE) # sameParity must be true to avoid squaring halves.
  return(c(mean(factors), mean(factors)-factors[1]))
  
}


# Examples:
num <- 500

# This is the only example for which `num %% 4`` can equal 2.
NearestFactors(num)

# For some even numbers, the result of this line will be different than the previous:
NearestFactors(num, sameParity = TRUE)

# Example output and proof that the answer is correct
(answer <- DifferenceofSquares(num))
answer[1] ^ 2 - answer[2] ^ 2
