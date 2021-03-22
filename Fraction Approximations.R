{
  # Create the two functions found in the "Prime Factors and Coprime.R" file:
    # The first calculates the Prime factors of an integer input
    # The second takes two inputs and compares their prime factors to determine if they are coprime.
  # The aformentioned file contains explanations of the steps taken in these functions.
  primes <- function(x=1) {
  if(x %% 1 != 0 | x < 1) {
    stop("Input must be a natural number.")
  }
  if(x == 1){
    return(c(1))
  }
  factors <- c()
  r <- x
  i <- 2
  while(prod(factors) != x & i <= sqrt(r)){
    if(r %% i == 0){
      factors <- append(factors, i)
      r <- r/i
    }else{
      if(i %% 2 == 0){
        i <- i+1
      }else{
        i <- i+2}
    }
  }
  if(prod(factors) != x){
    factors <- append(factors, x/prod(factors))
  }
  return(factors)
  }
  
  
  coprime <- function(a,b){
  a.factors <- unique(primes(a))
  b.factors <- unique(primes(b))

  for(i in 1:length(a.factors)){
    if(a.factors[i] %in% b.factors){
      return(FALSE)
    }
  }
  return(TRUE)
  }
}


# The FracApprox function takes a value and computes a fractional approximation of it.

# Arguments:
  # Decimal:      the input value, and the only argument that is required for basic use of the function.
  #
  # Num & Denom:  an initial approximation can be provided to give the program a place to start
  #
  # Significance: the number of digits after the decimal point that should match in the approximation
  #               If no number is manually chosen, the number of digits after the
  #                 decimal point in the input is used (excluding trailing zeroes.)
  #               Significance should be manually chosen if a non-terminating fraction or irrational number is the input.
  #
  # Side:         By default, the function doesn't care if the approximation is greater than or less than the input.
  #               Any positive or negative value here will tell the program to produce
  #                 an output either greater than or less than the decimal value, respectively.
  #
  # maxRounds:    The maximum number of iterations that the function will run.
  #
  # track:        If set to TRUE, will print out each approximation that comes closer to the input value.
  #                 TRUE by default.

# It should be noted that it is possible for errors to result if side does not equal zero and
  # the significance level is smaller than the number of digits after the decimal point.

FracApprox <- function(decimal=0, num = 1, denom = 2, significance = "default",
                       side=0, maxRounds = 100000, track = TRUE){
  
  options(scipen=999) # Prevent scientific notation from messing up values
  
  if(decimal %% 1 == 0){
    # If there is no decimal component, return the input by itself (no calculations are done.)
    return(decimal)
  }
  
  if(decimal < 0){
    # Negative inputs will be treated as positive, and the user will be notified.
    warning(paste("The input is negative. It was processed as a positive number."))
    decimal <- -decimal
  }
  
  if(decimal > 1 & num == 1 & denom == 2){
    # If the input has a non-zero whole number component and no ratio is manually chosen,
      # this sets the denominator to two and the numerator to a number close to,
      # and on the correct side of 2*decimal.
    num <- 2*decimal + sign(side)
    denom <- 2
  }
  
  # If negative or non-whole numbers are given for num and denom
    # (or the previous section set num to a non-integer), this sets them to whole numbers.
  num <- round(abs(num))
  denom <- round(abs(denom))
  
  #If no significance is manually chosen, set it to the number of digits after the decimal point.
  if(significance == "default"){
    significance <- length(strsplit(as.character(decimal), "")[[1]]) -      # Character length of the input
                    which(strsplit(as.character(decimal), "")[[1]] == ".")  # Placement of '.' character in string.
  }
  
  # Scaler increases by one every round, unless a better fractional approximation is found (then it is reset to 1).
  scaler <- 1
  
  # Roundnum keeps a tally of the number of iterations the loop runs for.
  roundNum <- 0
  
  if(track == TRUE){
    # Print starting values (labeled as round zero)
    print(noquote(paste("0: ",num,"/",denom,sep="")))
  }
  
  if(side == 0){ 
    # If no side is specified, this first loop runs.
    while(round(num/denom, digits = significance) != round(decimal, digits = significance) &
          roundNum < maxRounds){
      # The loop runs until the fraction reaches the desired numer of matching digits,
        # or the number of rounds exceeds the specified limit.
      roundNum <- roundNum + 1
    
      # Every round, the numerator and denominator are multiplied by the scaler value.
        # If either of those values plus or minus one yield a value closer to the requested value,
          # num and denom are replaced with the multiplied and added/subtracted values.
          # If none of the four options yield a closer ratio, scaler is increased by 1.
      
      # t_num and t_denom are temporary/test values
      t_num <- scaler * num
      t_denom <- scaler * denom

      if(abs((t_num - 1) / t_denom - decimal) <
         abs(num/denom - decimal)){
        num <- t_num - 1
        denom <- t_denom
        scaler <- 1
        }else if(abs((t_num + 1) / t_denom - decimal) <
                 abs(num/denom - decimal)){
          num <- t_num + 1
          denom <- t_denom
          scaler <- 1
          }else if(abs(t_num / (t_denom - 1) - decimal) <
                   abs(num/denom - decimal)){
            num <- t_num
            denom <- t_denom - 1
            scaler <- 1
            }else if(abs(t_num / (t_denom + 1) - decimal) <
                     abs(num/denom - decimal)){
              num <- t_num
              denom <- t_denom + 1
              scaler <- 1
              }else{
                # If none of those values improved on the current approximation,
                  # increase the scaler value by 1
                scaler <- scaler + 1
              }
      
      # If denom is larger than it needs to be to get an exact match for the input,
        # shrink it to a power of 10 and change num to get that exact match.
      if(denom > 10^(significance + 1)){
        num <- round(decimal * 10^significance)
        denom <- 10^significance
      }
      
      if(num != 0 & scaler == 1){
        # This loop runs if the numerator is not zero and a better approximation was found this round
          # (including if the previous chunk ran and changed the value of denom again).
          # The loop's purpose is to reduce a fraction where num and denom share factors (e.g. 4/10)
        while(coprime(num, denom) == FALSE & num != denom){
          # A while loop is used because we're using the unique() function on the
            # prime factors of the numerator and denominator to avoid matching a
            # single factor in the denominator multiple times.
          common <- 1 # This variable will be the greatest common factor of num and denom.
          for(i in 1:length(unique(primes(num)))){
            if(unique(primes(num))[i] %in% primes(denom)){
              common <- common * unique(primes(num))[i]
            }
            }
          num <- num/common
          denom <- denom/common
        }
      }
      
      # If a new best approximation has been found and track = TRUE, print roundNum number and new fraction.
      if(scaler == 1){
        if(track == TRUE){
          print(noquote(paste(roundNum, ": ", num, "/", denom, sep="")))
        }
      }
    }
    
    }else{ #This loop runs iff a side is chosen.
      while((round(num/denom, digits = significance) != round(decimal, digits = significance) & roundNum < maxRounds) |
            (sign(num/denom - decimal) == -sign(side) & roundNum < maxRounds)){
        # This loop runs the exact same as the one above, except it will keep running
          # (up to "maxRounds" times) if the num/denom ratio is on the wrong side of the input number.
          # Fractions that are closer will only be used if they are on the proper side.
        # Any comments on this loop's functionality can be found in the matching line in the above loop.
        roundNum <- roundNum+1
        
        t_num <- scaler * num
        t_denom <- scaler * denom
        
        if(abs((t_num - 1) / t_denom - decimal) < abs(num/denom - decimal) &
           sign((t_num - 1)/t_denom - decimal) != -sign(side)){
          num <- t_num - 1
          denom <- t_denom
          scaler <- 1
          }else if(abs((t_num + 1)/t_denom - decimal) < abs(num/denom - decimal) &
                   sign((t_num + 1)/t_denom - decimal) != -sign(side)){
            num <- t_num + 1
            denom <- t_denom
            scaler <- 1
            }else if(abs(t_num/(t_denom - 1) - decimal) < abs(num/denom - decimal) &
                     sign(t_num/(t_denom - 1) - decimal) != -sign(side)){
              num <- t_num
              denom <- t_denom - 1
              scaler <- 1
              }else if(abs(t_num/(t_denom + 1) - decimal) < abs(num/denom - decimal) &
                       sign(t_num/(t_denom + 1) - decimal) != -sign(side)){
                num <- t_num
                denom <- t_denom + 1
                scaler <- 1
                }else{
                  scaler <- scaler + 1
                }
        
        if(denom > 10^(significance + 1)){
          num <- round(decimal * 10^significance)
          denom <- 10^significance
        }
        
        if(num != 0 & scaler == 1){
          while(coprime(num, denom) == FALSE & num != denom){
            common <- 1
            for(i in 1:length(unique(primes(num)))){
              if(unique(primes(num))[i] %in% primes(denom)){
                common <- common * unique(primes(num))[i]
              }
              }
            num <- num/common
            denom <- denom/common
          }
        }
        
        if(scaler == 1){
          if(track == TRUE){
            print(noquote(paste(roundNum, ": ", num, "/", denom, sep="")))
          }
        }
      }
      }

  if(side != 0 & sign(num/denom - decimal) == -sign(side)){
    # If the calculated ratio is on the wrong side of the input, a warning is produced.
    warning(paste("Maximum number of rounds (", maxRounds,
                  ") exceeded. Please provide starting numerator and denominator values with a ratio on the correct side of your decimal.", sep=""))
  }else if(roundNum >= maxRounds){
    #If the maximum number of permitted rounds is exceeded, this warning is printed. The output value will not have the requested significance.
    warning(paste('Maximum number of rounds (', maxRounds,
                  ') exceeded. Consider decreasing significance level (or increasing "maxRounds").', sep = ""))
  }
  
  options(scipen = 0, digits = 7) #Restore scientific notation settings to default.
  
  # The output of the function is a character vector containing the number of rounds,
    # the fraction approximation, and the decimal expression of that fraction.
    # as.numeric() can be used on the first or third elements of the output.
  return(noquote(c(as.character(roundNum),
                   paste(num, "/", denom, sep = ""),
                   as.character(round(num/denom, digits = significance + 3)))))
}


# The closerFrac function calculates values for side = -1, side = 0, and side = 1,
  # and returns whichever is closest to the input value.
  # Arguments (except side) can all be set as usual (the default for `track` is set to FALSE)
closerFrac <- function(decimal = 0, num = 1, denom = 2, significance = "default",
                       maxRounds=100000, track = FALSE){
  sideless <- FracApprox(decimal, num, denom, significance, side =  0, maxRounds, track)
  lower <-    FracApprox(decimal, num, denom, significance, side = -1, maxRounds, track)
  upper <-    FracApprox(decimal, num, denom, significance, side =  1, maxRounds, track)
  
  # answerIndex records which of the three results yields the closest approximation.
  answerIndex <- which.min(c(abs(as.numeric(sideless[3]) - decimal),
                             abs(as.numeric(  lower[3]) - decimal),
                             abs(as.numeric(  upper[3]) - decimal)))

  # The three response vectors are put into a list, and the one corresponding with answerIndex is output
  # The side (-1, 0, or 1) is appended as a character element to the appropriate vector.
  return(c(list(sideless, lower, upper)[[answerIndex]], as.character(c(0, -1, 1)[answerIndex])))
}


# The lowerFrac function works similarly to closerFrac, but outputs the approximation whose denominator is smallest.
  # The purpose of having this output is that the fractions can be easier to remember.
lowerFrac <- function(decimal = 0, num = 1, denom = 2, significance = "default",
                      maxRounds=100000, track = FALSE){
  sideless <- FracApprox(decimal, num, denom, significance, side =  0, maxRounds, track)
  lower <-    FracApprox(decimal, num, denom, significance, side = -1, maxRounds, track)
  upper <-    FracApprox(decimal, num, denom, significance, side =  1, maxRounds, track)
  
  # The code below is hard to parse; what it's doing is splitting the second element of the character vector (the fraction)
    # into its individual characters, removing the '/' character and anything before,
    # and then concatenating the remaining characters and converting them into a numeric.
  sideless_denom <- as.numeric(paste(strsplit(sideless[2], "")[[1]][-(1:which(strsplit(sideless[2], "")[[1]] == "/"))], collapse = ""))
  lower_denom <-    as.numeric(paste(strsplit(lower[2], "")[[1]][-(1:which(strsplit(lower[2], "")[[1]] == "/"))], collapse = ""))
  upper_denom <-    as.numeric(paste(strsplit(upper[2], "")[[1]][-(1:which(strsplit(upper[2], "")[[1]] == "/"))], collapse = ""))

  # Whichever of those resulting numbers is the smallest determines which approximation is used.
  answerIndex <- which.min(c(sideless_denom, lower_denom, upper_denom))
  
  # The output is in the exact same format as it is for the closerFrac function.
  return(c(list(sideless, lower, upper)[[answerIndex]], as.character(c(0,-1,1)[answerIndex])))
}
