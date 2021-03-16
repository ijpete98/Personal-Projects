# Inspired by the Numberphile Video titled "What's special about 277777788888899?"


# The persistence function iteratively multiplies the digits in the base 10
  # expression of a non-negatve integer until the result is a one-digit number.

# If the parameter `track` is set to `TRUE` (which it is by default),
  # each product is printed and a summary is generated at the end.

# If `track == FALSE`, the return of the function is only the number of iterations.

persistence <- function(x, track = TRUE){
  if(class(x) %in% c("numeric", "integer") == FALSE | x %%1 != 0 | x < 0){
    stop("Input must be a whole number.")
  }
  initial.x <- x  # input will be changed, so save the initial value.
  if(track == TRUE){
    print(x)
  }
  steps <- 0
  
  # While loop runs as long as x is not a single-digit number.
  while(x > 9){
    digits <- strsplit(as.character(x), "")[[1]] # Split x into its individual digits.
    x <- prod(as.numeric(digits)) # Multiply digits of x.
    steps <- steps + 1 # Increment step count.
    if(track == TRUE){
      print(x)
    }
  }
  
  if(track == TRUE){
    paste(initial.x, " has a multiplicative persistence of ", steps, ".", sep = "")
  }else{
    return(steps)
  }
}


# This function generates a table of persistence values up to a given number,
  # but only includes input values for numbers whose product is not also a product
  # of any smaller number (the selection process is explained in the comments.)
persistenceTable <- function(upperlimit, lowerlimit = 1){
  DegreeOfPersistence <<- data.frame() #create table
  
  # The for loop runs using every value between the limits selected
  for(i in lowerlimit:upperlimit){
    # Create a vector containing the digits of the number currently being used
    rowdigits <- strsplit(as.character(i), "")[[1]]
    
    if(i %in% c(1, 10)){
      # 1 and 10 would be excluded by the next if statement,
        # so they are checked for manually and included.
      DegreeOfPersistence <<- rbind(DegreeOfPersistence,c(i, persistence(i, track = FALSE)))
      
    }else if("0" %in% rowdigits | "1" %in% rowdigits |
             sum(rowdigits == "2") + sum(rowdigits == "4") + sum(rowdigits == "3") > 1 |
             sum(rowdigits == "3") + sum(rowdigits == "6") > 1 |
             sum(rowdigits == "4") + sum(rowdigits == "6") > 1){
      # The logic beind the above statement is as follows:
        # If a number contains a zero digit, the product will be zero, so the number should not be used.
        # If a number contains a one digit, it could be removed and yield the same product.
        # For each of the following digit combinations, there is a way to
          # get the same product with fewer or smaller digits:
          # 2*2, 2*3, 2*4, 3*3, 3*4, 3*6, 4*4, 4*6.
      next
    }else if(sum(rowdigits[order(rowdigits)]==rowdigits)<length(rowdigits)){
      # If the digits are not in increasing order, there is a smaller number with the same digits.
      next
    }else{
      DegreeOfPersistence <<- rbind(DegreeOfPersistence,c(i, persistence(i, track = FALSE)))
    }
  }
  names(DegreeOfPersistence) <<- c("Input","Persistence")
}

