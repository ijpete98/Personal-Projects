# Function to take a base 10 integer and add it to its own reversed digits.
reverse.sum <- function(x) {
  if(class(x) != "numeric" | x %% 1 != 0){
    stop("Input must be a whole number.")
  }
  characters <- strsplit(as.character(x), "")[[1]]
  reversed <- paste(rev(characters), collapse = "")
  return(as.numeric(x) + as.numeric(reversed))
}


# Function to iterate reverse.sum() until a palindrome is reached or the result grows too large.
  # For background information, see the Wikipedia page on "Lychrel Numbers"
  # To see results for `report = TRUE` and `report = FALSE``, see end of document.

palindrome <- function(x, report = FALSE) {
  initial.x <- x  #Input value will be changed, so we save the inital value.
  steps <- 0  #For use when `report = TRUE`, track the number of iterations.
  if(report == TRUE){
    print(x)
  }
  
  # This while loop will iterate as long as x is not a palindrome.
  while(paste(rev(strsplit(as.character(x), "")[[1]]), collapse = "") != x) {
    if(x > 10^19) {
      # If x exceeds 10^19, cease calculation and print an error.
      stop(paste("Sum exceeded 10^19 after", steps, "steps."))
      break
    } else {
      x <- reverse.sum(x) # Iterate x
      steps <- steps + 1  # Iterate step count
      if(report == TRUE){
        print(x)
      }
    }
    }
  
  #Outputs depend on value of `report` (FALSE by default)
  if(report == TRUE){
    if (steps != 1 & steps != 0) {
      return(print(paste("Calculation completed in ", steps, " steps. ", x, " is ", round(x/initial.x, 4), " times larger than the starting number ", initial.x, ".", sep = "")))
      } else if(steps == 1){
        return(print(paste("Calculation completed in 1 step. ",
                           x, " is ", round(x/initial.x, 4),
                           " times larger than the starting number ",
                           initial.x, ".", sep = "")))
        }else{
          return(print(paste("Input of", initial.x, "is already a palindrome.")))
          }
    }else{
      return(x)
    }
  }



# Examples

palindrome(2718)
# Outputs final result

palindrome(2718, report = TRUE)
# Outputs each iteration and prints a summary (has a special case for palindrome inputs)

palindrome(196)
# 196 is believed to be a Lychrel Number, so an error is printed.
