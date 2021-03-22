#This implementation of the Collatz Sequence was one of the first programs I wrote in R, on the day I learned how to use loops.

collatz <- function(input, track = TRUE, chart = track, saveSequence = FALSE) {
  input.initial <- input #Input value will be changed, so save the original.
  collatz.sequence <- c(input.initial) #The input becomes the first element in the sequence.
  while(input != 1) {
    if(input %% 1 != 0) {
      stop("This function requires an integer input.")
    } else if(input < 1) {
      stop("Starting input must be a natural number.")
    } else if (input %% 2 == 0) {
      input <- input/2
    } else
      input <- 3*input + 1
    if(track == TRUE){ #If `track` is set to TRUE, print each iteration of the sequence.
      print(input)
      }
    collatz.sequence <- c(collatz.sequence, input)
  }
  
  if(saveSequence == TRUE){
    collatz.sequence <<- collatz.sequence
  }
  
  noquote(print(paste("Completed in", length(collatz.sequence) - 1, "steps.")))
  noquote(print(paste("Maximum value reached was ", max(collatz.sequence), ", which is ", round(max(collatz.sequence)/input.initial, 4), " times as large as the starting input of ", input.initial, ".", sep = "")))
  
  #By default, a the `chart` parameter is set equal to the input for `track`.
    #The appearance of the plot differs depending on the number of steps required.
    #It's plotted on a log scale so that each step can be clearly seen.
  if(chart == TRUE){
    if(length(collatz.sequence) > 50) {
      plot(y = log(collatz.sequence, 10), x = 0:(length(collatz.sequence) - 1), type = "l",
           xlab = "Steps", ylab = "Magnitude", main = paste("Collatz Progression of", input.initial))
      } else {
        plot(y = log(collatz.sequence, 10), x = 0:(length(collatz.sequence) - 1), pch = 20, type = "b",
             xlab = "Steps", ylab = "Magnitude", main = paste("Collatz Progression of", input.initial))      
      }
  }
  
  return(data.frame("Input" = input.initial, "Steps" = length(collatz.sequence) - 1, "Maximum" = max(collatz.sequence)))
}


#Examples

#The plot for a short sequence (length <= 50) has dots for every point
collatz(25)

#The plot for a longer sequence has only lines.
collatz(27)

#For lengthy sequences, it's best to set `track` to FALSE.
collatz(837799, track = FALSE, chart = TRUE, saveSequence = TRUE)
