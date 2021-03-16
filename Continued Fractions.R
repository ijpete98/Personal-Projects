#Function to create a continued fraction with a predetermined max number of levels.
  #For a rational number, 10 levels will typically be enough to give an exact result.
createContFrac <- function(decimal, levels = 10){
  
  #Create rounds variable to compare to `levels` parameter input.
  rounds <- 0
  
  #Establish initial values of the produced vector and remainder variable (this will be used and changed in the loop)
  out_vec <- floor(decimal) # The first element of out_vec is the whole number component of the input.
  remainder <- decimal %% 1
  
  #The remainder doesn't have to be zero, because the way that R rounds values results in problems with that.
  while(rounds < levels & remainder > 10^-6){
    out_vec <- append(out_vec, floor(1/remainder))
    remainder <- (1/remainder) %% 1
    rounds <- rounds + 1
  }
  
  #If the last number in the sequence is one, remove it and add 1 to the new final value.
  if(length(out_vec) > 1 & out_vec[length(out_vec)] == 1){
    out_vec <- c(out_vec[1:(length(out_vec) - 2)], out_vec[(length(out_vec) - 1)] + 1)
  }
  
  return(out_vec)
}


#Function to convert a continued fraction vector (with whole number component) to a decimal
ContFracToDecimal <- function(CFvec){
  
  #If the vector contains any non-positive values (except for the first value) or non-integers, print an error.
  if((sum(CFvec %% 1 == 0) + sum(CFvec[-1] > 0)) < (2*length(CFvec) - 1)){
    stop("All vector elements must be positive whole numbers.")
  }
  
  #Go through the vector backwards to get all the nestings right.
  if(length(CFvec) > 1){
    result <- 1/CFvec[length(CFvec)]
  }else{
    #This is here so that length-one vectors print the correct value.
    result <- 0
  }
  if(length(CFvec) > 2){
    for(i in (length(CFvec)-1):2){
      result <- 1/(CFvec[i] + result)
    }
  }
  
  #Add the first vector element to the produced decimal (or zero, if length-one) to get the final answer.
  result <- CFvec[1] + result
  return(result)
}



#Function to convert a continued fraction into a normal fraction
simplifyContFrac <- function(CFvec){
  
  #If the vector contains any non-positive values (except for the first value) or non-integers, print an error.
  if((sum(CFvec %% 1 == 0) + sum(CFvec[-1] > 0)) < (2*length(CFvec) - 1)){
    stop("All vector elements must be positive whole numbers.")
  }
  
  if(length(CFvec) == 1){ #If the input is a single number X, return "X/1"
    return(noquote(paste(CFvec, "/", 1, sep = "")))
  }
  
  if(length(CFvec) == 2){ #If the input has 2 numbers X and Y, return (1 + XY)/Y 
    return(noquote(paste(1 + prod(CFvec), "/", CFvec[2], sep = "")))
  }
  
  num <- 1
  denom <- CFvec[length(CFvec)]
  for(i in (length(CFvec)-1):2){
    num <- CFvec[i]*denom + num
    num2 <- denom
    denom <- num
    num <- num2
  }
  num <- CFvec[1]*denom + num
  return(noquote(paste(num, "/", denom, sep = "")))
}



#Examples
createContFrac(pi)
simplifyContFrac(c(3,7,16))
ContFracToDecimal(c(3,7,16))
