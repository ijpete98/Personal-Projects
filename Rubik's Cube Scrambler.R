# cubeScramble takes a Rubik's cube scramble in standard notation, and outputs
  # a colored image representing the correct scrambled state
# Acceptable characters in the scramble are: 
  # The 6 faces:          U, D, L, R, F, and  B
  # Full cube rotations:  X, Y, and Z
  # Slice turns:          M, E, and S
  # Modifying characters: 2 and ' (apostrophe)
# The image produced is titled by default with a printout of the scramble.
  # An alternate title can be chosen by the user, or, if `title` = FALSE, no title will be added.

cubeScramble <- function(string = "ff'", title = TRUE){
  #Create initial, unscrambled state (W, M, & Y stand for White, Middle, & Yellow)
  {
  Wrow <- c(rep("", 3), rep("W", 3), rep("", 6))
  Mrow <- c(rep("O", 3), rep("G", 3), rep("R", 3), rep("B", 3))
  Yrow <- c(rep("", 3), rep("Y", 3), rep("", 6))
  
  rubiks_cube <- as.matrix(rbind(Wrow, Wrow, Wrow,
                                 Mrow, Mrow, Mrow,
                                 Yrow, Yrow, Yrow))
  }
  
  # This bracket manipulates the string to make it easily usable
  {
  # If scramble is put is as a vector of moves, concatenate to a single character string
  if(length(string) > 1){
    string <- paste(string, collapse = "")
  }
    
  # Remove any spaces
  if(" " %in% strsplit(string, "")[[1]]){
    string <- paste(strsplit(string, " ")[[1]], collapse = "")
  }
    
  # Capitalize the string and separate it to make a vector of individual letters and symbols.
  string <- toupper(strsplit(string, "")[[1]])
  }
  
  # Create a string to be used as the "title" of the image produced
  if(title == TRUE){
    titleString <- string
    i <- 2
    while(i <= length(titleString)){
      if(titleString[i] != "'" & titleString[i] != "2" & titleString[i] != " " & titleString[i - 1] != " "){
        titleString <- c(titleString[1:(i - 1)], " ", titleString[-(1:(i-1))])
        }
      i <- i + 1
      }
    titleString <- paste(titleString, collapse = "")
    }else if(title == FALSE){
      titleString <- NULL
      }else{
        titleString <- title
        }
  
  # Produce an error if the first instruction is one intended to modify previous instructions.
  if(string[1] %in% c("'", "2")){
    stop(paste('First character is \"', string[1], '\", which must be preceded by a face of the cube.', sep = ""))
  }
  
  # Change any modifying characters to an equivalent expression that does not use modifying characters.
  i <- 1
  while(i <= length(string)){ # Use a while loop here because length(string) will change
    
    # Nothing is modified unless the current character is "2" or an apostrophe.
    if(string[i] %in% c("2", "'")){
      
      if(i < length(string)){
        # If there are consecutive instances of "'" or "2", print an error.
        if(string[i + 1] %in% c("2", "'")){
          stop(noquote('Scramble cannot contain consecutive \"2\" or \"\'\" characters.'))
        }
      }
      
      # Replace a "2" with a duplicate of the character preceding it,
        # and a "'" with two such duplicates (a counterclockwise turn is the same as 3 clockwise.)
      if(string[i] == "2"){
        string[i] <- string[i - 1]
        }else if(string[i] == "'"){
          if(i < length(string)){
            # If this apostrophe is the last character, the `if` statement
              # prevents attempting to append nonexistant characters.
            string <- c(string[1:(i-1)], rep(string[i-1], 2), string[(i+1):length(string)])
            }else{
              string <- c(string[1:(i-1)], rep(string[i-1], 2))
            }
        }
      }
    i <- i + 1
  }

  # Convert full cube rotations into two face rotations and a slice turn
  i <- 1
  while(i <= length(string)){
    if(i == 1 & length(string) > 1){
      if(string[i] == "X"){
        string <- c(rep("M", 3), rep("L", 3), "R", string[2:length(string)])
      }else if(string[i] == "Y"){
        string <- c(rep("E", 3), rep("D", 3), "U", string[2:length(string)])
      }else if(string[i] == "Z"){
        string <- c("F", "S", rep("B", 3), string[2:length(string)])
      }
    }else if(i == 1){ # If the rotation is the only move, replace it without 
      if(string[i] == "X"){
        string <- c(rep("M", 3), rep("L", 3), "R")
      }else if(string[i] == "Y"){
        string <- c(rep("E", 3), rep("D", 3), "U")
      }else if(string[i] == "Z"){
        string <- c("F", "S", rep("B", 3))
      }
    }else if(i < length(string)){
      if(string[i] == "X"){
        string <- c(string[1:(i - 1)], rep("M", 3), rep("L", 3), "R", string[(i + 1):length(string)])
      }else if(string[i] == "Y"){
        string <- c(string[1:(i - 1)], rep("E", 3), rep("D", 3), "U", string[(i + 1):length(string)])
      }else if(string[i] == "Z"){
        string <- c(string[1:(i - 1)], "F", "S", rep("B", 3), string[(i + 1):length(string)])
      }
    }else{
      if(string[i] == "X"){
        string <- c(string[1:(i - 1)], rep("M", 3), rep("L", 3), "R")
      }else if(string[i] == "Y"){
        string <- c(string[1:(i - 1)], rep("E", 3), rep("D", 3), "U")
      }else if(string[i] == "Z"){
        string <- c(string[1:(i - 1)], "F", "S", rep("B", 3))
      }
    }
    i <- i + 1
  }
  
  # Now that the string contains no modifier characters, scramble the cube.
  # For each move made, a copy of the current cube state is created.
    # It is modified according to the move made, then becomes the new current state.
  # If any character is invalid (not U, D, L, R, F, B, M, E, or S), an error results.
  for(i in 1:length(string)){
    tempCube <- rubiks_cube
    move <- string[i]
    if(move == "U"){
      for(j in 1:12){ #layer
        tempCube[4, j] <- rubiks_cube[4, (j + 2) %% 12 + 1]
      }
      { #Edges
      tempCube[1, 5] <- rubiks_cube[2, 4]
      tempCube[2, 6] <- rubiks_cube[1, 5]
      tempCube[3, 5] <- rubiks_cube[2, 6]
      tempCube[2, 4] <- rubiks_cube[3, 5]}
      { #Corners
        tempCube[1, 4] <- rubiks_cube[3, 4]
        tempCube[1, 6] <- rubiks_cube[1, 4]
        tempCube[3, 6] <- rubiks_cube[1, 6]
        tempCube[3, 4] <- rubiks_cube[3, 6]
      }
    }else if(move == "D"){
      for(j in 1:12){
        tempCube[6, j] <- rubiks_cube[6, (j - 4) %% 12 + 1]
      }
      { #Edges
        tempCube[7, 5] <- rubiks_cube[8, 4]
        tempCube[8, 6] <- rubiks_cube[7, 5]
        tempCube[9, 5] <- rubiks_cube[8, 6]
        tempCube[8, 4] <- rubiks_cube[9, 5]}
      { #Corners
        tempCube[7, 4] <- rubiks_cube[9, 4]
        tempCube[7, 6] <- rubiks_cube[7, 4]
        tempCube[9, 6] <- rubiks_cube[7, 6]
        tempCube[9, 4] <- rubiks_cube[9, 6]
      }
    }else if(move == "R"){
      { #layer
        tempCube[1:6, 6] <- rubiks_cube[4:9, 6]
        tempCube[7:9, 6] <- rubiks_cube[6:4, 10]
        tempCube[6:4, 10] <- rubiks_cube[1:3, 6]}
      { #Edges
        tempCube[4, 8] <- rubiks_cube[5, 7]
        tempCube[5, 9] <- rubiks_cube[4, 8]
        tempCube[6, 8] <- rubiks_cube[5, 9]
        tempCube[5, 7] <- rubiks_cube[6, 8]}
      { #Corners
        tempCube[4, 7] <- rubiks_cube[6, 7]
        tempCube[4, 9] <- rubiks_cube[4, 7]
        tempCube[6, 9] <- rubiks_cube[4, 9]
        tempCube[6, 7] <- rubiks_cube[6, 9]}
    }else if(move == "L"){
      { #layer
        tempCube[9:4, 4] <- rubiks_cube[6:1, 4]
        tempCube[3:1, 4] <- rubiks_cube[4:6, 12]
        tempCube[4:6, 12] <- rubiks_cube[9:7, 4]
      }
      { #Edges
        tempCube[4, 2] <- rubiks_cube[5, 1]
        tempCube[5, 3] <- rubiks_cube[4, 2]
        tempCube[6, 2] <- rubiks_cube[5, 3]
        tempCube[5, 1] <- rubiks_cube[6, 2]}
      { #Corners
        tempCube[4, 1] <- rubiks_cube[6, 1]
        tempCube[4, 3] <- rubiks_cube[4, 1]
        tempCube[6, 3] <- rubiks_cube[4, 3]
        tempCube[6, 1] <- rubiks_cube[6, 3]}
    }else if(move == "F"){
      { #layer
        for(k in 1:3){
        tempCube[7, 7 - k] <- rubiks_cube[k + 3, 7]
        tempCube[7 - k, 3] <- rubiks_cube[7, 7 - k]
        tempCube[3, k + 3] <- rubiks_cube[7 - k, 3]
        tempCube[k + 3, 7] <- rubiks_cube[3, k + 3]}}
      { #edges
        tempCube[4, 5] <- rubiks_cube[5, 4]
        tempCube[5, 6] <- rubiks_cube[4, 5]
        tempCube[6, 5] <- rubiks_cube[5, 6]
        tempCube[5, 4] <- rubiks_cube[6, 5]}
      { #corners
        tempCube[4, 4] <- rubiks_cube[6, 4]
        tempCube[4, 6] <- rubiks_cube[4, 4]
        tempCube[6, 6] <- rubiks_cube[4, 6]
        tempCube[6, 4] <- rubiks_cube[6, 6]}
    }else if(move == "B"){
      { #layer
        for(k in 1:3){
          tempCube[k + 3, 1] <- rubiks_cube[1, 7 - k]
          tempCube[1, k + 3] <- rubiks_cube[k + 3, 9]
          tempCube[k + 3, 9] <- rubiks_cube[9, 7 - k]
          tempCube[9, 7 - k] <- rubiks_cube[7 - k, 1]}}
      { #edges
        tempCube[4, 11] <- rubiks_cube[5, 10]
        tempCube[5, 12] <- rubiks_cube[4, 11]
        tempCube[6, 11] <- rubiks_cube[5, 12]
        tempCube[5, 10] <- rubiks_cube[6, 11]}
      { #corners
        tempCube[4, 10] <- rubiks_cube[6, 10]
        tempCube[4, 12] <- rubiks_cube[4, 10]
        tempCube[6, 12] <- rubiks_cube[4, 12]
        tempCube[6, 10] <- rubiks_cube[6, 12]
        
        }
    }else if(move == "M"){
      tempCube[9:4, 5] <- rubiks_cube[6:1, 5]
      tempCube[3:1, 5] <- rubiks_cube[4:6, 11]
      tempCube[4:6, 11] <- rubiks_cube[9:7, 5]
    }else if(move == "E"){
      for(j in 1:12){
        tempCube[5, j] <- rubiks_cube[5, (j - 4) %% 12 + 1]
      }
    }else if(move == "S"){
      for(k in 1:3){
        tempCube[8, 7 - k] <- rubiks_cube[k + 3, 8]
        tempCube[7 - k, 2] <- rubiks_cube[8, 7 - k]
        tempCube[2, k + 3] <- rubiks_cube[7 - k, 2]
        tempCube[k + 3, 8] <- rubiks_cube[2, k + 3]}
    }else{
      stop(paste('Invalid character entered: "', move, '"', sep = ""))
    }
    
    rubiks_cube <- tempCube
  }
  
  # Generate values to be used in the `image` function
  {
    x <- 1:12
    y <- 1:9
    
    # Fill a matrix with values 1 to 7, corresponding with each letter used in the cube matrix.
    colors_matrix <- matrix(data = rep(0, 108), nrow = 12)
    for(i in 1:12){
      for(j in 1:9){
        # "10 - j" is used because the image produced is vertically mirrored otherwise
        colors_matrix[i, j] <- which(rubiks_cube[10 - j, i] == c("", "W", "O", "G", "R", "B", "Y"))
      }
    }
  }
  
  return(image(x, y, colors_matrix,
               col = c("darkgray", "white", "orange", "limegreen", "red", "blue", "yellow2"),
               axes = FALSE, xlab = NA, ylab = NA, asp = 1, main = titleString))

}



# EXAMPLES

# 6 Centers
cubeScramble("F' B R' L U' D F' B")

# Superflip (using slice moves and rotations)
cubeScramble("X2 S U B2 D2 M D' M2 S U R2 D M2 U B2 U S2 Y",
             title = "Superflip")

# Yusheng Du's world record scramble
cubeScramble("F U2 L2 B2 F' U L2 U R2 D2 L' B L2 B' R2 U2",
             title = "World Record Scramble (3.47 Seconds)")

# World Record Solve added to the above scramble (image has no title)
cubeScramble(paste("F U2 L2 B2 F' U L2 U R2 D2 L' B L2 B' R2 U2",
                   "Z Y",                      # Inspection rotations
                   "U R2 U' F' L F' U' L'",    # xxCross
                   "U' R U R2 U R U2 R' U R",  # Last 2 F2L pairs
                   "U R' U' R U' R' U2 R U"    # LL
                   ), title = FALSE)
