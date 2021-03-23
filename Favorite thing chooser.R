# The functions created in this document allow a user to rank, from best to worst,
  # any number of elements (possibly all of them) in a vector of objects.
  # The vector must be called "objectsToOrder", because having its name called
    # in the function arguments would make the function slower for a user to run repeatedly.
  # Rather than working like a tournament bracket, objects that are outranked are not totally eliminated,
    # but are instead removed from the running only until all objects that outranked them have themselves been ranked.
    # This means that even if #1 eliminates #2 early on, #2 can still be ranked in its rightful place.

# If you want to understand what the functions do, I recommend skipping to line 397 and
  # reading the descriptions in the second set of brackets. Then you can take a swing at
  # using the functions with a small, simple set of objects to rank.


# This bracket creates the functions that we use (nine of them).
  # Their purposes are best understood through experience with using them.
{
#resetFavorites function resets "outclassed" and "ranked" data frames,
  # and deletes "remaining" and "untouched" vectors (these are created again
  # upon running the "selected" function for the first time)
resetFavorites <- function(){
  outclassed <<- data.frame("object"=NA, "index"=NA, "by"=NA, "byindex"=NA)[-1,]
  ranked <<- data.frame("rank"=NA, "object"=NA, "index"=NA)[-1,]
  if(exists("remaining") == TRUE){
    remove(remaining, envir = .GlobalEnv)
    remove(untouched, envir = .GlobalEnv)
    remove(justPassed, envir = .GlobalEnv)
  }
}


# selected() is the main function that users will interface with. 
selected <- function(a0=NA,a1=NA,a2=NA,a3=NA,a4=NA,a5=NA,a6=NA,a7=NA,a8=NA,a9=NA,
                     minNum = parameters[1], maxNum = parameters[2],
                     fractionToShow = parameters[3], indexOrder = parameters[4], sameLevel=parameters[5]){

  # The first time the function is run, it ignores all of its arguments and instead
    # copies objectsToOrder to the `remaining` vector, then runs the `indexGenerator` function.
  if(exists("remaining") == FALSE){
    remaining <<- 1:length(objectsToOrder)
    justPassed <<- FALSE
    return(indexGenerator(minNum,maxNum,fractionToShow,indexOrder,sameLevel))
  }
  
  # a0 through a9 are the indexes of the objects that were chosen by the user.
    # By default, they are all set to NA so that the user can choose as few elements as they want.
  # These two lines remove any duplicates and NAs from the user's selection.
  favorites <<- unique(c(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9))
  favorites <<- favorites[is.na(favorites)==FALSE]
  
  # If "selected(0)" is run, print "PASS" and produce a new group of objects to choose from.
  if(length(favorites) == 1 & favorites[1] == 0){
    print("PASS")
    justPassed <<- TRUE
    return(indexGenerator(minNum, maxNum, fractionToShow, indexOrder, sameLevel))
  }
  
  justPassed <<- FALSE
  
  #If invalid numbers are selected, print an error:
  if(sum(favorites %in% 1:length(indexes)) < length(favorites)){
    stop(paste("Invalid number(s) selected: ", favorites[favorites %in% 1:length(indexes) == FALSE]))
  }
  
  # If all presented objects are chosen, print an error, and
    # prompt the user to PASS or to select again from the same set of objects.
  if(length(favorites) == length(indexes)){
    stop('You have selected every option. Please deselect some, or use "selected(0)" to generate a new set to choose from.')
  }
  
  # Identify objects that were NOT selected.
  nonFavorites <- (1:length(indexes))[-favorites]
  
  #Add all new comparisons to the "outclassed" table.
  for(i in nonFavorites){
    for(j in favorites){
      outclassed <<- rbind(outclassed,
                           data.frame("object" = objectsToOrder[remaining[indexes[i]]],
                                      "index" = remaining[indexes[i]],
                                      "by" = objectsToOrder[remaining[indexes[j]]],
                                      "byindex" = remaining[indexes[j]]))
    }
  }
  
  # Remove outclassed objects from the "remaining" vector by populating that vector with
    # all objects that are in neither the final rankings nor in the table of eliminated objects
    # (This assignment is copied and used several times in multiple functions)
  remaining <<- (1:length(objectsToOrder))[(1:length(objectsToOrder) %in% ranked$index == FALSE) +
                                             (1:length(objectsToOrder) %in% outclassed$index == FALSE) == 2]

  # If there is only one object in the "remaining" vector, rank it, delete all comparisons involving it,
    # reinstate anything that only it eliminated, and repeat if necessary.
  while(length(remaining) == 1){
    # Rank it
    ranked <<- rbind(ranked,
                     data.frame("rank" = dim(ranked)[1] + 1,
                                "object" = objectsToOrder[remaining[1]],
                                "index"=remaining[1]))
    
    # Remove relevant comparisons
    outclassed <<- outclassed[outclassed$byindex != remaining[1],]

    # Identify objects that should be in `remaining` vector (same way as before)
    remaining <<- (1:length(objectsToOrder))[(1:length(objectsToOrder) %in% ranked$index == FALSE) +
                                               (1:length(objectsToOrder) %in% outclassed$index == FALSE)==2]
  }
  
  #If there are no objects left to rank:
  if(length(remaining) == 0){
    #If there are no comparisons in the "outclassed" table, print a completion message:
    if(dim(outclassed)[1] == 0){
      if(parameters[5] == TRUE){
        untouched <<- as.numeric(c())
      return(noquote('All objects have been ranked. They can be viewed in the "ranked" dataframe. To reset rankings, run resetFavorites()'))
      }
    }else{ #If there are comparisons still in the table (which should never happen), remove all of them that involve already ranked objects.
      fixUnranked()
    }
  }
  
  #If there are still comparisons to be made, produce a group of objects to compare.
  indexGenerator(minNum, maxNum, fractionToShow, indexOrder, sameLevel)
}


# This `indexGenerator` function is called by the `selected` function and
  # shouldn't be directly called by a user.
  # It's responsible for most of the behind-the-scenes computation.
indexGenerator <- function(minNum, maxNum, fractionToShow, indexOrder, sameLevel){
  # If anyone puts in bad numbers for min & max, this bracketed code will fix it:
  {
  # Make sure both values are non-negative integers
  minNum <- round(abs(minNum))
  maxNum <- round(abs(maxNum))
  # Make sure the values are ordered properly
  if(minNum > maxNum){
    parameters <<- parameters[c(2,1,3,4,5)]
    warning("minNum value provided was larger than maxNum. The two values were automatically swapped")
  }
  }
  
  # This bracket determines how many objects will be presented for selection
  {
  # Initially set the number to the number remaining divided by the fractionToShow value.
    # Then adjust that value if it's outside of the range of minNum and maxNum.
  num <- round(length(remaining)/fractionToShow)
    if(num < minNum) {num <- minNum}
    if(num > maxNum) {num <- maxNum}
    if(num > length(remaining)) {num <- length(remaining)}
  }
  
  # Create a sample of indexes to pull from the "remaining" vector:
  {
  if(sameLevel == FALSE){
    # If sameLevel is FALSE, we don't care which indexes we use as long as
      # the corresponding objects are neither already ranked nor outclassed.
    indexes <<- sample(1:length(remaining),min(c(num,length(remaining))))
  }else{ #This will do the same thing as the previous line, but be pickier about which indexes it can select.
    # Any objects that are still in the running but have eliminated nothing are put in the "untouched" vector.
      # These will be the first indexes chosen from (four lines from now).
    untouched <<- remaining[remaining %in% outclassed$byindex == FALSE]
    indexes <<- c()
    if(length(untouched) != 0){
      indexes <<- which(remaining %in% untouched[sample(1:length(untouched), min(c(num, length(untouched))))])
    }
    
    # Create vector to tally for each remaining object how many objects it has eliminated.
    superseding_count <<- c()
    for(i in 1:length(remaining)){
      superseding_count <<- append(superseding_count, sum(outclassed$byindex == remaining[i]))
    }
    
    countup <- 1
    while(length(indexes) < num){
      # If there is an object that has eliminated the proper amount of other objects, pull from 
        # such objects until we have enough indexes, or all with that elimination value are used
        # (in which case we increment the number by one).
      if(countup %in% superseding_count){
        newinds <- sample(1:sum(superseding_count == countup),
                          min(c(num - length(indexes), sum(superseding_count == countup))))
                          # The `min` function is used in the line above to make sure we don't select too many indexes.
        newinds <- which(superseding_count == countup)[newinds]
        indexes <<- append(indexes, newinds)
      }
      countup <- countup + 1
    }
    indexes <<- sample(indexes) #Randomly order the indexes.
  }
  }
  
  #If the last selection was a PASS and the number of selectable indexes is low
    # due to "sameLevel == TRUE", it is likely that a new selection of indexes
    # will contain many of the same elements that were just PASS-ed on;
    # This does not result in a positive experience for the user.
    # Therefore, the following bracket of code helps fix that problem.
  if(sameLevel == TRUE & justPassed == TRUE){
    # The bracket below calculates the number of indexes that can actually be selected, assuming "sameLevel == TRUE"
    {
    num_valid_inds <- sum(superseding_count == min(superseding_count))
    numtosum <- min(superseding_count)
    while(num_valid_inds < num){
      numtosum <- numtosum + 1
      num_valid_inds <- num_valid_inds + sum(superseding_count == numtosum)
    }
    }
    
    # If the sum is too small (i.e. the selection is too narrow), generate a new set of indexes.
      # This sample is a basic sample from ALL elements in the `remaining` vector (as if `sameLevel` was set to FALSE)
    if(num_valid_inds <= (2.5 * num)){
      indexes <<- sample(1:length(remaining), min(c(num, length(remaining))))
    }
  }
  
  # If parameters[4] is TRUE, order the indexes.
    # Doing this doesn't alter how the functions run, and has few practical uses.
  if(indexOrder == TRUE){
    indexes <<- indexes[order(indexes)]
  }
  
  # The output of this function is a printed data frame containing the names of
    # the objects to be ranked, and numbers to identify their position
  data.frame(ind = 1:min(c(num, length(remaining))),
             object = objectsToOrder[remaining[indexes]])
}


# If an element that has already been ranked is still marked as eliminating other objects,
  # this function will remove such comparisons from the "outclassed" data frame.
  # It should be noted that this situation should only ever come up if the rankings are
    # altered outside of the `selected` function (e.g. using the `manuallyRank` function)
fixUnranked <- function(){
  for(i in 1:dim(ranked)[1]){
    outclassed <<- outclassed[outclassed$byindex != ranked$index]
  }
  remaining <<- (1:length(objectsToOrder))[(1:length(objectsToOrder) %in% ranked$index == FALSE) +
                                             (1:length(objectsToOrder) %in% outclassed$index == FALSE)==2]
}


# If a user wants to remove a comparison from the "outclassed" data frame, they can use
  # this function, with the indexes of the eliminated and eliminating objects, to do so.
outclassedUndo <- function(index, byindex){
  outclassed <<- outclassed[(outclassed$index == index & outclassed$byindex == byindex) == FALSE, ]
  remaining <<- (1:length(objectsToOrder))[(1:length(objectsToOrder) %in% ranked$index == FALSE) +
                                             (1:length(objectsToOrder) %in% outclassed$index == FALSE)==2]
  selected(0)
}


# Immediately rank any object (by its name or its index in objectsToOrder).
  # If it is currently elimanated by another object, it will still be ranked,
  # but a warning will be shown.
manuallyRank <- function(object){
  if(length(object) > 1){
    stop("If you have multiple objects to manually rank, you must run them through this function one at a time.")
  }
  # Check to see if the object is already ranked
  if(object %in% ranked$index | object %in% ranked$object){
    stop("Selected object is alredy ranked")
  }
  
  # Check to see if index or object name is valid before attempting to add it to the rankings.
  if(object %in% objectsToOrder | object %in% 1:length(objectsToOrder)){
    # If the index of the object was used, run this bracket:
    if(class(object) == "numeric"){
      # If the object is outclassed, print the warning before adding it to the rankings.
      if(object %in% outclassed$index){
        warning(paste("Object number ", object, " (", objectsToOrder[object],
                    ") was previously outlcassed by at least ", sum(outclassed$index == object),
                    " other objects. These comparisons have been permanently removed from the outclassed table.",
                    sep = " "))
        outclassed <<- subset(outclassed, index != object)
        }
      ranked <<- rbind(ranked,
                       data.frame("rank" = dim(ranked)[1] + 1,
                                  "object" = objectsToOrder[object],
                                  "index" = object))
      }else{
        # If the object is outclassed, print the warning before adding it to the rankings.
        if(object %in% outclassed$object){
          warning(paste(object, " (object number ", which(objectsToOrder == object),
                      ") was previously outclassed by at least ", sum(outclassed$object == object),
                      " other objects. These comparisons have been permanently removed from the outclassed table.",
                      sep = " "))
        outclassed <<- subset(outclassed, object != object)
      }
      ranked <<- rbind(ranked,
                       data.frame("rank" = dim(ranked)[1] + 1,
                                  "object" = object,
                                  "index" = which(objectsToOrder == object)))
    }
  if(dim(outclassed)[1] > 0){
    # Use the `fixUnranked` function to remove any objects that were outclassed by the newly ranked object.
    fixUnranked()
  }
  # Finish by generating a new set of objects to select from.
  selected(0)
  }else{
    stop("Invalid object selected.")
  }
}


# The `manualOutclassed` function allows the user to quickly establish comparisons between many
  # objects' indexes, without having to wait for them to be selected by the indexGenerator.
manualOutclassed <- function(better, worse){
  # If the `better` and `worse` vectors share elements, print an error that identifies the shared elements.
  if(sum(better %in% worse) != 0){
    stop(paste("The following objects are listed in both arguments:", better[better %in% worse]))
  }
  
  # Any indexes that are already ranked are removed from the vectors
  better <- better[better %in% ranked$index == FALSE]
  worse <- worse[worse %in% ranked$index == FALSE]
  
  # For every combination of an element from `better` and an element from `worse`,
    # add a row with that combination to the `outclassed` data frame.
  for(i in better){
    for(j in worse){
      # The row will only be added if a comparison involving the two objects isn't already in the table.
        # This is to avoid duplicate rows, or the possibility of two objects mutually eliminating each-other.
      if(dim(subset(outclassed, index == j & byindex == i))[1] == 0 &
         dim(subset(outclassed, index == i & byindex == j))[1] == 0){
        outclassed <<- rbind(outclassed,
                             data.frame("object" = objectsToOrder[j],
                                        "index" = j,
                                        "by" = objectsToOrder[i],
                                        "byindex" = i))
      }
    }
  }
  
  # Remove from the `remaining` vector any newly outranked objects
  remaining <<- remaining[remaining %in% worse == FALSE]
  
  # If there is only one object in the "remaining" vector, rank it,
    # delete all comparisons involving it, reinstate anything that only it eliminated, and repeat if necessary.
    # (This loop works the same as the loop that does the same thing in the `selected` function.)
  while(length(remaining) == 1){
    ranked <<- rbind(ranked,
                     data.frame("rank" = dim(ranked)[1] + 1,
                                "object" = objectsToOrder[remaining[1]],
                                "index" = remaining[1]))
    outclassed <<- outclassed[outclassed$byindex != remaining[1],]
    remaining <<- (1:length(objectsToOrder))[(1:length(objectsToOrder) %in% ranked$index == FALSE) +
                                               (1:length(objectsToOrder) %in% outclassed$index == FALSE) == 2]
  }
  
  # Resolve any issues that may have resulted from the above while loop.
  if(dim(ranked)[1] != 0 & dim(outclassed)[1] > 0){
    fixUnranked()
  }
  
  # Generate a new set of objects for the user to select from
  selected(0)
}  


# Function to remove a single object that has already been ranked from the rankings.
  # This function could be called by users, but is here mostly to be called by the `removeRanked` function.
SingleRankRemove <- function(rankValue){
  if(dim(ranked)[1] < rankValue){
    stop("Number specified is larger than total number of currently ranked objects.")
  }
  remaining <<- append(remaining, ranked$index[rankValue])
  remaining <<- remaining[order(remaining)]
  
  if(parameters[5] == TRUE){
    untouched <- append(untouched, ranked$index[rankValue])
  }
  
  ranked <<- ranked[-rankValue,]
  
  # If there were objects ranked below the removed object, correct their rank numbers (subtract 1)
  if(dim(ranked)[1] >= rankValue){
    ranked[rankValue:dim(ranked)[1], 1] <<- rankValue:dim(ranked)[1]
  }
}

removeRanked <- function(ranksvector){
  # Order the objects to remove from lowest rank to highest (i.e. biggest number to smallest)
    # This will prevent incorrect objects from being removed due to rank numbers changing.
  ranksvector <- ranksvector[order(ranksvector, decreasing = TRUE)]
  for(i in ranksvector)
    SingleRankRemove(i)
  if(length(remaining) == 1){
      warning('The single requested object was removed from a completed table, and so is the only currently unranked object.\nIf you would like to place it in last place on the ranked table, you can use the command "manuallyRank(index)".\nOtherwise, continue to remove ranked objects from the table until all objects that you would like to compare are again unranked.')
  }else{
    selected(0)
  }
}

# This is here to create the `outclassed` and `ranked` data frames alongside the functions that are read in.
resetFavorites()
}


# Between these brackets are instructions on how to use the various functions.
{
  
# In its simplest form, all you need to do is fill the character vector objectsToOrder, then run selected() to start it off.
# A list will come up. You can select up to 10 items (the list will almost certainly have fewer than that)
  # Each item that is not selected is marked down as being eliminated by each object that is.
  # Any not selected will be removed from the running until all the selected ones that round get ranked.
  # The order of the selection doesn't matter, and the selected items are not ranked in relation to one-another.
# To do the selecting, type "selected(#, #, #)" with up to 10 numbers separated by commas.
# Selecting all or none of them doesn't do anything to progress towards the final rankings.
  # If you want to skip a round, type "selected(0)". The ones that you skip will reappear later.


# Other functions:
  #
  # resetFavorites:   Resets all comparisons (empties data frames) and removes a few values from the environment.
  #                     The first execution of selected() will restart the process of ranking and recreate the necessary values.
  #
  # outclassedUndo:   If a comparison is in the `outclassed` data frame that you would like to remove,
  #                     outclassedUndo() will take the index of the eliminated object and the index of
  #                     the object that eliminated it, and remove the row in `outclassed` that contains those values.
  #                     Such removals must be done one at a time.
  #
  # manuallyRank:     If you know that a given object is the best one not yet ranked,
  #                     use manuallyRank() with the name of the object or its index
  #                     in the objectsToOrder vector to put it in the rankings
  #                     after the already ranked objects.
  #
  # manualOutclassed: Takes two vectors as input. Each index in the first vector will
  #                     be marked as eliminating each index in the second.
  #
  # removeRanked:     Takes a vector of rank numbers to be removed from the final rankings.
  #                     For example, if you wish to remove the objects that are
  #                     ranked third and fifth, use "removeRanked(c(3, 5))".
  #                     The order of the objects doesn't matter, and all objects
  #                     ranked below those removed will be moved up.
  
  
# The `parameters` vector contains five elements that can be changed at any time. In order:
  #
  # minNum:         The fewest objects to select from that should be shown. If the number
  #                   of objects remaining is less than minNum, this parameter is ignored.
  #                 Recommended value is 2, but for larger sets of objects, 5 is reasonable.
  #
  # maxNum:         The greatest number of objects to select from that should be shown.
  #                 It is recommended that this value not exceed 11 (as only 10 objects can be chosen).
  #
  # fractionToShow: The number of objects remaining is divided by this value to determine
  #                   how many objects to select from should be shown. If the resulting
  #                   quantity is outside of the range of minNum and maxNum, it is changed
  #                   to equal the closer of those two values.
  #                 Because minNum and maxNum overrule the results here, the value
  #                   chosen for this is not super important.
  #                 Depending on the size of the `objectsToOrder` vector,
  #                   I recommend setting it to between 3 and 10.
  #
  # indexOrder:     In no way affects how the function works, and has minimal utility.
  #                 If set to TRUE, objects selected for ranking will be put in the same order
  #                   that they appear relative to each-other in the `objectsToOrder` vector
  #                 May be useful if the elements of that vector are in a meaningful order.
  #
  # sameLevel:      Affects in what order the objects to be chosen from are selected.
  #                 Objects that have eliminated no or few other objects will
  #                   be first to be selected by the indexGenerator.
  #                 This results in the number of objects doing the eliminating being much larger,
  #                   which means that more comparisons will persist when new rankings are determined.
}


parameters <- c(2, 5, 3, FALSE, TRUE)
# For details on what these parameters are used for, see the last few lines of the above bracket.
# Recommended values are 2-4, 5-10, 3-10, either TRUE or FALSE, and TRUE.

objectsToOrder <- c(1:10)

# Because this function will be used repeatedly but with changing argument values,
  # I find it easiest to work with it in the console section of RStudio,
  # rather than here in the script editor.
selected(0)
