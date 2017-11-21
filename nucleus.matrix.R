#Returns a matrix that has position of a nucleus for each frame
#nuc.num = the nucleus that you want to track from your start frame to your end frame
#range = the +/- that you want to track to find all the matches from frame 1 to frame 25 for that nucleus
#start.frame = frame you want to start with for all the comparisons
#end.frame = frame you want to end with for all the comparisons
#ref.frame = the frame that you will use as a referance to see if a given coordinate should be included or not
#best practice would be to have the ref.frame be quite close to the start.frame cause otherwise the 
#referance frame might have moved a lot and pick up a different nucleus
nucleus.matrix = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  if(end.frame <= start.frame)
  {
    print("invalid starting and ending frame")
  }
  #first, get the matrix for our referance frame (the frame that everything will be compared)
  refFrame.mat = frame.matrix(ref.frame)
  
  num.nucs.refFrame = num.nuc.frame(ref.frame)
  #take whatever values correspond to "nuc.num" from the referance:
  x1 = refFrame.mat[nuc.num,1] 
  y1 = refFrame.mat[nuc.num,2]
  child1 = refFrame.mat[nuc.num,3]
  
  #Create an empty matrix that will be filled in:
  columns = c("X", "Y", "# Children")
  len.rows = 1 + (end.frame - start.frame)
  rows = c()
  for(i in 0:(len.rows-1))
  {
    val = paste("Frame", (i+start.frame), sep="")
    rows = c(rows, val)
  }
  emptyX = rep(0, len.rows)
  emptyY = rep(0, len.rows)
  emptyChild = rep(0, len.rows)
  nucX.mat = matrix(c(emptyX, emptyY, emptyChild), nrow = len.rows, ncol = 3, dimnames = list(rows, columns))
  
  #initiate the referance frame entry:
  #nucX.mat[ref.frame,1] = x1
  #nucX.mat[ref.frame,2] = y1
  #nucX.mat[ref.frame,3] = child1
  
  #now, need to run through every frame and then every nuc in each frame to find matches
  for(i in start.frame:end.frame)
  {
    num.nucs.frameX = num.nuc.frame(i)
    frameX.mat = frame.matrix(i)
    #need following to account for fact that i will start at the start.frame, but the index of the matrix starts with 1
    index.shifter = start.frame - 1  
    for(j in 1:num.nucs.frameX)
    {
      #need to account for if a frame has no nuclei
      if(num.nucs.frameX == 0)
      {
        break
      }
      x2 = frameX.mat[j,1]
      y2 = frameX.mat[j,2]
      child2 = frameX.mat[j,3]
      if(((abs(x1 - x2)) <= range) && ((abs(y1 - y2)) <= range))
      {
        nucX.mat[(i-index.shifter),1] = x2
        nucX.mat[(i-index.shifter),2] = y2
        nucX.mat[(i-index.shifter),3] = child2
        break #this way there aren't repeats found
      }
    }
  }
  #need to control for if there are still 0's leftover
  for(i in 1:len.rows)
  {
    if(nucX.mat[i,1] == 0 && nucX.mat[i,2] == 0)
    {
      nucX.mat[i,1] = NA
      nucX.mat[i,2] = NA
      nucX.mat[i,3] = NA
    }
  }
  return(nucX.mat)
}