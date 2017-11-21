#NOTE: THIS ASSUMES THAT THERE ARE EITHER 0, 1, 2, 3, or 4 CHILDREN (NO 5s OR HIGHER)
#returns a matrix for a given nuclei that has all the info you need in it (position in each frame, number
#of children in each frame, positions of children and integrated intensities of children)
total.nuc.matrix = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  #First, generate the simple matrix for this given nuclei
  basic.mat = nucleus.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #Now, create an empty matrix of NA's that we can concatenate with the above matrix to create skeleton for new matrix
  columns = c("Child1.X", "Child1.Y", "Intense.1", "Child2.X", "Child2.Y", "Intense.2", 
              "Child3.X", "Child3.Y", "Intense.3", "Child4.X", "Child4.Y", "Intense.4")
  rows = c()
  len.rows = 1 + (end.frame - start.frame)
  for(i in 0:(len.rows-1))
  {
    val = paste("Frame", (i+start.frame), sep="")
    rows = c(rows, val)
  }
  add.mat = matrix(NA, nrow = (end.frame - start.frame) + 1, ncol = 12, dimnames = list(rows, columns))
  
  #Now, let's actually do that concatenation
  final.mat = cbind(basic.mat,add.mat)
  
  #Now, need to find all data and fill it into the appropriate locations
  for(i in 1:((end.frame-start.frame)+1))
  {
    frameNum = (start.frame + i) - 1
    nucX = final.mat[i,1]
    nucY = final.mat[i,2]
    numChildren = final.mat[i,3]
    
    #need to move on if the current row is all NAs; only need to check one, cause if one is, all others are
    if(is.na(nucX) == TRUE)
    {
      next
    }
    
    #Find the object number from the first data set to this specific nuclei in order to determine parent nuclei number
    for(j in 1:length(frame.numbers))
    {
      if(frameNum == frame.numbers[j] && nucX == x.locs[j] && nucY == y.locs[j] && numChildren == fish.count[j])
      {
        object.num = object.numbers[j] #recall, object number corresponds to parent nuclei
        break
      }
    }
    
    #now that we have the parent nuclei we can find the corresponding children coordinates in the second data set
    for(k in 1:length(frame.numbers.2))
    {
      if(frame.numbers.2[k] == frameNum && parent.nuc[k] == object.num)
      {
        if(is.na(final.mat[i,4]) == TRUE)
        {
          final.mat[i,4] = x.locs.spots[k]
          final.mat[i,5] = y.locs.spots[k]
          final.mat[i,6] = mean.intens[k]
          next
        }
        if(is.na(final.mat[i,4]) == FALSE && is.na(final.mat[i,7]) == TRUE)
        {
          final.mat[i,7] = x.locs.spots[k]
          final.mat[i,8] = y.locs.spots[k]
          final.mat[i,9] = mean.intens[k]
          next
        }
        if(is.na(final.mat[i,4]) == FALSE && is.na(final.mat[i,7]) == FALSE && is.na(final.mat[i,10]) == TRUE)
        {
          final.mat[i,10] = x.locs.spots[k]
          final.mat[i,11] = y.locs.spots[k]
          final.mat[i,12] = mean.intens[k]
          next
        }
        if(is.na(final.mat[i,4]) == FALSE && is.na(final.mat[i,7]) == FALSE && is.na(final.mat[i,10]) == FALSE)
        {
          final.mat[i,13] = x.locs.spots[k]
          final.mat[i,14] = y.locs.spots[k]
          final.mat[i,15] = mean.intens[k]
          next
        }
      }
    }
  }
  return(final.mat)
}