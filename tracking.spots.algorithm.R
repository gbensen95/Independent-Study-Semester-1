#this function spits out a matrix that takes a nucleus and goes frame by frame and tells 
#you how each child name (number) gets updated. In other words, I start with child 1 in the 
#first frame and then go to all other frames and it spits out what number child is the
#closest to where child 1 was before it. 

tracking.spots.algorithm = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  results.vec = c()
  num.frames = length(working.mat[,1])
  for(i in 1:(num.frames-1))
  {
    #recall, just looking at child 1
    curr.x = working.mat[i, 4]
    curr.y = working.mat[i, 5]
    if(is.na(curr.x) == TRUE)
    {
      next
    }
    
    
    child.indeces.x = c(4, 7, 10, 13)
    child.indeces.y = c(5, 8, 11, 14)
    distances = c()
    for(l in 1:4)
    {
      if(is.na(working.mat[(i+1), child.indeces.x[l]]) == TRUE)
      {
        break
      }
      next.x = working.mat[(i+1), child.indeces.x[l]]
      next.y = working.mat[(i+1), child.indeces.y[l]]
      dist = sqrt(((next.x - curr.x)^2) + ((next.y - curr.y)^2))
      distances = c(distances, dist)
    }
    if(length(distances) == 0)
    {
      next
    }
    val = min(distances)
    index = which(distances == val)
    results.vec = c(results.vec, index)
  }
  return(results.vec)
}