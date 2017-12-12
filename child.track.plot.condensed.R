# Does the same thing as child.track.plotAll but only plots child 1 and child 2 and omits
#the times when there are 3 or 4 children

child.track.plot.condensed = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #get indeces for rows where there are 3 or 4 children so we can skip over them later
  children = working.mat[,3]
  indeces.skip = c()
  for(i in 1:length(children))
  {
    if(is.na(children[i]) == TRUE)
    {
      next
    }
    if(children[i] == 3 | children[i] == 4)
    {
      indeces.skip = c(indeces.skip, i)
    }
  }
  
  c1.x = working.mat[,4]
  c1.y = working.mat[,5]
  c2.x = working.mat[,7]
  c2.y = working.mat[,8]
  
  #now set the child positions to NA at the indeces we found to skip over
  for(l in indeces.skip)
  {
    is.na(c1.x[l]) = TRUE
    is.na(c1.y[l]) = TRUE
    is.na(c2.x[l]) = TRUE
    is.na(c2.y[l]) = TRUE
  }
  
  find.limX = na.omit(c(c1.x, c2.x))
  find.limY = na.omit(c(c1.y, c2.y))
  
  if(length(find.limX) == 0)
  {
    return()
  }
  
  min.x = min(find.limX, na.rm = TRUE)
  max.x = max(find.limX, na.rm = TRUE)
  min.y = min(find.limY, na.rm = TRUE)
  max.y = max(find.limY, na.rm = TRUE)
  
  plot(c1.x, c1.y, col = "blue", ylim = c(min.y, max.y), xlim = c(min.x, max.x),
       xlab = "X Pixel Values", ylab = "Y Pixel Values", pch = 20, 
       main = c("Nucleus", toString(nuc.num)))
  points(c2.x, c2.y, col = "red", pch = 20)

  
  legend("bottomright", legend = c("Child 1", "Child 2"), 
         col = c("blue", "red"), cex = 0.8, pch = 20)
}