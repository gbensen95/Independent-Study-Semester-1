# plot to see if the child numbers actually do match up from frame to frame

child.track.plotAll = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  c1.x = working.mat[,4]
  c1.y = working.mat[,5]
  c2.x = working.mat[,7]
  c2.y = working.mat[,8]
  c3.x = working.mat[,10]
  c3.y = working.mat[,11]
  c4.x = working.mat[,13]
  c4.y = working.mat[,14]
  
  find.limX = na.omit(c(c1.x, c2.x, c3.x, c4.x))
  find.limY = na.omit(c(c1.y, c2.y, c3.y, c4.y))
  
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
  points(c3.x, c3.y, col = "green", pch = 20)
  points(c4.x, c4.y, col = "orange", pch = 20)
  
  legend("bottomright", legend = c("Child 1", "Child 2", "Child 3", "Child 4"), 
         col = c("blue", "red", "green", "orange"), cex = 0.8, pch = 20)
}
