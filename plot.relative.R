#This function takes the matrix created by track.spots, and plots all the positions of the children. 
#However, it does positions relative to the centroid(i.e. the center of the nucleus). So we consider
#the nucleus center to be at position (0,0) and then we plot the other spots around it. 
plot.relative = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = track.spots(nuc.num, ref.frame, start.frame, end.frame, range)
  new.mat = matrix(0, nrow = length(working.mat[,1]), ncol = 4)
  
  new.mat[,1] = working.mat[,1] - working.mat[,4]
  new.mat[,2] = working.mat[,2] - working.mat[,5]
  new.mat[,3] = working.mat[,1] - working.mat[,6]
  new.mat[,4] = working.mat[,2] - working.mat[,7]
  
  x.vals = c(new.mat[,1], new.mat[,3])
  y.vals = c(new.mat[,2], new.mat[,4])
  x.min = min(x.vals, na.rm=TRUE)
  x.max = max(x.vals, na.rm=TRUE)
  y.min = min(y.vals, na.rm=TRUE)
  y.max = max(y.vals, na.rm=TRUE)
  print(new.mat)
  
  plot(new.mat[,1], new.mat[,2], xlab = "X Loc Relative to Centroid", 
       ylab = "Y Loc Relative to Centroid", main = paste("Nucleus", toString(nuc.num), sep = " "),
       col = rgb(1,0,0,0.5), pch = 20, xlim = c((x.min-5),(x.max+5)), ylim = c((y.min-5),(y.max+5)), 
       type = "o")
  points(new.mat[,3], new.mat[,4], col = rgb(0,0,1,0.5), pch = 20, type = "o")
  points(0,0, col = "black", pch = 8, type = "o")
  points(new.mat[1,1], new.mat[1,2], col = "green", pch = 17)
  points(new.mat[1,3], new.mat[1,4], col = "green", pch = 17)
  
  legend("bottomright", legend = c("Child 1", "Child 2", "Centroid", "Start Pos."), 
         col = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), "black", "green"), 
         pch = c(20, 20, 8, 17), cex = 0.8)
}