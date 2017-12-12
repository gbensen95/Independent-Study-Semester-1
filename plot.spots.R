#this function will plot the position of child 1 for a given spot

plot.spots = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #child 1
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
  
  plot(c1.x[1], c1.y[1], col = rgb(0.1, 0, 0), ylim = c(min.y, max.y), xlim = c(min.x, max.x),
       xlab = "X Pixel Values", ylab = "Y Pixel Values", pch = 20, 
       main = c("Nucleus", toString(nuc.num)))
  points(c2.x[1], c2.y[1], col = rgb(0, 0.1, 0), pch = 20)
  points(c3.x[1], c3.y[1], col = rgb(0, 0, 0.1), pch = 20)
  points(c4.x[1], c4.y[1], col = rgb(0.1, 0.1, 0.1), pch = 20)
  
  portion = 0.3
  for(i in 2:length(c1.x))
  {
    points(c1.x[i], c1.y[i], col = rgb(portion, 0, 0), pch = 20)
    portion = portion + 0.03
  }
  
  portion = 0.3
  for(i in 2:length(c2.x))
  {
    points(c2.x[i], c2.y[i], col = rgb(0, portion, 0), pch = 20)
    portion = portion + 0.03
  }
  
  portion = 0.3
  for(i in 2:length(c3.x))
  {
    points(c3.x[i], c3.y[i], col = rgb(0, 0, portion), pch = 20)
    portion = portion + 0.03
  }
  
  portion = 0.3
  for(i in 2:length(c4.x))
  {
    points(c4.x[i], c4.y[i], col = rgb(portion, portion, portion), pch = 20)
    portion = portion + 0.03
  }
  
  colors = c(rgb(0.3,0,0), rgb(0,0.3,0), rgb(0,0,0.3), rgb(0.3,0.3,0.3))
  legend("bottomright", legend = c("Child 1", "Child 2", "Child 3", "Child 4"), 
         col = colors, cex = 0.8, pch = 20)
}