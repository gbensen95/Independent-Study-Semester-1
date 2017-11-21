#makes plot of where each nucleus goes over the course of each frame
plot.tot.view = function(ref.frame, start.frame, end.frame, range) 
{
  num.nucs.refFrame = num.nuc.frame(ref.frame)
  nuc1.mat = nucleus.matrix(1, ref.frame, start.frame, end.frame, range)
  plot(nuc1.mat[,1], nuc1.mat[,2], ylim = c(0,260), xlim = c(0,515), xlab = "X-pixel loc", ylab = "Y-pixel loc")
  for(i in 1:num.nucs.refFrame)
  {
    curr.nuc.mat = nucleus.matrix(i, ref.frame, start.frame, end.frame, range)
    points(curr.nuc.mat[,1], curr.nuc.mat[,2])
  }
}