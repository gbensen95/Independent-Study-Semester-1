#plot for each nuc that shows how many children it has frame to frame
plot.nuc = function(nuc.num, ref.frame, start.frame, end.frame, range) 
{
  nucX.mat = nucleus.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  frame = start.frame:end.frame
  nuc.name = toString(nuc.num)
  print(nuc.name)
  title = paste("Nucleus", nuc.name)
  plot(frame, nucX.mat[,3], xlab = "Frame Number", ylab = "Number of Children", ylim = c(0,3), xlim = c(0,25),
       main = title)
  for(i in 1:length(frame))
  {
    if(toString(nucX.mat[i,3]) == "NA")
    {
      abline(v = ((i + start.frame) - 1), untf = FALSE, col = "Red")
    }
  }
}