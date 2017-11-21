#This is a plot representing the mean value of distance apart between children for each frame
#the error bars represent the standard deviation for each mean value 

mean.plot = function(ref.frame, start.frame, end.frame, range, ci, conversion.dist, conversion.time)
{
  #create empty vector to store stuff that can then be averaged for mean values
  num.nucs = num.nuc.frame(ref.frame)
  row = num.nucs
  col = length(start.frame:end.frame)
  mat = matrix(0, nrow = row, ncol = col)
  
  for(i in 1:num.nucs)
  {
    distance.curr = distance.vec(i,ref.frame,start.frame,end.frame,range,conversion.dist)
    for(j in 1:length(distance.curr))
    {
      mat[i,j] = distance.curr[j]
    }
  }
  #now find the means and standard deviations
  mean.distances = c()
  stand.devs = c()
  for(l in 1:col)
  {
    new.mean = mean(mat[,l], na.rm = TRUE)
    new.std.dev = sd(mat[,l], na.rm = TRUE)
    mean.distances = c(mean.distances, new.mean)
    stand.devs = c(stand.devs, new.std.dev)
  }
  library(ggplot2)
  #create vectors for the data frame
  x.frames = start.frame:end.frame
  len = length(x.frames)
  x.values = conversion.time*(seq(1,len,by=1))
  y = mean.distances
  sd = stand.devs
  dframe = data.frame(x.values,y,sd)
  graph = ggplot(dframe, aes(x.values, y, ymin = y-sd, ymax = y+sd)) + 
    geom_point(alpha = 1, shape = 21, fill = "white", color = "black", size = 1.5, stroke = 1.5) + 
    geom_errorbar() + 
    labs(x = "Time (s)", y = "Distance (nm)") +
    geom_smooth(method = "loess", level = ci) +
    ylim(0,4500)

  return(graph)
}