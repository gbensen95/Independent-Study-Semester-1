#This plot is the distance apart between children at each frame for each nucleus
all.nuc.plot = function(ref.frame, start.frame, end.frame, range, conversion.dist, conversion.time)
{
  #initialize plot
  y.values = distance.vec(1, ref.frame, start.frame, end.frame, range)
  x.frames = start.frame:end.frame
  len = length(x.frames)
  x.values = conversion.time*(seq(1,len,by=1))
  plot(x.values, y.values, ylim = c(0,30), xlab = "Time", ylab = "Chromosome Distance Apart", type = "l")
  range = length(x.values) - 1
  counts = 0
  for(i in 1:range)
  {
    if(is.na(y.values[i]) == TRUE)
    {
      counts = counts + 1
      next
    }
    if(is.na(y.values[i]) == FALSE && counts > 0)
    {
      dist = counts + 1
      x0 = x.values[i - dist]
      y0 = y.values[i - dist]
      x1 = x.values[i]
      y1 = y.values[i]
      segments(x0, y0, x1, y1, lty = "dashed")
      counts = 0
    }
  }  
  #now run through rest
  num.nucs = num.nuc.frame(ref.frame)
  for(i in 2:num.nucs)
  {
    y.values = distance.vec(i, ref.frame, start.frame, end.frame, range, conversion.dist)
    x.frames = start.frame:end.frame
    len = length(x.frames)
    x.values = conversion.time*(seq(1,len,by=1))
    col = c(5*i, 5*i, 5*i)
    points(x.values, y.values, xlab = "Time (s)", ylab = "Distance (nm)", type = "l", col = col)
    range = length(x.values) - 1
    counts = 0
    for(i in 1:range)
    {
      if(is.na(y.values[i]) == TRUE)
      {
        counts = counts + 1
        next
      }
      if(is.na(y.values[i]) == FALSE && counts > 0)
      {
        dist = counts + 1
        x0 = x.values[i - dist]
        y0 = y.values[i - dist]
        x1 = x.values[i]
        y1 = y.values[i]
        segments(x0, y0, x1, y1, lty = "dashed")
        counts = 0
      }
    }  
  }
}