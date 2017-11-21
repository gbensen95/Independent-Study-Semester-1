#Plots the distance apart children are for each frame of a given nucleus. How these distances are actually found
#are outlined thoroughly in the notes I made on evernote. This function also takes conversion parameters so that
#the distance and time stuff can be more meaningful
#NOTE: THIS ASSUMES THAT THERE ARE EITHER 0, 1, 2, 3, or 4 CHILDREN (NO 5s OR HIGHER)
distance.plot = function(nuc.num, ref.frame, start.frame, end.frame, range, conversion.dist, conversion.time)
{
  #first, create the total matrix that the data is going to be drawn from:
  nuc.info.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #create vector to store the distances in
  distances.vec = c()
  
  #Now, go through each frame and determine the distance apart that the chromosomes are
  nrows = length(nuc.info.mat[,1])
  for(i in 1:nrows)
  {
    #first, deal with NAs and the few cases where 0 children, but nucleus found
    if(is.na(nuc.info.mat[i,3]) == TRUE | nuc.info.mat[i,3] == 0)
    {
      distance = NA
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 1)
    {
      distance = 0
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 2)
    {
      x1 = nuc.info.mat[i,4]
      y1 = nuc.info.mat[i,5]
      x2 = nuc.info.mat[i,7]
      y2 = nuc.info.mat[i,8]
      a = (x1 - x2)^2
      b = (y1 - y2)^2
      distance = sqrt(a + b)
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 3)
      #if 3, find shortest distance, toss it out, then average the two longer distances
      #doing this because one chromosome must be split into sister chromatids
    {
      #find first distance
      x1.1 = nuc.info.mat[i,4]
      y1.1 = nuc.info.mat[i,5]
      x2.1 = nuc.info.mat[i,7]
      y2.1 = nuc.info.mat[i,8]
      a.1 = (x1.1 - x2.1)^2
      b.1 = (y1.1 - y2.1)^2
      distance.1 = sqrt(a.1 + b.1)
      #find second distance
      x1.2 = nuc.info.mat[i,4]
      y1.2 = nuc.info.mat[i,5]
      x2.2 = nuc.info.mat[i,10]
      y2.2 = nuc.info.mat[i,11]
      a.2 = (x1.2 - x2.2)^2
      b.2 = (y1.2 - y2.2)^2
      distance.2 = sqrt(a.2 + b.2)
      #find third distance
      x1.3 = nuc.info.mat[i,7]
      y1.3 = nuc.info.mat[i,8]
      x2.3 = nuc.info.mat[i,10]
      y2.3 = nuc.info.mat[i,11]
      a.3 = (x1.3 - x2.3)^2
      b.3 = (y1.3 - y2.3)^2
      distance.3 = sqrt(a.3 + b.3)
      #find minimum distance 
      first.vec = c(distance.1, distance.2, distance.3)
      min = min(vec)
      second.vec = c()
      for(i in 1:3)
      {
        if(first.vec[i] == min)
        {
          next
        }
        else
        {
          second.vec = c(second.vec, first.vec[i])
        }
      }
      distance = mean(second.vec)
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 4)
      #if 4, find two longest distances, average them
      #this might be slightly too long... but can do it under human error assumption
    {
      #find first distance
      x1.1 = nuc.info.mat[i,4]
      y1.1 = nuc.info.mat[i,5]
      x2.1 = nuc.info.mat[i,7]
      y2.1 = nuc.info.mat[i,8]
      a.1 = (x1.1 - x2.1)^2
      b.1 = (y1.1 - y2.1)^2
      distance.1 = sqrt(a.1 + b.1)
      #find second distance
      x1.2 = nuc.info.mat[i,4]
      y1.2 = nuc.info.mat[i,5]
      x2.2 = nuc.info.mat[i,10]
      y2.2 = nuc.info.mat[i,11]
      a.2 = (x1.2 - x2.2)^2
      b.2 = (y1.2 - y2.2)^2
      distance.2 = sqrt(a.2 + b.2)
      #find third distance
      x1.3 = nuc.info.mat[i,7]
      y1.3 = nuc.info.mat[i,8]
      x2.3 = nuc.info.mat[i,10]
      y2.3 = nuc.info.mat[i,11]
      a.3 = (x1.3 - x2.3)^2
      b.3 = (y1.3 - y2.3)^2
      distance.3 = sqrt(a.3 + b.3)
      #find fourth distance
      x1.4 = nuc.info.mat[i,4]
      y1.4 = nuc.info.mat[i,5]
      x2.4 = nuc.info.mat[i,13]
      y2.4 = nuc.info.mat[i,14]
      a.4 = (x1.4 - x2.4)^2
      b.4 = (y1.4 - y2.4)^2
      distance.4 = sqrt(a.4 + b.4)
      #find fifth distance
      x1.5 = nuc.info.mat[i,7]
      y1.5 = nuc.info.mat[i,8]
      x2.5 = nuc.info.mat[i,13]
      y2.5 = nuc.info.mat[i,14]
      a.5 = (x1.5 - x2.5)^2
      b.5 = (y1.5 - y2.5)^2
      distance.5 = sqrt(a.5 + b.5)
      #find sixth distance
      x1.6 = nuc.info.mat[i,10]
      y1.6 = nuc.info.mat[i,11]
      x2.6 = nuc.info.mat[i,13]
      y2.6 = nuc.info.mat[i,14]
      a.6 = (x1.6 - x2.6)^2
      b.6 = (y1.6 - y2.6)^2
      distance.6 = sqrt(a.6 + b.6)
      
      #find the two longest distances
      new.vec = c(distance.1, distance.2, distance.3, distance.4, distance.5, distance.6)
      max1 = max(new.vec)
      index1 = which(new.vec == max1)
      new.vec = new.vec[-index1]
      max2 = max(new.vec)
      distance = (max1 + max2)/2
      distances.vec = c(distances.vec, distance)
      next
    }
  }
  
  #now, plot the distances vs. the frame!
  x.frames = start.frame:end.frame
  len = length(x.frames)
  x.values = conversion.time*(seq(1,len,by=1))
  y.values = distances.vec*conversion.dist #need to convert to distance instead of pixels
  title = paste("Nucleus", nuc.num)
  plot(x.values, y.values, xlab = "Time (s)", ylab = "Distance (nm)", main = title, type = "l")
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