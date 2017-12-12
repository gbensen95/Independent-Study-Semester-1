#This function finds the average amount a chromosome (spot) moves in a cell cycle per frame and returns a 
#matrix that has frame number, child number, avg. distance moved

avg.dist = function(ref.frame, start.frame, end.frame, range)
{
  num.nucs = num.nuc.frame(ref.frame)
  avg.distances.vec = c()
  for(i in 1:num.nucs)
  {
    working.mat = track.spots(i, ref.frame, start.frame, end.frame, range)
    distances.c1 = c()
    distances.c2 = c()
    for(l in 1:(length(working.mat[,1])-1))
    {
      #first, child 1
      x1.c1 = working.mat[l,4]
      y1.c1 = working.mat[l,5]
      x2.c1 = working.mat[(l+1),4]
      y2.c1 = working.mat[(l+1),5]
      if(is.na(x1.c1) == TRUE | is.na(x2.c1) == TRUE)
      {
        next
      }
      else
      {
        dist.c1 = sqrt(((x1.c1-x2.c1)^2) + ((y2.c1-y1.c1)^2))
        distances.c1 = c(distances.c1, dist.c1)
      }
      #now, child 2
      x1.c2 = working.mat[l,6]
      y1.c2 = working.mat[l,7]
      x2.c2 = working.mat[(l+1),6]
      y2.c2 = working.mat[(l+1),7]
      if(is.na(x1.c2) == TRUE | is.na(x2.c2) == TRUE)
      {
        next
      }
      else
      {
        dist.c2 = sqrt(((x1.c2-x2.c2)^2) + ((y2.c2-y1.c2)^2))
        distances.c2 = c(distances.c2, dist.c2)
      }
    }
    avg.dist.c1 = sum(distances.c1)/length(distances.c1)
    avg.dist.c2 = sum(distances.c2)/length(distances.c2)
    avg.distances.vec = c(avg.distances.vec, avg.dist.c1, avg.dist.c2)
  }
  num.rows = num.nucs*2
  num.cols = 3
  columns = c("Nuc #", "Child #", "Avg. Dist. Moved per Frame")
  col.1 = c()
  for(k in 1:num.nucs)
  {
    val = c(k,k)
    col.1 = c(col.1, val)
  }
  col.2 = rep(c(1,2), num.nucs)
  col.3 = avg.distances.vec
  cols = c(col.1, col.2, col.3)
  destination.mat = matrix(cols, nrow = num.rows, ncol = num.cols)
  colnames(destination.mat) = columns
  return(destination.mat)
}