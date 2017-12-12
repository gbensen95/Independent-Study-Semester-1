#This function takes the old nuc matrix, creates a new one with the spots tracked. It just tracks
#two spots and throws out the extra

track.spots = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #create parameters for new matrix
  columns = c("Nuc.X", "Nuc.Y", "# Children", "Chromosome.1.X", "Chromosome.1.Y", "Chromosome.2.X",
              "Chromosome.2.Y")
  rows = c()
  len.rows = 1 + (end.frame - start.frame)
  for(i in 0:(len.rows-1))
  {
    val = paste("Frame", (i+start.frame), sep = "")
    rows = c(rows, val)
  }
  
  #create empty new matrix that will be filled in with values
  new.mat = matrix(NA, nrow = length(working.mat[,1]), ncol = 7, dimnames = list(rows,columns))
  
  #match up the columns that won't change
  new.mat[,1] = working.mat[,1]
  new.mat[,2] = working.mat[,2]
  new.mat[,3] = working.mat[,3]
  
  #now let's get to the actual tracking to fill in the rest of the columns
  
  #find where to start
  for(i in 1:length(working.mat[,1]))
  {
    val = working.mat[i,3]
    if(is.na(val) == TRUE)
    {
      next 
    }
    else
    {
      start = i
      break
    }
  }
  
  #now, however many children are in this starting frame, need to initialize just the two children
  
  #first, if start num children = 1
  if(working.mat[start,3] == 1)
  {
    new.mat[start,4] = working.mat[start,4]
    new.mat[start,5] = working.mat[start,5]
    new.mat[start,6] = working.mat[start,4]
    new.mat[start,7] = working.mat[start,5]
  }
  
  #if start num children = 2
  if(working.mat[start,3] == 2)
  {
    new.mat[start,4] = working.mat[start,4]
    new.mat[start,5] = working.mat[start,5]
    new.mat[start,6] = working.mat[start,7]
    new.mat[start,7] = working.mat[start,8]
  }
  
  #if start num children = 3
  if(working.mat[start,3] == 3)
  {
    x1 = working.mat[start,4]
    y1 = working.mat[start,5]
    x2 = working.mat[start,7]
    y2 = working.mat[start,8]
    x3 = working.mat[start,10]
    y3 = working.mat[start,11]
    
    #need to find the distances between all points
    dist.1 = sqrt(((x1-x2)^2) + ((y1-y2)^2))
    dist.2 = sqrt(((x1-x3)^2) + ((y1-y3)^2))
    dist.3 = sqrt(((x2-x3)^2) + ((y2-y3)^2))
    
    distance.vec = c(dist.1, dist.2, dist.3)
    min.dist = min(distance.vec)

    #if min distance = dist.1, average 1 and 2 and set to chrom 1, set 3 to chrom 2
    if(min.dist == dist.1)
    {
      new.mat[start,4] = (x1 + x2)/2
      new.mat[start,5] = (y1 + y2)/2
      new.mat[start,6] = x3
      new.mat[start,7] = y3
    }
    #if min distance = dist.2, avg 1 and 3 and set to chrom 1, set 2 to chrom 2 
    if(min.dist == dist.2)
    {
      new.mat[start,4] = (x1 + x3)/2
      new.mat[start,5] = (y1 + y3)/2
      new.mat[start,6] = x2
      new.mat[start,7] = y2
    }
    #if min distance = dist.3, set 1 to chrom 1, avg 2 and 3 and set to chrom 2
    if(min.dist == dist.3)
    {
      new.mat[start,4] = x1
      new.mat[start,5] = x2
      new.mat[start,6] = (x2 + x3)/2
      new.mat[start,7] = (y2 + y3)/2
    }
  }
  
  #if start num children = 4
  if(working.mat[start,3] == 4)
  {
    x1 = working.mat[start,4]
    y1 = working.mat[start,5]
    x2 = working.mat[start,7]
    y2 = working.mat[start,8]
    x3 = working.mat[start,10]
    y3 = working.mat[start,11]
    x4 = working.mat[start,13]
    y4 = working.mat[start,14]
    
    #need to find the distances between all points
    dist.1 = sqrt(((x1-x2)^2) + ((y1-y2)^2))
    dist.2 = sqrt(((x1-x3)^2) + ((y1-y3)^2))
    dist.3 = sqrt(((x1-x4)^2) + ((y1-y4)^2))
    dist.4 = sqrt(((x2-x3)^2) + ((y2-y3)^2))
    dist.5 = sqrt(((x2-x4)^2) + ((y2-y4)^2))
    dist.6 = sqrt(((x3-x4)^2) + ((y3-y4)^2))
    
    distance.vec = c(dist.1, dist.2, dist.3, dist.4, dist.5, dist.6)
    first.min.dist = min(distance.vec)
    
    #if min distance = dist.1 or dist.6, avg 1 and 2 and set to chrom 1, avg 3 and 4 set to chrom 2
    if(min.dist == dist.1 | min.dist == dist.6)
    {
      new.mat[start,4] = (x1 + x2)/2
      new.mat[start,5] = (y1 + y2)/2
      new.mat[start,6] = (x3 + x4)/2
      new.mat[start,7] = (y3 + y4)/2
    }
    
    #if min distance = dist.2 or dist.5, avg 1 and 3 and set to chrom 1, avg 2 and 4 set to chrom 2
    if(min.dist == dist.2 | min.dist == dist.5)
    {
      new.mat[start,4] = (x1 + x3)/2
      new.mat[start,5] = (y1 + y3)/2
      new.mat[start,6] = (x2 + x4)/2
      new.mat[start,7] = (y2 + y4)/2
    }
    
    #if min distance = dist.3 or dist.4, avg 1 and 3 and set to chrom 1, avg 2 and 4 set to chrom 2
    if(min.dist == dist.3 | min.dist == dist.4)
    {
      new.mat[start,4] = (x1 + x4)/2
      new.mat[start,5] = (y1 + y4)/2
      new.mat[start,6] = (x2 + x3)/2
      new.mat[start,7] = (y2 + y3)/2
    }
  }
  
  #now that we have starting positions for chrom 1 and chrom 2, can move on to future values
  for(l in (start+1):length(working.mat[,1]))
  {
    #first need to deal with NAs and 0s
    if(is.na(working.mat[l,3] == TRUE) | working.mat[l,3] == 0)
    {
      next
    }
    #find the old (referance) index by finding last position that wasn't an NA or 0
    for(k in start:(l-1))
    {
      if(is.na(working.mat[k,3]) == TRUE | working.mat[k,3] == 0)
      {
        next
      }
      else
      {
        old.index = k
      }
    }
    #use index to find old positions
    old.x.1 = new.mat[old.index,4]
    old.y.1 = new.mat[old.index,5]
    old.x.2 = new.mat[old.index,6]
    old.y.2 = new.mat[old.index,7]
    
    #find the next frames spot positions by finding the spot closest to the old one
    
    #find closest to child 1 first
    indeces.vec = c(4,7,10,13)
    distances.1 = c()
    for(n in indeces.vec)
    {
      x = working.mat[l,n]
      y = working.mat[l,(n+1)]
      if(is.na(x) == TRUE | is.na(y) == TRUE)
      {
        break
      }
      dist = sqrt(((x-old.x.1)^2) + ((y-old.y.1)^2))
      distances.1 = c(distances.1, dist)
    }
    min.c1 = min(distances.1)
    new.c1.x.index = which(distances.1 == min.c1)
    new.c1.x = working.mat[l,indeces.vec[new.c1.x.index]]
    new.c1.y = working.mat[l,(indeces.vec[new.c1.x.index]+1)]
    
    #now find closest to child 2
    if(length(distances.1) == 1)
    {
      new.c2.x = new.c1.x
      new.c2.y = new.c1.y
    }
    else
    {
      indeces.vec = indeces.vec[-new.c1.x.index]
      distances.2 = c()
      for(n in indeces.vec)
      {
        x = working.mat[l,n]
        y = working.mat[l,(n+1)]
        if(is.na(x) == TRUE | is.na(y) == TRUE)
        {
          break
        }
        dist = sqrt(((x-old.x.2)^2) + ((y-old.y.2)^2))
        distances.2 = c(distances.2, dist)
      }
      min.c2 = min(distances.2)
      new.c2.x.index = which(distances.2 == min.c2)
      new.c2.x = working.mat[l,indeces.vec[new.c2.x.index]]
      new.c2.y = working.mat[l,(indeces.vec[new.c2.x.index]+1)]
    }
    new.mat[l,4] = new.c1.x
    new.mat[l,5] = new.c1.y
    new.mat[l,6] = new.c2.x
    new.mat[l,7] = new.c2.y
  }
  return(new.mat)
}
