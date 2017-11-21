#creates a matrix that in the first column has child number and in the second column has integrated intensity
#values. 
intensity.mat = function(ref.frame, start.frame, end.frame, range)
{
  num.nucs = num.nuc.frame(ref.frame)
  children.vec = c()
  intensity.vec = c()
  for(i in 1:num.nucs)
  {
    working.mat = total.nuc.matrix(i, ref.frame, start.frame, end.frame, range)
    for(f in 1:length(working.mat[,1]))
    {
      curr.num.child = working.mat[f,3]
      if(curr.num.child == 0 | is.na(curr.num.child) == TRUE)
      {
        next
      }
      for(k in 1:curr.num.child)
      {
        children.vec = c(children.vec, curr.num.child)
        curr.intensity.ind = 3*k + 3
        intensity.vec = c(intensity.vec, working.mat[f,curr.intensity.ind])
      }
    }
  }
  #now make matrix of children and intensity to return
  vec = c(children.vec, intensity.vec)
  columns = c("Child Number", "Intensity")
  child.intensity.mat = matrix(vec, ncol = 2, nrow = length(children.vec))
  colnames(child.intensity.mat) = columns
  
  
  return(child.intensity.mat)
}


