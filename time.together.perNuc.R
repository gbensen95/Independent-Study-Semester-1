#Over the course of the whole cell cycle, how much time in total are chromosomes spent together?
#Returns total number of non NA or 0 instances, number of frames children are together, and the
#proportion of these two things. It returns a vector with three entries in this order
time.together.perNuc = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  children = working.mat[,3]
  registered.vals = 0
  count.together = 0
  for(i in children)
  {
    #I am going to throw out all cases of NAs and 0s
    if(is.na(i) == TRUE | i == 0)
    {
      next
    }
    if(i == 1)
    {
      count.together = count.together + 1
    }
    registered.vals = registered.vals + 1
  }
  prop.together = count.together/registered.vals
  important.vals = c(prop.together, count.together, registered.vals)
  return(important.vals)
}