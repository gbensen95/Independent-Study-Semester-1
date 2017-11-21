#Finds the proportion of nuclei that never show paired children
prop.no.paired = function(ref.frame, start.frame, end.frame, range)
{
  num.nucs = num.nuc.frame(ref.frame)
  counts = 0
  for(i in 1:num.nucs)
  {
    working.vec = time.together.perNuc(i, ref.frame, start.frame, end.frame, range)
    if(working.vec[2] == 0)
    {
      counts = counts + 1
    }
  }
  prop.never.together = counts/num.nucs
  return(prop.never.together)
}