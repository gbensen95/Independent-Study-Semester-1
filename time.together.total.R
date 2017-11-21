#This does the same thing as time.together.perNuc, but combines all the data from each nuc
time.together.total = function(ref.frame, start.frame, end.frame, range)
{
  num.nucs = num.nuc.frame(ref.frame)
  tot.counts.together = 0
  tot.registered.vals = 0
  for(i in 1:num.nucs)
  {
    working.vec = time.together.perNuc(i, ref.frame, start.frame, end.frame, range)
    tot.counts.together = tot.counts.together + working.vec[2]
    tot.registered.vals = tot.registered.vals + working.vec[3]
  }
  tot.prop.together = tot.counts.together/tot.registered.vals
  vec = c(tot.prop.together, tot.counts.together, tot.registered.vals)
  return(vec)
}