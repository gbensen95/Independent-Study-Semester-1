#This function takes a total nucleus matrix and looks specifically at cases where there is 1 child
#and turns all the rows that have an intensity in a given threshold range to NA values

nuc.tot.mat.limited = function(nuc.num, ref.frame, start.frame, end.frame, range, 
                               threshold.min, threshold.max)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  num = length(working.mat[,1])
  for(i in 1:num)
  {
    child = working.mat[i,3]
    intensity = working.mat[i,6]
    if(child == 1 & intensity >= threshold.min & intensity <= threshold.max)
    {
      working.mat[i,] = NA
    }
  }
  return(working.mat)
}