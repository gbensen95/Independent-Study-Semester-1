#This function finds the overall avg distance moved by a child throughout a cell cycle

tot.avg.dist = function(ref.frame, start.frame, end.frame, range)
{
  working.mat = avg.dist(ref.frame, start.frame, end.frame, range)
  avg.distances = working.mat[,3]
  avg.distances = na.omit(avg.distances)
  sum.distances = sum(avg.distances)
  num.distances = length(avg.distances)
  average = sum.distances/num.distances
  return(average)
}