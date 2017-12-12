#This function takes the matrix provided by avg.dist.R and makes the third column into average
#velocity throughout cell cycle by dividing the avg. distance per frame by a 
#time per frame converstion

avg.velocity = function(ref.frame, start.frame, end.frame, range, time.conversion)
{
  working.mat = avg.dist(ref.frame, start.frame, end.frame, range)
  col.3 = working.mat[,3]/time.conversion
  col.1 = working.mat[,1]
  col.2 = working.mat[,2]
  num.nucs = num.nuc.frame(ref.frame)
  num.cols = 3
  num.rows = num.nucs*2
  columns = c("Nuc #", "Child #", "Avg. Velocity. Per Cell Cycle")
  new.mat = matrix(c(col.1, col.2, col.3), nrow = num.rows, ncol = num.cols)
  colnames(new.mat) = columns
  return(new.mat)
}