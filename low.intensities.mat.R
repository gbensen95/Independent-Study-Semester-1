#This function takes the matrix for a nucleus from the original total.nuc.matrix function output and
#returns a matrix that has nucleus number, frame number, intensity, and nucleus centroid position
#for every instance where there is 1 child found and the intensity value for that child is less
#than some threshold max and greater than some threshold min.

low.intensities.mat = function(ref.frame, start.frame, end.frame, range, threshold.min, threshold.max)
{
  nuc.nums = c()
  frame.nums = c()
  x.pos = c()
  y.pos = c()
  intensities = c()
  
  num.nuclei = num.nuc.frame(ref.frame)
  
  for(l in 1:num.nuclei)
  {
    working.mat = total.nuc.matrix(l, ref.frame, start.frame, end.frame, range)
    for(i in 1:length(working.mat[,1]))
    {
      num.child = working.mat[i,3]
      if(is.na(num.child) == TRUE)
      {
        next
      }
      if(num.child == 1)
      {
        intensity = working.mat[i, 6]
        if((intensity <= threshold.max) & (intensity >= threshold.min))
        {
          nuc.nums = c(nuc.nums, l)
          frame.num = (i + start.frame) - 1
          frame.nums = c(frame.nums, frame.num)
          x.pos = c(x.pos, working.mat[i,1])
          y.pos = c(y.pos, working.mat[i,2])
          intensities = c(intensities, working.mat[i,6])
        }
      }
    }
  }
  columns = c("Nucleus Num", "Frame Num", "X-Pos Centroid", "Y-Pos Centroid", "Intensity")
  new.mat = matrix(c(nuc.nums, frame.nums, x.pos, y.pos, intensities), nrow = length(nuc.nums), ncol = 5)
  colnames(new.mat) = columns
  
  return(new.mat)
}