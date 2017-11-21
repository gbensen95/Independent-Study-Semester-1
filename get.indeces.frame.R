#returns all the indeces for a given frame number
get.indeces.frame = function(frame.num)
{
  frameX = which(frame.numbers == frame.num)
  if(length(frameX) == 1)
  {
    frameX = c('NA')
  }
  return(frameX)
}