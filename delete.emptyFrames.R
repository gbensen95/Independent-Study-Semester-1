#Function gets rid of all empty frames
delete.emptyFrames = function(data.set)
{
  frame.numbers = data.set[,1]
  for(i in 1:(length(frame.numbers) - 1))
  {
    if(frame.numbers[i] == frame.numbers[i + 1])
    {
      next
    }
    if(frame.numbers[i] == (frame.numbers[i + 1] - 1))
    {
      next
    }
    to.change = which(frame.numbers == frame.numbers[i + 1])
    for(k in to.change)
    {
      frame.numbers[k] = frame.numbers[i] + 1
    }
  }
  return(frame.numbers)
}