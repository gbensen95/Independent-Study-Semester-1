#Produces a bar graph showing the average amount of time (consecutive frames)
#that chromosomes come together when they actually do
#NOTE: In order to deal with the NAs, I just reset the count as soon as an NA was reached. If it was a 0, I did the same thing
#so if a 0 or NA was come across, i just reset the count. Essentially, this just changed the code so that it's either
#if the current value is a 1 do this, if the current value isn't a 1 then reset. 
frames.together.bar = function(ref.frame, start.frame, end.frame, range)
{
  num.nucs = num.nuc.frame(ref.frame)
  N = length(start.frame:end.frame)
  frequency = rep(0,N)
  for(i in 1:num.nucs)
  {
    working.mat = total.nuc.matrix(i, ref.frame, start.frame, end.frame, range)
    count = 0
    for(j in 1:N)
    {
      if(is.na(working.mat[j,3]) == TRUE)
      {
        frequency[count] = frequency[count] + 1
        count = 0
        next
      }
      if(working.mat[j,3] != 1)
      {
        frequency[count] = frequency[count] + 1
        count = 0
        next
      }
      if(working.mat[j,3] == 1)
      {
        count = count + 1
        next
      }
    }
  }
  label = seq(1, N, by=1)
  barplot(frequency, names.arg = label, xlab = "Consecutive Frames Together", ylab = "Frequency")
  return(frequency)
}