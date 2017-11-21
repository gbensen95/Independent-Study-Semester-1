#Returns the number of nuclei found in a given frame
num.nuc.frame = function(frame.num)
{
  x = get.indeces.frame(frame.num)
  num.nuc = length(x)
  return(num.nuc)
}