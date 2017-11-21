#creates matrix for a given frame that has all nuclei and positions found
frame.matrix = function(frameNum)
{
  nucs.frame.frameNum.x = c()
  nucs.frame.frameNum.y = c()
  num.children.frameNum = c()
  frame = get.indeces.frame(frameNum)
  if(length(frame) == 0) #we need to worry about the case where no nuclei are found
  {
    nucs.frame.frameNum.x = c(0)
    nucs.frame.frameNum.y = c(0)
    num.children.frameNum = c(0)
    columns1 = c("X", "Y", "# Children")
    rows1 = c("NA")
    frameNum.nuc.cords1 = matrix(c(nucs.frame.frameNum.x, nucs.frame.frameNum.y, num.children.frameNum), 
                                 nrow = 1, ncol = 3, dimnames = list(rows1, columns1))
    return(frameNum.nuc.cords1)
  }
  columns2 = c("X", "Y", "# Children")
  rows2 = c()
  for(i in frame) #takes on the actual values in frame 7, i.e. the indeces of original frame vector
  {
    cord.x = x.locs[i]
    cord.y = y.locs[i]
    curr.fish = fish.count[i]
    num.children.frameNum = c(num.children.frameNum, curr.fish)
    nucs.frame.frameNum.x = c(nucs.frame.frameNum.x, cord.x)
    nucs.frame.frameNum.y = c(nucs.frame.frameNum.y, cord.y)
  }
  for(j in 1:length(frame))
  {
    num = toString(j)
    nuc = paste("Nuc", num)
    rows2 = c(rows2, nuc)
  }
  frameNum.nuc.cords2 = matrix(c(nucs.frame.frameNum.x, nucs.frame.frameNum.y, num.children.frameNum), 
                               nrow = length(frame), ncol = 3, dimnames = list(rows2, columns2))
  return(frameNum.nuc.cords2)
}