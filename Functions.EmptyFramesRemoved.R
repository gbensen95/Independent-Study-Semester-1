#install packages I will need:
#install.packages("ggplot2")

#read in data set
data.set = read.table("CellCycle1.txt", header=TRUE, sep="\t")
data.set = na.omit(data.set)

#First, get rid of all the empty frames
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

frame.numbers = delete.emptyFrames(data.set)
object.numbers = data.set[,2]
fish.count = data.set[,3]
x.locs = data.set[,4]
y.locs = data.set[,5]




#1. get all indeces relating to a frame
get.indeces.frame = function(frame.num)
{
  frameX = which(frame.numbers == frame.num)
  if(length(frameX) == 1)
  {
    frameX = c('NA')
  }
  return(frameX)
}

#2. get # of nuclei found in each frame
num.nuc.frame = function(frame.num)
{
  x = get.indeces.frame(frame.num)
  num.nuc = length(x)
  return(num.nuc)
}

#3. Create matrix for each frame that has each nucleus found with its coordinates
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


#4. This function will create a matrix for each nucleus from the referance frame and determine its path through each frame
#nuc.num = the nucleus that you want to track from your start frame to your end frame
#range = the +/- that you want to track to find all the matches from frame 1 to frame 25 for that nucleus
#start.frame = frame you want to start with for all the comparisons
#end.frame = frame you want to end with for all the comparisons
#ref.frame = the frame that you will use as a referance to see if a given coordinate should be included or not
#best practice would be to have the re.frame be quite close to the start.frame cause otherwise the 
#referance frame might have moved a lot and pick up a different nucleus

nucleus.matrix = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  if(end.frame <= start.frame)
  {
    print("invalid starting and ending frame")
  }
  #first, get the matrix for our referance frame (the frame that everything will be compared)
  refFrame.mat = frame.matrix(ref.frame)
  
  num.nucs.refFrame = num.nuc.frame(ref.frame)
  #take whatever values correspond to "nuc.num" from the referance:
  x1 = refFrame.mat[nuc.num,1] 
  y1 = refFrame.mat[nuc.num,2]
  child1 = refFrame.mat[nuc.num,3]
  
  #Create an empty matrix that will be filled in:
  columns = c("X", "Y", "# Children")
  len.rows = 1 + (end.frame - start.frame)
  rows = c()
  for(i in 0:(len.rows-1))
  {
    val = paste("Frame", (i+start.frame), sep="")
    rows = c(rows, val)
  }
  emptyX = rep(0, len.rows)
  emptyY = rep(0, len.rows)
  emptyChild = rep(0, len.rows)
  nucX.mat = matrix(c(emptyX, emptyY, emptyChild), nrow = len.rows, ncol = 3, dimnames = list(rows, columns))
  
  #initiate the referance frame entry:
  nucX.mat[ref.frame,1] = x1
  nucX.mat[ref.frame,2] = y1
  nucX.mat[ref.frame,3] = child1
  
  #now, need to run through every frame and then every nuc in each frame to find matches
  for(i in start.frame:end.frame)
  {
    num.nucs.frameX = num.nuc.frame(i)
    frameX.mat = frame.matrix(i)
    #need following to account for fact that i will start at the start.frame, but the index of the matrix starts with 1
    index.shifter = start.frame - 1  
    for(j in 1:num.nucs.frameX)
    {
      #need to account for if a frame has no nuclei
      if(num.nucs.frameX == 0)
      {
        break
      }
      x2 = frameX.mat[j,1]
      y2 = frameX.mat[j,2]
      child2 = frameX.mat[j,3]
      if(((abs(x1 - x2)) <= range) && ((abs(y1 - y2)) <= range))
      {
        nucX.mat[(i-index.shifter),1] = x2
        nucX.mat[(i-index.shifter),2] = y2
        nucX.mat[(i-index.shifter),3] = child2
        break #this way there aren't repeats found
      }
    }
  }
  #need to control for if there are still 0's leftover
  for(i in 1:len.rows)
  {
    if(nucX.mat[i,1] == 0 && nucX.mat[i,2] == 0)
    {
      nucX.mat[i,1] = NA
      nucX.mat[i,2] = NA
      nucX.mat[i,3] = NA
    }
  }
  return(nucX.mat)
}

#Now, it would be great to be able to have a plotting function

#we are actually gonna have two plotting functions
#1. same plot as always wear it shows where each nuc goes over the course of all frames
# the arguments are the same as in the nucleus.matrix function
plot.tot.view = function(ref.frame, start.frame, end.frame, range) 
{
  num.nucs.refFrame = num.nuc.frame(ref.frame)
  nuc1.mat = nucleus.matrix(1, ref.frame, start.frame, end.frame, range)
  plot(nuc1.mat[,1], nuc1.mat[,2], ylim = c(0,260), xlim = c(0,515), xlab = "X-pixel loc", ylab = "Y-pixel loc")
  for(i in 1:num.nucs.refFrame)
  {
    curr.nuc.mat = nucleus.matrix(i, ref.frame, start.frame, end.frame, range)
    points(curr.nuc.mat[,1], curr.nuc.mat[,2])
  }
}

#2. plot for each nuc that shows how many children it has frame to frame
#arguments are same as they have been
plot.nuc = function(nuc.num, ref.frame, start.frame, end.frame, range) 
{
  nucX.mat = nucleus.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  frame = start.frame:end.frame
  nuc.name = toString(nuc.num)
  print(nuc.name)
  title = paste("Nucleus", nuc.name)
  plot(frame, nucX.mat[,3], xlab = "Frame Number", ylab = "Number of Children", ylim = c(0,3), xlim = c(0,25),
       main = title)
  for(i in 1:length(frame))
  {
    if(toString(nucX.mat[i,3]) == "NA")
    {
      abline(v = ((i + start.frame) - 1), untf = FALSE, col = "Red")
    }
  }
}

#This function is going to check to see if there are any duplicates (i.e. see if the same nucleus is counted as 2 dif ones)
check.duplicates = function(ref.frame, start.frame, end.frame, range)
{
  count = 0 #set the count for duplicates to be equal to 0
  #get the total number of nuclei we will be looking at based on the referance frame
  total.nucs = num.nuc.frame(ref.frame)
  #Need to cycle through each of the nuclei (remember total number based on the referance frame)
  for(i in 1:total.nucs)
  {
    #create a current nuc matrix representing the nucleus you are on that you will then compare to all other nuc matrices
    curr.mat = nucleus.matrix(i, ref.frame, start.frame, end.frame, range)
    #now run through every other nuc matrix to check for duplicate (dont need to look at previous matrices cause those
    #were already checked in the previous steps)
    p = i + 1
    if(p > total.nucs)
    {
      break
    }
    for(j in p:total.nucs)
    {
      next.mat = nucleus.matrix(j, ref.frame, start.frame, end.frame, range)
      increment = (end.frame - start.frame) + 1
      #now, within each matrix, need to compare every single x,y coordinate pair
      for(k in 1:increment)
      {
        for(l in 1:increment)
        {
          #if a duplicate is found, act accordingly, but first, need to account for if there is an NA found:
          if(is.na(curr.mat[k,1]) == TRUE | is.na(curr.mat[k,2]) == TRUE | is.na(next.mat[l,1]) == TRUE | 
             is.na(next.mat[l,2]) == "NA")
          {
            next
          }
          if(curr.mat[k,1] == next.mat[l,1] && curr.mat[k,2] == next.mat[l,2])
          {
            count = count + 1
            print("The nuclei numbers that match are:")
            print(c(i,j))
            print("In frames (first number corresponds to first nucleus printed above):")
            name_1 = (start.frame + k) - 1
            name_2 = (start.frame + l) - 1
            print(c(name_1, name_2))
          }
        }
      }
    }
  }
  print("So the total number of duplicates is:")
  print(count)
}

#Now, we want to introduce the spot data, and create an even more complex matrix for each nuclei that will hopefully
#contain frame #, coordinates, # children, and then coordinates for the children, and mean intensity for each spot

#Before we make that function, let's read in our new data set:
data.set.2 = read.table("CellCycle1Spots2.txt", header=TRUE, sep="\t")
data.set.2 = na.omit(data.set.2)

frame.numbers.2 = delete.emptyFrames(data.set.2)
x.locs.spots = data.set.2[,2]
y.locs.spots = data.set.2[,3]
integrated.intens = data.set.2[,4]
parent.nuc = data.set.2[,5]

#what we know here that's really valuable is that the parent.nuc number here matches up with the object number (in each
#frame) from the previous data set

#NOTE: THIS ASSUMES THAT THERE ARE EITHER 0, 1, 2, 3, or 4 CHILDREN (NO 5s OR HIGHER)
total.nuc.matrix = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  #First, generate the simple matrix for this given nuclei
  basic.mat = nucleus.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #Now, create an empty matrix of NA's that we can concatenate with the above matrix to create skeleton for new matrix
  columns = c("Child1.X", "Child1.Y", "Intense.1", "Child2.X", "Child2.Y", "Intense.2", 
              "Child3.X", "Child3.Y", "Intense.3", "Child4.X", "Child4.Y", "Intense.4")
  rows = c()
  len.rows = 1 + (end.frame - start.frame)
  for(i in 0:(len.rows-1))
  {
    val = paste("Frame", (i+start.frame), sep="")
    rows = c(rows, val)
  }
  add.mat = matrix(NA, nrow = (end.frame - start.frame) + 1, ncol = 12, dimnames = list(rows, columns))
  
  #Now, let's actually do that concatenation
  final.mat = cbind(basic.mat,add.mat)
  
  #Now, need to find all data and fill it into the appropriate locations
  for(i in 1:((end.frame-start.frame)+1))
  {
    frameNum = (start.frame + i) - 1
    nucX = final.mat[i,1]
    nucY = final.mat[i,2]
    numChildren = final.mat[i,3]
    
    #need to move on if the current row is all NAs; only need to check one, cause if one is, all others are
    if(is.na(nucX) == TRUE)
    {
      next
    }
    
    #Find the object number from the first data set to this specific nuclei in order to determine parent nuclei number
    for(j in 1:length(frame.numbers))
    {
      if(frameNum == frame.numbers[j] && nucX == x.locs[j] && nucY == y.locs[j] && numChildren == fish.count[j])
      {
        object.num = object.numbers[j] #recall, object number corresponds to parent nuclei
        break
      }
    }
    
    #now that we have the parent nuclei we can find the corresponding children coordinates in the second data set
    for(k in 1:length(frame.numbers.2))
    {
      if(frame.numbers.2[k] == frameNum && parent.nuc[k] == object.num)
      {
        if(is.na(final.mat[i,4]) == TRUE)
        {
          final.mat[i,4] = x.locs.spots[k]
          final.mat[i,5] = y.locs.spots[k]
          final.mat[i,6] = mean.intens[k]
          next
        }
        if(is.na(final.mat[i,4]) == FALSE && is.na(final.mat[i,7]) == TRUE)
        {
          final.mat[i,7] = x.locs.spots[k]
          final.mat[i,8] = y.locs.spots[k]
          final.mat[i,9] = mean.intens[k]
          next
        }
        if(is.na(final.mat[i,4]) == FALSE && is.na(final.mat[i,7]) == FALSE && is.na(final.mat[i,10]) == TRUE)
        {
          final.mat[i,10] = x.locs.spots[k]
          final.mat[i,11] = y.locs.spots[k]
          final.mat[i,12] = mean.intens[k]
          next
        }
        if(is.na(final.mat[i,4]) == FALSE && is.na(final.mat[i,7]) == FALSE && is.na(final.mat[i,10]) == FALSE)
        {
          final.mat[i,13] = x.locs.spots[k]
          final.mat[i,14] = y.locs.spots[k]
          final.mat[i,15] = mean.intens[k]
          next
        }
      }
    }
  }
  return(final.mat)
}

#Need to have a function that plots all this data for a nucleus in representative way:
#NOTE: THIS ASSUMES THAT THERE ARE EITHER 0, 1, 2, 3, or 4 CHILDREN (NO 5s OR HIGHER)
distance.plot = function(nuc.num, ref.frame, start.frame, end.frame, range, conversion.dist, conversion.time)
{
  #first, create the total matrix that the data is going to be drawn from:
  nuc.info.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #create vector to store the distances in
  distances.vec = c()
  
  #Now, go through each frame and determine the distance apart that the chromosomes are
  nrows = length(nuc.info.mat[,1])
  for(i in 1:nrows)
  {
    #first, deal with NAs and the few cases where 0 children, but nucleus found
    if(is.na(nuc.info.mat[i,3]) == TRUE | nuc.info.mat[i,3] == 0)
    {
      distance = NA
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 1)
    {
      distance = 0
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 2)
    {
      x1 = nuc.info.mat[i,4]
      y1 = nuc.info.mat[i,5]
      x2 = nuc.info.mat[i,7]
      y2 = nuc.info.mat[i,8]
      a = (x1 - x2)^2
      b = (y1 - y2)^2
      distance = sqrt(a + b)
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 3)
      #if 3, find shortest distance, toss it out, then average the two longer distances
      #doing this because one chromosome must be split into sister chromatids
    {
      #find first distance
      x1.1 = nuc.info.mat[i,4]
      y1.1 = nuc.info.mat[i,5]
      x2.1 = nuc.info.mat[i,7]
      y2.1 = nuc.info.mat[i,8]
      a.1 = (x1.1 - x2.1)^2
      b.1 = (y1.1 - y2.1)^2
      distance.1 = sqrt(a.1 + b.1)
      #find second distance
      x1.2 = nuc.info.mat[i,4]
      y1.2 = nuc.info.mat[i,5]
      x2.2 = nuc.info.mat[i,10]
      y2.2 = nuc.info.mat[i,11]
      a.2 = (x1.2 - x2.2)^2
      b.2 = (y1.2 - y2.2)^2
      distance.2 = sqrt(a.2 + b.2)
      #find third distance
      x1.3 = nuc.info.mat[i,7]
      y1.3 = nuc.info.mat[i,8]
      x2.3 = nuc.info.mat[i,10]
      y2.3 = nuc.info.mat[i,11]
      a.3 = (x1.3 - x2.3)^2
      b.3 = (y1.3 - y2.3)^2
      distance.3 = sqrt(a.3 + b.3)
      #find minimum distance 
      first.vec = c(distance.1, distance.2, distance.3)
      min = min(vec)
      second.vec = c()
      for(i in 1:3)
      {
        if(first.vec[i] == min)
        {
          next
        }
        else
        {
          second.vec = c(second.vec, first.vec[i])
        }
      }
      distance = mean(second.vec)
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 4)
      #if 4, find two longest distances, average them
      #this might be slightly too long... but can do it under human error assumption
    {
      #find first distance
      x1.1 = nuc.info.mat[i,4]
      y1.1 = nuc.info.mat[i,5]
      x2.1 = nuc.info.mat[i,7]
      y2.1 = nuc.info.mat[i,8]
      a.1 = (x1.1 - x2.1)^2
      b.1 = (y1.1 - y2.1)^2
      distance.1 = sqrt(a.1 + b.1)
      #find second distance
      x1.2 = nuc.info.mat[i,4]
      y1.2 = nuc.info.mat[i,5]
      x2.2 = nuc.info.mat[i,10]
      y2.2 = nuc.info.mat[i,11]
      a.2 = (x1.2 - x2.2)^2
      b.2 = (y1.2 - y2.2)^2
      distance.2 = sqrt(a.2 + b.2)
      #find third distance
      x1.3 = nuc.info.mat[i,7]
      y1.3 = nuc.info.mat[i,8]
      x2.3 = nuc.info.mat[i,10]
      y2.3 = nuc.info.mat[i,11]
      a.3 = (x1.3 - x2.3)^2
      b.3 = (y1.3 - y2.3)^2
      distance.3 = sqrt(a.3 + b.3)
      #find fourth distance
      x1.4 = nuc.info.mat[i,4]
      y1.4 = nuc.info.mat[i,5]
      x2.4 = nuc.info.mat[i,13]
      y2.4 = nuc.info.mat[i,14]
      a.4 = (x1.4 - x2.4)^2
      b.4 = (y1.4 - y2.4)^2
      distance.4 = sqrt(a.4 + b.4)
      #find fifth distance
      x1.5 = nuc.info.mat[i,7]
      y1.5 = nuc.info.mat[i,8]
      x2.5 = nuc.info.mat[i,13]
      y2.5 = nuc.info.mat[i,14]
      a.5 = (x1.5 - x2.5)^2
      b.5 = (y1.5 - y2.5)^2
      distance.5 = sqrt(a.5 + b.5)
      #find sixth distance
      x1.6 = nuc.info.mat[i,10]
      y1.6 = nuc.info.mat[i,11]
      x2.6 = nuc.info.mat[i,13]
      y2.6 = nuc.info.mat[i,14]
      a.6 = (x1.6 - x2.6)^2
      b.6 = (y1.6 - y2.6)^2
      distance.6 = sqrt(a.6 + b.6)
      
      #find the two longest distances
      new.vec = c(distance.1, distance.2, distance.3, distance.4, distance.5, distance.6)
      max1 = max(new.vec)
      index1 = which(new.vec == max1)
      new.vec = new.vec[-index1]
      max2 = max(new.vec)
      distance = (max1 + max2)/2
      distances.vec = c(distances.vec, distance)
      next
    }
  }
  
  #now, plot the distances vs. the frame!
  x.frames = start.frame:end.frame
  len = length(x.frames)
  x.values = conversion.time*(seq(1,len,by=1))
  y.values = distances.vec*conversion.dist #need to convert to distance instead of pixels
  title = paste("Nucleus", nuc.num)
  plot(x.values, y.values, xlab = "Time (s)", ylab = "Distance (nm)", main = title, type = "l")
  range = length(x.values) - 1
  counts = 0
  for(i in 1:range)
  {
    if(is.na(y.values[i]) == TRUE)
    {
      counts = counts + 1
      next
    }
    if(is.na(y.values[i]) == FALSE && counts > 0)
    {
      dist = counts + 1
      x0 = x.values[i - dist]
      y0 = y.values[i - dist]
      x1 = x.values[i]
      y1 = y.values[i]
      segments(x0, y0, x1, y1, lty = "dashed")
      counts = 0
    }
  }
}

#now, want this same function, but that doesnt plot it... just returns the y-values
distance.vec = function(nuc.num, ref.frame, start.frame, end.frame, range, conversion.dist)
{
  #first, create the total matrix that the data is going to be drawn from:
  nuc.info.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  
  #create vector to store the distances in
  distances.vec = c()
  
  #Now, go through each frame and determine the distance apart that the chromosomes are
  nrows = length(nuc.info.mat[,1])
  for(i in 1:nrows)
  {
    #first, deal with NAs and the few cases where 0 children, but nucleus found
    if(is.na(nuc.info.mat[i,3]) == TRUE | nuc.info.mat[i,3] == 0)
    {
      distance = NA
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 1)
    {
      distance = 0
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 2)
    {
      x1 = nuc.info.mat[i,4]
      y1 = nuc.info.mat[i,5]
      x2 = nuc.info.mat[i,7]
      y2 = nuc.info.mat[i,8]
      a = (x1 - x2)^2
      b = (y1 - y2)^2
      distance = sqrt(a + b)
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 3)
      #if 3, find shortest distance, toss it out, then average the two longer distances
      #doing this because one chromosome must be split into sister chromatids
    {
      #find first distance
      x1.1 = nuc.info.mat[i,4]
      y1.1 = nuc.info.mat[i,5]
      x2.1 = nuc.info.mat[i,7]
      y2.1 = nuc.info.mat[i,8]
      a.1 = (x1.1 - x2.1)^2
      b.1 = (y1.1 - y2.1)^2
      distance.1 = sqrt(a.1 + b.1)
      #find second distance
      x1.2 = nuc.info.mat[i,4]
      y1.2 = nuc.info.mat[i,5]
      x2.2 = nuc.info.mat[i,10]
      y2.2 = nuc.info.mat[i,11]
      a.2 = (x1.2 - x2.1)^2
      b.2 = (y1.2 - y2.1)^2
      distance.2 = sqrt(a.2 + b.2)
      #find third distance
      x1.3 = nuc.info.mat[i,7]
      y1.3 = nuc.info.mat[i,8]
      x2.3 = nuc.info.mat[i,10]
      y2.3 = nuc.info.mat[i,11]
      a.3 = (x1.3 - x2.3)^2
      b.3 = (y1.3 - y2.3)^2
      distance.3 = sqrt(a.3 + b.3)
      #find minimum distance 
      first.vec = c(distance.1, distance.2, distance.3)
      min = min(vec)
      second.vec = c()
      for(i in 1:3)
      {
        if(first.vec[i] == min)
        {
          next
        }
        else
        {
          second.vec = c(second.vec, first.vec[i])
        }
      }
      distance = mean(second.vec)
      distances.vec = c(distances.vec, distance)
      next
    }
    if(nuc.info.mat[i,3] == 4)
      #if 4, find two longest distances, average them
      #this might be slightly too long... but can do it under human error assumption
    {
      #find first distance
      x1.1 = nuc.info.mat[i,4]
      y1.1 = nuc.info.mat[i,5]
      x2.1 = nuc.info.mat[i,7]
      y2.1 = nuc.info.mat[i,8]
      a.1 = (x1.1 - x2.1)^2
      b.1 = (y1.1 - y2.1)^2
      distance.1 = sqrt(a.1 + b.1)
      #find second distance
      x1.2 = nuc.info.mat[i,4]
      y1.2 = nuc.info.mat[i,5]
      x2.2 = nuc.info.mat[i,10]
      y2.2 = nuc.info.mat[i,11]
      a.2 = (x1.2 - x2.2)^2
      b.2 = (y1.2 - y2.2)^2
      distance.2 = sqrt(a.2 + b.2)
      #find third distance
      x1.3 = nuc.info.mat[i,7]
      y1.3 = nuc.info.mat[i,8]
      x2.3 = nuc.info.mat[i,10]
      y2.3 = nuc.info.mat[i,11]
      a.3 = (x1.3 - x2.3)^2
      b.3 = (y1.3 - y2.3)^2
      distance.3 = sqrt(a.3 + b.3)
      #find fourth distance
      x1.4 = nuc.info.mat[i,4]
      y1.4 = nuc.info.mat[i,5]
      x2.4 = nuc.info.mat[i,13]
      y2.4 = nuc.info.mat[i,14]
      a.4 = (x1.4 - x2.4)^2
      b.4 = (y1.4 - y2.4)^2
      distance.4 = sqrt(a.4 + b.4)
      #find fifth distance
      x1.5 = nuc.info.mat[i,7]
      y1.5 = nuc.info.mat[i,8]
      x2.5 = nuc.info.mat[i,13]
      y2.5 = nuc.info.mat[i,14]
      a.5 = (x1.5 - x2.5)^2
      b.5 = (y1.5 - y2.5)^2
      distance.5 = sqrt(a.5 + b.5)
      #find sixth distance
      x1.6 = nuc.info.mat[i,10]
      y1.6 = nuc.info.mat[i,11]
      x2.6 = nuc.info.mat[i,13]
      y2.6 = nuc.info.mat[i,14]
      a.6 = (x1.6 - x2.6)^2
      b.6 = (y1.6 - y2.6)^2
      distance.6 = sqrt(a.6 + b.6)
      
      #find the two longest distances
      new.vec = c(distance.1, distance.2, distance.3, distance.4, distance.5, distance.6)
      max1 = max(new.vec)
      index1 = which(new.vec == max1)
      new.vec = new.vec[-index1]
      max2 = max(new.vec)
      distance = (max1 + max2)/2
      distances.vec = c(distances.vec, distance)
      next
    }
  }
  #now perform the conversion!
  distances.vec = distances.vec*conversion.dist
  return(distances.vec)
}

#Now, want to create a plot that has all of the nuclei from above in the same plot
all.nuc.plot = function(ref.frame, start.frame, end.frame, range, conversion.dist, conversion.time)
{
  #initialize plot
  y.values = distance.vec(1, ref.frame, start.frame, end.frame, range)
  x.frames = start.frame:end.frame
  len = length(x.frames)
  x.values = conversion.time*(seq(1,len,by=1))
  plot(x.values, y.values, ylim = c(0,30), xlab = "Time", ylab = "Chromosome Distance Apart", type = "l")
  range = length(x.values) - 1
  counts = 0
  for(i in 1:range)
  {
    if(is.na(y.values[i]) == TRUE)
    {
      counts = counts + 1
      next
    }
    if(is.na(y.values[i]) == FALSE && counts > 0)
    {
      dist = counts + 1
      x0 = x.values[i - dist]
      y0 = y.values[i - dist]
      x1 = x.values[i]
      y1 = y.values[i]
      segments(x0, y0, x1, y1, lty = "dashed")
      counts = 0
    }
  }  
  #now run through rest
  num.nucs = num.nuc.frame(ref.frame)
  for(i in 2:num.nucs)
  {
    y.values = distance.vec(i, ref.frame, start.frame, end.frame, range, conversion.dist)
    x.frames = start.frame:end.frame
    len = length(x.frames)
    x.values = conversion.time*(seq(1,len,by=1))
    col = c(5*i, 5*i, 5*i)
    points(x.values, y.values, xlab = "Time (s)", ylab = "Distance (nm)", type = "l", col = col)
    range = length(x.values) - 1
    counts = 0
    for(i in 1:range)
    {
      if(is.na(y.values[i]) == TRUE)
      {
        counts = counts + 1
        next
      }
      if(is.na(y.values[i]) == FALSE && counts > 0)
      {
        dist = counts + 1
        x0 = x.values[i - dist]
        y0 = y.values[i - dist]
        x1 = x.values[i]
        y1 = y.values[i]
        segments(x0, y0, x1, y1, lty = "dashed")
        counts = 0
      }
    }  
  }
}

#Create updated plot that is ggplot with mean and variance and all that; not sure I totally understand how
#this function works... but spent a lot of time looking into it and this is what I came up with
#in terms of how I think it all works, look at notes in evernote cause I have it outlined there

#this plot uses ggplot to spit out the mean distances apart for 
#NOTE: WHAT I STILL DON'T KNOW IS EXACTLY WHAT THE METHOD PARAMETER DOES
mean.plot = function(ref.frame, start.frame, end.frame, range, conversion.dist, conversion.time)
{
  #create empty vector to store stuff that can then be averaged for mean values
  num.nucs = num.nuc.frame(ref.frame)
  row = num.nucs
  col = length(start.frame:end.frame)
  mat = matrix(0, nrow = row, ncol = col)
  
  for(i in 1:num.nucs)
  {
    distance.curr = distance.vec(i,ref.frame,start.frame,end.frame,range,conversion.dist)
    for(j in 1:length(distance.curr))
    {
      mat[i,j] = distance.curr[j]
    }
  }
  #now find the means
  mean.distances = c()
  for(l in 1:col)
  {
    new.mean = mean(mat[,l], na.rm = TRUE)
    mean.distances = c(mean.distances, new.mean)
  }
  
  library(ggplot2)
  #create vectors for the data frame
  x.frames = start.frame:end.frame
  len = length(x.frames)
  x.values = conversion.time*(seq(1,len,by=1))
  y = mean.distances
  sd = stand.devs
  dframe = data.frame(x.values,y,sd)
  graph = ggplot(dframe, aes(x.values, y, ymin = y-sd, ymax = y+sd)) + 
    geom_point() + 
    geom_errorbar() + 
    labs(x = "Time (s)", y = "Distance (nm)") +
    ylim(0,4500)
}

#Now, let's create some functions to look into some summary statistics we might care about

#first, look at: over the course of the whole cell cycle, how much time in total are chromosomes spent together?
#this is going to be PER NUCLEUS; returns total number of non NA or 0 instances, number of frames children are together, and the
#proportion of these two things
time.together.perNuc = function(nuc.num, ref.frame, start.frame, end.frame, range)
{
  working.mat = total.nuc.matrix(nuc.num, ref.frame, start.frame, end.frame, range)
  children = working.mat[,3]
  registered.vals = 0
  count.together = 0
  for(i in children)
  {
    #I am going to throw out all cases of NAs and 0s
    if(is.na(i) == TRUE | i == 0)
    {
      next
    }
    if(i == 1)
    {
      count.together = count.together + 1
    }
    registered.vals = registered.vals + 1
  }
  prop.together = count.together/registered.vals
  important.vals = c(prop.together, count.together, registered.vals)
  return(important.vals)
}

#Now, let's do the same thing but combining all the per nucleus data
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

#Now, let's create a function that finds the proportion of nuclei that never show paired children
prop.no.paired = function(ref.frame, start.frame, end.frame, range)
{
  num.nucs = num.nuc.frame(ref.frame)
  counts = 0
  for(i in 1:num.nucs)
  {
    working.vec = time.together.perNuc(i, ref.frame, start.frame, end.frame, range)
    if(working.vec[2] == 0)
    {
      counts = counts + 1
    }
  }
  prop.never.together = counts/num.nucs
  return(prop.never.together)
}

#Finally, let's create a function that produces a bar graph showing the average amount of time (consecutive frames)
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

#Now, let's make some functions to look at the integrated intensity values

intensity.mat = function(ref.frame, start.frame, end.frame, range)
{
  num.nucs = num.nuc.frame(ref.frame)
  children.vec = c()
  intensity.vec = c()
  for(i in 1:num.nucs)
  {
    working.mat = total.nuc.matrix(i, ref.frame, start.frame, end.frame, range)
    for(f in 1:length(working.mat[,1]))
    {
      curr.num.child = working.mat[f,3]
      if(curr.num.child == 0 | is.na(curr.num.child) == TRUE)
      {
        next
      }
      for(k in 1:curr.num.child)
      {
        children.vec = c(children.vec, curr.num.child)
        curr.intensity.ind = 3*k + 3
        intensity.vec = c(intensity.vec, working.mat[f,curr.intensity.ind])
      }
    }
  }
  #now make matrix of children and intensity to return
  vec = c(children.vec, intensity.vec)
  columns = c("Child Number", "Intensity")
  child.intensity.mat = matrix(vec, ncol = 2, nrow = length(children.vec))
  colnames(child.intensity.mat) = columns
  
  
  return(child.intensity.mat)
}

plot.intensities = function(ref.frame, start.frame, end.frame, range)
{
  working.mat = intensity.mat(ref.frame, start.frame, end.frame, range)
  x.values = working.mat[,1]
  y.values = working.mat[,2]
  plot(x.values, y.values, xlab = "Number of Children", ylab = "Integrated Intensity")
}
