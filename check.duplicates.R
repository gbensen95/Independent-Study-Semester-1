#checks to see if there are any duplicates (i.e. see if the same nucleus is counted as 2 dif ones)
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