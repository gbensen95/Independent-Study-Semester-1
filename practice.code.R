#This is the script I will use to actually run all the functions that I have mad

#install packages I will need:
#install.packages("ggplot2")

#set working director
setwd("~/Desktop/Bowdoin Senior Fall/Ind. Study/R Functions")
#now source all functions so they can be utilized
source("all.nuc.plot.R")
source("check.duplicates.R")
source("delete.emptyFrames.R")
source("distance.plot.R")
source("distance.vec.R")
source("frame.matrix.R")
source("frames.together.bar.R")
source("Functions.EmptyFramesRemoved.R")
source("Functions.Updated.R")
source("get.indeces.frame.R")
source("intensity.mat.R")
source("mean.plot.R")
source("nucleus.matrix.R")
source("num.nuc.frame.R")
source("intensities.boxplot.R")
source("intensities.dotplot.R")
source("intensities.violinplot.R")
source("plot.nuc.R")
source("plot.tot.view.R")
source("prop.no.paired.R")
source("time.together.perNuc.R")
source("time.together.total.R")
source("total.nuc.matrix.R")
source("child.track.plotAll.R")
source("tracking.spots.algorithm.R")
source("track.spots.R")
source("plot.spots.R")

#read in data set that contains nucleus info
data.set = read.table("CellCycle1.txt", header=TRUE, sep="\t")
data.set = na.omit(data.set)

frame.numbers = delete.emptyFrames(data.set)
object.numbers = data.set[,2]
fish.count = data.set[,3]
x.locs = data.set[,4]
y.locs = data.set[,5]

#read in data set that contains child (spots) info
data.set.2 = read.table("CellCycle1Spots2.txt", header=TRUE, sep="\t")
data.set.2 = na.omit(data.set.2)

frame.numbers.2 = delete.emptyFrames(data.set.2)
x.locs.spots = data.set.2[,2]
y.locs.spots = data.set.2[,3]
integrated.intens = data.set.2[,4]
parent.nuc = data.set.2[,5]

#check child.track.plotAll function
#num = num.nuc.frame(4)
#for(i in 1:num)
#{
 # child.track.plotAll(i,4,4,23,15)
#}

#check child.track.plot.condensed function
#num = num.nuc.frame(4)
#for(i in 1:num)
#{
 # child.track.plot.condensed(i,4,4,23,15)
#}

#check tracking.spots.algorithm
#num = num.nuc.frame(4)
#for(i in 1:num)
#{
 # print(tracking.spots.algorithm(i,4,4,23,15))
#}

#working with the tracking algorithm to find the best nuclei
#best nuclei are those that track child 1 all the way through each frame correctly
#by correctly i mean child 1 of the next frame is the closest child to position of child 1
#in the old frame
#num = num.nuc.frame(4)
#best.nucs = c()
#for(i in 1:num)
#{
#  a = tracking.spots.algorithm(i,4,4,23,15)
#  b = which(a == 1)
#  if((length(a) == length(b)) & length(a) >= 1)
#  {
#    best.nucs = c(best.nucs, i)
#  }
#}

#now, let's test the plotting function out to plot these best nuclei 
#for(i in best.nucs)
#{
#  plot.spots(i,4,4,23,15)
#}

