#takes the values in intensity.mat and creates a box plot
intensities.boxplot = function(ref.frame, start.frame, end.frame, range)
{
  working.mat = intensity.mat(ref.frame, start.frame, end.frame, range)
  x.values = working.mat[,1]
  y.values = working.mat[,2]
  
  library(ggplot2)
  dframe = data.frame(x.values, y.values)
  
  #First, do a boxplot
  graph = ggplot(dframe, aes(x = factor(x.values), y = y.values)) + 
    labs(x = "Number of Children", y = "Integrated Intensity") + 
    geom_boxplot()
  
  return(graph)
}