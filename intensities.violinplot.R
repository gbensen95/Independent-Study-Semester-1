#takes the values in intensity.mat and creates a violin plot
intensities.violinplot = function(ref.frame, start.frame, end.frame, range)
{
  working.mat = intensity.mat(ref.frame, start.frame, end.frame, range)
  x.values = working.mat[,1]
  y.values = working.mat[,2]
  
  library(ggplot2)
  dframe = data.frame(x.values, y.values)
  
  #First, do a boxplot
  graph = ggplot(dframe, aes(x = factor(x.values), y = y.values)) + 
    labs(x = "Number of Children", y = "Integrated Intensity") + 
    geom_violin(scale = "area", alpha = 0.6, fill = "red")
  
  return(graph)
}