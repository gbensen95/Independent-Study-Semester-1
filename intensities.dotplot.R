#takes the values in intensity.mat and creates a dot plot
intensities.dotplot = function(ref.frame, start.frame, end.frame, range)
{
  working.mat = intensity.mat(ref.frame, start.frame, end.frame, range)
  x.values = working.mat[,1]
  y.values = working.mat[,2]
  
  library(ggplot2)
  dframe = data.frame(x.values, y.values)
  
  #First, do a boxplot
  graph = ggplot(dframe, aes(x = factor(x.values), y = y.values)) + 
    labs(x = "Number of Children", y = "Integrated Intensity") + 
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.75, alpha = 0.4,
                 fill = "red", color = "black")
  
  return(graph)
}