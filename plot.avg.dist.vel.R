#this function creates violin plots for avg dist moved by child per frame and avg velocity of child
#throughout cell cycle

plot.avg.dist.vel = function(ref.frame, start.frame, end.frame, range, time.conversion)
{
  dist.mat = avg.dist(ref.frame, start.frame, end.frame, range)
  velocity.mat = avg.velocity(ref.frame, start.frame, end.frame, range, time.conversion)
  
  dist.vals = dist.mat[,3]
  velocity.vals = velocity.mat[,3]
  
  library(ggplot2)
  dframe.dist = data.frame(0, dist.vals)
  dframe.velocity = data.frame(0, velocity.vals)
  
  graph.dist = ggplot(dframe.dist, aes(x = factor(0), y = dist.vals)) + 
    labs(x = "Place Holder", y = "Avg Distance Traveled by Child Per Frame") + 
    geom_violin(scale = "area", alpha = 0.6, fill = "red", na.rm = TRUE)
  
  graph.velocity = ggplot(dframe.velocity, aes(x = factor(0), y = velocity.vals)) + 
    labs(x = "Place Holder", y = "Avg Velocity for Child in Cell Cycle (pixels/s)") + 
    geom_violin(scale = "area", alpha = 0.6, fill = "blue", na.rm = TRUE)
  
  print(graph.dist)
  print(graph.velocity)
}
