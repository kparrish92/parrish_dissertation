## Calculate euclidean distance
e_dist = function(x1, x2, y1, y2)
{
  d = sqrt((x1 - x2)^2  + (y1 - y2)^2)
  return(d)}

## Calculate probability given the two distances
calc_prob = function(dist1, dist2)
{
  sim_prob = dist1/(dist1 + dist2)
  return(sim_prob)
}
