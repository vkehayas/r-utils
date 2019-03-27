SampleData = function(dt, nDiv) {
  
  smallT = dt[sample(1:nrow(dt), nrow(dt)/nDiv)]
  
  return(smallT)
  
}