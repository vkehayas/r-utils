SampleData = function(dt, nDiv) {
  
  smallT = dt[sample(1:nrow(dt), round(nrow(dt)/nDiv))]
  
  return(smallT)
  
}