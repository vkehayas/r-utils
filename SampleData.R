SampleData = function(dt, nDiv) {
  
  if (nDiv <= 1) {
    
    smallT = dt
    
  } else {
    
    nRow = nrow(dt)
    
    smallT = dt[sample(1:nRow, 
                       nRow/nDiv), ]
    
  }
  
  return(smallT)
  
}
