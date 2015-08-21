#REDUNDANT 
#given an input of turning points and an input of sgn=-1 if the first intreval decreased
#sgn=1 if the first interval was increasing
#returns a data frame of intervals on which the original sequence increased

find_increasing_intervals<-function(x, sgn=1){
      n<-length(x)
      x1<-x[1:(n-1)]
      x2<-x[2:n]
      all_intervals<-data.frame(cbind(x1,x2))
      m<-nrow(all_intervals)
      
      odds<-2*c(0:(m %/% 2))+1
      evens<-2*c(1:(m %/% 2))
      if (sgn==1){
            increasing_intervals<-all_intervals[evens,]
            return(increasing_intervals) 
      }
      else {
            increasing_intervals<-all_intervals[odds,]
            return(increasing_intervals) 
      }
      
}