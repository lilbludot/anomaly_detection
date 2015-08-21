#this function takes a sequence of numbers and finds the indices of 
#the values at which the sequence turns from increasing to decreasing or vice-versa


find_turning_pts<-function(seq,turning_points=c(1), sgn=1, index=1){
      n<-length(turning_points)
      if (length(seq)==1 | length(seq)==0){
            return(turning_points)
      }
      diff<-seq[2]-seq[1]
      if (sign(diff) == 0) {
            sgn<-sign(seq[2])
            index<-index+1
            return(find_turning_pts(seq[2:length(seq)], turning_points,sgn,index))
      }
      else if (sign(diff)==sgn){
            index<-index+1
            return(find_turning_pts(seq[2:length(seq)], turning_points, sgn, index))
      }
      else {
            sgn<-sign(diff)
            turning_points<-c(turning_points, index)
            index<-index+1
            return(find_turning_pts(seq[2:length(seq)], turning_points, sgn, index))
            
      }
}

#s<-c(1,2,4,0,0,0,3,4,5,6,2,1)
#(s, c(0), 1, 1)
#recursor(s)
