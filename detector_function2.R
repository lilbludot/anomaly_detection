#this function takes a time series of length of at least 75
#and looks for anomalies using the statistical process control chart, the CUSUM chart
#TEMPORARY: the input should not be much longer than 365 
#FINAL VERSION: the input can be any length longer than 74


detector <- function(time_series){
      #temporary version
      require(qcc)
      require(ggplot2)
      require(zoo)
      n<-length(time_series)
      stopifnot(n>=75)
      source("training_data_finder_function.R")
      source("cusum_interpreter_function.R")
      training_data_object <- training_data_finder(time_series)
      if (length(training_data_object$training_data) == 0) {
            print("This method cannot be applied to this particular time series.")
      }
      else {
            
            training_data<-training_data_object$training_data
            #plotting the time_series and the training set by
            #creating a data frame that is to be plotted using ggplot2
            start_val<-training_data_object$segment_starting_point
            segment_length<-training_data_object$segment_length
            end_val<-segment_length + start_val-1
            
            #creating the CUSUM chart
            x_bar<-mean(training_data)
            sigma<-sd(training_data)
            k<-training_data_object$k+1
            H<-training_data_object$H
            anomaly_indices<-cusum_interpreter(time_series,x_bar,sigma,H,k)
            #print(anomaly_indices)
            anomaly_indices2<-cusum_interpreter(rollmean(time_series,3),x_bar,sigma,H,k)
            #print(anomaly_indices2)
            
            df <- data.frame(
                  index = 1:n,
                  y = time_series,
                  col = as.character(c(rep("black", (start_val-1)), 
                          rep("green", segment_length), 
                          rep("black", (n-end_val)))), 
                          stringsAsFactors = FALSE)
            lower_anomaly<-NULL
            upper_anomaly<-NULL
            if (length(anomaly_indices2$upper_anomaly)!= 0){
                  #df$col[anomaly_indices2$upper_anomaly]="yellow"
            }
            if (length(anomaly_indices2$lower_anomaly) != 0){
                  #df$col[anomaly_indices2$lower_anomaly] ="yellow"
            }
            if (length(anomaly_indices$upper_anomaly)!= 0){
                  df$col[anomaly_indices$upper_anomaly]="red"
                  upper_anomaly<-anomaly_indices$upper_anomaly
            }
           
            if (length(anomaly_indices$lower_anomaly) != 0){
                  df$col[anomaly_indices$lower_anomaly] ="red"
                  lower_anomaly<-anomaly_indices$lower_anomaly
                 
            }
          
          
            return(list(upper=upper_anomaly, lower=lower_anomaly))
            
      }
}