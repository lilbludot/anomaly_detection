#this function takes a daily time series with length greater than 
#365 and the year, month, and day of its first entry
#and looks for anomalies using a moving window of length 365

window<-function(ts, year=2012,mo=01,day=01, window_length=365){
      source("detector_function2.R")
      w<-window_length
      n<-length(ts)
      print(n)
      year<-as.character(year)
      mo<-as.character(mo)
      day<-as.character(day)
      the_date<-paste(year,"/",mo,"/",day, collapse="")
      the_date<-gsub(" ","", the_date)
      print(the_date)
      dates<-seq(as.Date(the_date), by = "day",length.out = n) 
      plotter_df<-data.frame(index=1:n, y=ts, date=dates)
      for (i in 1:(n-w)){
            plotter_df[[i+3]]<-rep(NA, n)
      }
      color_plotter_df<-data.frame(index=1:n, y=ts, date=dates, stringsAsFactors = FALSE)
      for (i in 1:(n-w)){
            color_plotter_df[[i+3]]<-as.character(rep("black", n))
      }
      for (i in 1:(n-w)){
            x<-ts[i:(i+w-1)]
            obj<-detector(x)
            if (length(obj$upper) != 0){
                  plotter_df[[i+3]][i:(i+w-1)][obj$upper]<-
                        plotter_df$y[i:(i+w-1)][obj$upper]
                  color_plotter_df[[i+3]][i:(i+w-1)][obj$upper]<-"firebrick1"
            }
            if (length(obj$lower) != 0){
                  plotter_df[[i+3]][i:(i+w-1)][obj$lower]<-
                        plotter_df$y[i:(i+w-1)][obj$lower]
                  color_plotter_df[[i+3]][i:(i+w-1)][obj$lower]<-"darkorange"
            }
            
            
      }
      return(list(df1=plotter_df,df2=color_plotter_df))
      
}