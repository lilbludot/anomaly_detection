two_color_plotter<-function(time_series,segment_length, start_value, end_value){
      n<-length(time_series)
      df <- data.frame(
            index = 1:n,
            y = time_series,
            col = c(rep("black", (start_val-1)), 
                    rep("red", segment_length), 
                    rep("black", (n-end_val)))
      )
      ggplot(df, aes(x=index, y=y)) + 
            geom_line(aes(colour=col, group=1)) + 
            scale_colour_identity()
}