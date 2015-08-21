

result_plotter<-function(df1,df2, main_title){
      options(expressions=10000)
      require(dplyr)
      opar <- par() 
      fnc<-function(v){
            return(length(v)-sum(is.na(v))-3)
      }
      df1$counts<-apply(df1, 1, fnc)
      source("df_maker_function3.R")
      df<-df1[,c("index","counts")]
      print(nrow(df1))
      #df<-filter(df, counts>0)
      print(nrow(df))
      dot_plot_df<-df_maker3(df)
      
     
      names(dot_plot_df)<-c("index","y")
      #print(tail(dot_plot_df, 24))
      m<-max(dot_plot_df$y, na.rm=TRUE)
      #print(m)
      labs<-seq(1,1200,by=200)
      #print(length(dot_plot_df$index))
      pdf( "DownloadThruputFinds.pdf", width = 20, height = 8 )
      par(mfrow = c(2,1),oma = c(2, 2, 1, 1), mar = c(2, 2, 2, 0)+0.1, xpd = NA)
      plot(df1$index, df1$y, type="l",xaxt = 'n', ylab="Measurement", xlab="", 
           main=main_title,
           col=rgb(0,0,0,alpha=0.7))
      points(df1$index, df1$y, col=rgb(0,0,0,alpha=0.6), cex=0.7, pch=1)
      
      for( i in (4: (ncol(df1)-1))){
            if (sum(is.na(df1[[i]]))!=nrow(df1)){
                  points(df1$index, df1[[i]], col=rgb(1,0.1,0, alpha=0.3), pch=19)
            }
      }
      #axis(1, at=labs, labels=df1$date[labs])
      
      plot(dot_plot_df$index, dot_plot_df$y, ylim=c(1,m*1.1),
           pch=20, col="red1", cex=1.2,
           ylab="Number of Votes", xlab="Date", bty="n", xaxt = 'n')
      axis(1, at=labs, labels=df1$date[labs])
      
      par(mfrow=c(1,1), mar= c(5, 4, 4, 2) + 0.1)
      #par(opar)
      dev.off()
      
      #, ylim=c(0,m*1.001) xlim=c(0,length(dot_plot_df$index)),
}