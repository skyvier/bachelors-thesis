library(ggplot2)
library(ggthemes)

process_simulations <- function (folder_path, step_parameter) {
   read_degrees_file <- function (filename) {
      data.list <- read.table(filename)
      data <- unlist(data.list, use.names=F)
      data.tail <- Filter(function (x) { return(x >= 2); }, data)
      return(data.tail);
   }

   approx_cdf <- function (sample, ks) {
      mapped <- Map(function (k) { length(sample[sample >= k + 1]) / length(sample) }, ks)
      return(unlist(mapped))
   }

   files <- list.files(folder_path, pattern="^degrees_[0-9]+.csv")

   complete_data <- c()
   for (file in files) {
      degrees <- read_degrees_file(file.path(folder_path, file))
      complete_data <- c(complete_data, degrees) 
   }

   ks <- 1:max(complete_data)
   sample_cdf <- approx_cdf(complete_data, ks)
   lower_bound_cdf <- ks^(-step_parameter/2)

   jpeg(file.path(folder_path, 'approx_cdf.jpg'))

   plot(ks, lower_bound_cdf, log="y", type="l", lty=3, lwd=2, 
        ylab="Komplementaarinen kertymäfunktio", xlab="Asteluku",
        main="Komplementaarinen kertymäfunktio ja alaraja")
   lines(ks, sample_cdf, lty=5, lwd=2)
   grid()
   legend("topright", legend=c("Alaraja", "Approksimaatio"), lty=c(3, 5), lwd=c(2, 2), cex=1, inset=0.03)

   dev.off()

   init_indices <- 1:(0.998*length(complete_data))
   frequencies <- data.frame(init = sort(complete_data)[init_indices])
   hist.plot <- ggplot(frequencies, aes(x = init)) + geom_histogram(col="black", fill="grey") + 
      scale_x_log10(name = "Asteluku", breaks=c(2,10,100,1000,5000)) + 
      scale_y_continuous(name = "Määrä") + ggtitle("Astelukujen esiintyvyys") + 
      theme_hc() + theme(plot.title = element_text(hjust=0.5))
   ggsave(filename=file.path(folder_path, 'approx_hist.jpg'), plot=hist.plot)

   return(list(sample_cdf = sample_cdf, lower_bound_cdf = lower_bound_cdf, data = complete_data))
}
