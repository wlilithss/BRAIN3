```{r}
library(rlist)
library(tuneR)
library(randomForest)
library(e1071)
library(reshape2)

all_files= list.files("D:/University/DATA3888/Project/Data2")
print(all_files)
X =list()
Y=c()


# specify the path to the parent folder
parent_folder <- "D:/University/DATA3888/Project/Data2"

# loop through all folders in the parent folder
X=list()
Y=list()
id=1
for (folder in list.files(parent_folder, full.names = TRUE)){
  
  # check if the item in the parent folder is a folder
  if (file.info(folder)$isdir){
    for (txt_file in list.files(folder, pattern = "\\.txt$", full.names = TRUE)){
      
      events_list <- list()
      con <- file(txt_file, "r")
      while (TRUE) {
        line = readLines(con, n = 1)
        # print(line)
        if (length(line) == 0) {
          break
        }
        if (substr(line, 1, 1) == "#") {
          next
        }
        # split the line by comma
        line_split <- strsplit(line, ",")[[1]]
        
        # extract the value after the comma and add it to the list
        value <- trimws(line_split[2])
        events_list <- c(events_list, as.numeric(value))
      }
      close(con)

      
    }
    # print(length(events_list))

    # loop through all .wav files in the subfolder
    for (wav_file in list.files(folder, pattern = "\\.wav$", full.names = TRUE)){
      wave <- readWave(wav_file)
      
      # # print(events_list)
      # seq <- seq_len(length(wave))/wave@samp.rate 
      # plot(seq, wave@left, type = "l", ylab="Signal", xlab="Time(seconds)",ylim = c(-3000, 3000))
      # abline(v = events_list, col = "red")
      # break
      
      name_parts <- strsplit(basename(wav_file), split = "\\.")[[1]]
      name_chars <- unlist(strsplit(name_parts[1], split = ""))
      # print(length(name_chars))
      Y=append(Y,name_chars)
      for(i in 1:length(events_list)){
        start_index=round(events_list[[i]]*wave@samp.rate)
        stop_index=round((events_list[[i]]+0.75)*wave@samp.rate)
        xtemp=as.list(wave@left[start_index:stop_index])
        X=append(X,list(xtemp))
      }
      
    }
  }
}
# X = as.data.frame(do.call(rbind, t(X)))
X <- as.data.frame(do.call(rbind, lapply((X), function(x) unlist(x))))

melted_X=melt(t(X))
melted_X$Var1 <- as.integer(gsub("V", "", melted_X$Var1))
colnames(melted_X) <- c("Time","Id", "Values")
write.csv(melted_X, file = "Training_X.csv", row.names = FALSE)
write.csv(Y, file = "Training_Y.csv", row.names = FALSE)

```
