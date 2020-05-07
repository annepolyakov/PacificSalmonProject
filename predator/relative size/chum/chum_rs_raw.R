chum <- final.fish[ which(final.fish$Species=='Chum'), ]
myvars <- c("ID_code", "Length_TSFT_mm", "Chum_total", "total", "Injury","Sex", "Ocean_age", "Set_time")
chum <- chum[myvars]

# make sure data makes sense
chum <- chum[ which(chum$Chum_total > 0), ]

# create unique id and add it to data (enter total number of data entries for id number)
id <- numeric(14993)
id[1] <- 1
chum <- chum[order(chum$ID_code), ]
for (i in 2:14993){
  if (chum$ID_code[i] == chum$ID_code[i-1]){
    id[i] <- id[i-1]
  }
  else{
    id[i] <- id[i-1]+1
  }
}

chum <- data.frame(chum,id)
#RELATIVE SIZE
# add mean size and relative size of group to each data entry
temp <- data.frame("id"=1:930, "mean.size"=1:930)
for (i in 1:930){
  temp$mean.size[i] <- mean(chum$Length_TSFT_mm[chum$id==i])
}
chum <- merge(chum, temp, by="id")
for (i in 1:14993){
  chum$relative.size[i] <- chum$Length_TSFT_mm[i]/chum$mean.size[i]
}
# plot raw data for fish relative size
# normal bins
bin.y <- vector(mode="numeric", length=4)
error <- vector(mode="numeric", length=4)
bin.x <- vector(mode="numeric", length=4)

# remove outliers
plot(chum$relative.size, chum$Injury)
no.injury <- chum$relative.size[ which(chum$Injury=="0")]
boxplot.stats(no.injury)
hist(no.injury)
chum <- chum[ which(chum$relative.size < "1.7"),]

# bins
bin <- (max(chum$relative.size) - min(chum$relative.size))/4
for (i in 1:4){
  data <- chum[ which(chum$relative.size >= min(chum$relative.size)+((i-1)*bin) & chum$relative.size < min(chum$relative.size)+(i*bin)), ]
  bin.y[i] <- sum(data$Injury)/length(data$Injury)
  bin.x[i] <- mean(data$relative.size)
  n <- length(data$Injury)
  error[i] <- qt(0.975,df=n-1)*sd(data$Injury)/sqrt(n)
}

lower <- bin.y-error
upper <- bin.y+error

raw <- cbind(bin.x, bin.y, bin.y-error, bin.y+error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

write.csv(raw, "chum_rs_raw.csv")