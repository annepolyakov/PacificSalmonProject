sockeye <- final.fish[ which(final.fish$Species=='Sockeye'), ]
myvars <- c("ID_code", "Length_TSFT_mm", "Sockeye_total", "total", "Injury", "Sex", "Ocean_age", "Set_time", "Year.x")
sockeye <- sockeye[myvars]

# make sure data makes sense
sockeye <- sockeye[ which(sockeye$Sockeye_total > 0), ]

# create unique id and add it to data (enter total number of data entries for id number)
id <- numeric(17116)
id[1] <- 1
sockeye <- sockeye[order(sockeye$ID_code), ]
for (i in 2:17116){
  if (sockeye$ID_code[i] == sockeye$ID_code[i-1]){
    id[i] <- id[i-1]
  }
  else{
    id[i] <- id[i-1]+1
  }
}

sockeye <- data.frame(sockeye,id)

# RELATIVE SIZE DATA
# add mean size and relative size of group to data
temp <- data.frame("id"=1:813, "mean.size"=1:813)
for (i in 1:813){
  temp$mean.size[i] <- mean(sockeye$Length_TSFT_mm[sockeye$id==i])
}

sockeye <- merge(sockeye, temp, by="id")

for (i in 1:17116){
  sockeye$relative.size[i] <- sockeye$Length_TSFT_mm[i]/sockeye$mean.size[i]
}

# plot raw dat for fish relative size
# normal bins
bin.y <- vector(mode="numeric", length=4)
error <- vector(mode="numeric", length=4)
bin.x <- vector(mode="numeric", length=4)

# remove outliers
sockeye <- sockeye[ which(sockeye$relative.size < "2"),]

plot(sockeye$relative.size, sockeye$Injury)

# normal bins
bin <- (max(sockeye$relative.size) - min(sockeye$relative.size))/4

for (i in 1:4){
  data <- sockeye[ which(sockeye$relative.size >= min(sockeye$relative.size)+((i-1)*bin) & sockeye$relative.size < min(sockeye$relative.size)+(i*bin)), ]
  bin.y[i] <- sum(data$Injury)/length(data$Injury)
  bin.x[i] <- mean(data$relative.size)
  n <- length(data$Injury)
  error[i] <- qt(0.945,df=n-1)*sd(data$Injury)/sqrt(n)
}

plot(bin.x, bin.y, type="l")

lower <- bin.y-error
upper <- bin.y+error

raw <- cbind(bin.x, bin.y, bin.y-error, bin.y+error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

write.csv(raw, "sockeye_rs_raw.csv")