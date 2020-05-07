final.fish <- read.csv("finalfish.csv", header=TRUE)
chum <- final.fish[ which(final.fish$Species=='Chum'), ]
myvars <- c("ID_code", "Sex", "Length_TSFT_mm", "Chum_total", "Stomach_volume", "Ocean_age", "Set_time")
chum <- chum[myvars]

# only use data with sex identified
chum <- subset(chum, Sex %in% c("Female", "Male"))

# make sure data makes sense
#chum$Chum_total <- as.numeric(levels(chum$Chum_total))[chum$Chum_total]
chum <- chum[ which(chum$Chum_total > 0), ]
dim(chum)

fullness <- vector(mode="numeric", length=1331)

for (i in 1:1331){
  if (chum$Stomach_volume[i] == "Empty"){
    fullness[i] <- 0
  }
}

for (i in 1:1331){
  if (chum$Stomach_volume[i] == "Trace" || chum$Stomach_volume[i] == "Medium" || 
      chum$Stomach_volume[i] == "Full" || chum$Stomach_volume[i] == "Distended"){
    fullness[i] <- 1
  }
}

chum$fullness <- fullness

########################################
# RAW DATA #
########################################
# NORMAL BINS

#look at raw data
plot(chum$Chum_total, chum$fullness)

bin.y <- vector(mode="numeric", length=5)
bin.y.sd <- vector(mode="numeric", length=5)
bin.x <- vector(mode="numeric", length=5)

for (i in 1:5){
  data <- chum[ which(chum$Chum_total >= (i-1)*250 & chum$Chum_total < i*250), ]
  bin.y[i] <- mean(data$fullness)
  bin.y.sd[i] <- sd(data$fullness)
  bin.x[i] <- mean(data$Chum_total)
}

temp1 <- bin.x[!is.na(bin.y)]
temp2 <- bin.y[!is.na(bin.y)]
plot(temp1, temp2, type="l", xlab="Chum total", ylab="Chum fullness")

temp3 <- bin.y.sd[!is.na(bin.y)]
error <- temp3

raw <- cbind(bin.x, bin.y, bin.y-error, bin.y+error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

write.csv(raw, "chum_sf_raw.csv")
