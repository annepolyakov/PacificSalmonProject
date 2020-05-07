### BINOMIAL REGRESSION ANALYSIS ###
## SOCKEYE SALMON ##

final.fish <- read.csv("finalfish.csv", header=TRUE)
sockeye <- final.fish[ which(final.fish$Species=='Sockeye'), ]
myvars <- c("ID_code", "Sex", "Length_TSFT_mm", "Sockeye_total", "Stomach_volume","Ocean_age","Set_time")
sockeye <- sockeye[myvars]

# only use data with sex identified
sockeye <- subset(sockeye, Sex %in% c("Female", "Male"))

# make sure data makes sense (sockeye total cannot be zero, if it is, assuming that data is wrong)
sockeye$Sockeye_total <- as.numeric(levels(sockeye$Sockeye_total))[sockeye$Sockeye_total]
sockeye <- sockeye[ which(sockeye$Sockeye_total > 0), ]
dim(sockeye)

fullness <- vector(mode="numeric", length=625)

for (i in 1:625){
  if (sockeye$Stomach_volume[i] == "Empty"){
    fullness[i] <- 0
  }
}

for (i in 1:625){
  if (sockeye$Stomach_volume[i] == "Trace" || sockeye$Stomach_volume[i] == "Medium" || 
      sockeye$Stomach_volume[i] == "Full" || sockeye$Stomach_volume[i] == "Distended"){
    fullness[i] <- 1
  }
}

sockeye$fullness <- fullness

######   RAW DATA ###########
#####################################################################

#plot raw data
plot(sockeye$Sockeye_total, sockeye$fullness)

# make bins
bin.y <- vector(mode="numeric", length=4)
bin.y.sd <- vector(mode="numeric", length=4)
bin.x <- vector(mode="numeric", length=4)

for (i in 1:4){
  data <- sockeye[ which(sockeye$Sockeye_total >= (i-1)*330 & sockeye$Sockeye_total < i*330), ]
  bin.y[i] <- mean(data$fullness)
  bin.y.sd[i] <- sd(data$fullness)
  bin.x[i] <- mean(data$Sockeye_total)
}
temp1 <- bin.x
temp2 <- bin.y
temp3 <- bin.y.sd
error <- temp3
plot(temp1, temp2, type="l", xlab="Sockeye total", ylab="Sockeye fullness")
lines(temp1, lower)
lines(temp1, upper)
raw <- cbind(temp1, temp2, temp2-error, temp2+error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

write.csv(raw, file ="sockeye_sf_raw.csv", row.names=TRUE)
