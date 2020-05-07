final.fish <- read.csv("finalfish.csv", header=TRUE)
pink <- final.fish[ which(final.fish$Species=='Pink'), ]
myvars <- c("ID_code", "Sex", "Length_TSFT_mm", "Pink_total", "Stomach_volume","Ocean_age", "Set_time")
pink <- pink[myvars]

# only use data with sex identified
pink <- subset(pink, Sex %in% c("Female", "Male"))

# make sure data makes sense
#pink$Pink_total <- as.numeric(levels(pink$Pink_total))[pink$Pink_total]
pink <- pink[ which(pink$Pink_total > 0), ]
dim(pink)

fullness <- vector(mode="numeric", length=386)

for (i in 1:386){
  if (pink$Stomach_volume[i] == "Empty"){
    fullness[i] <- 0
  }
}

for (i in 1:386){
  if (pink$Stomach_volume[i] == "Trace" || pink$Stomach_volume[i] == "Medium" || 
      pink$Stomach_volume[i] == "Full" || pink$Stomach_volume[i] == "Distended"){
    fullness[i] <- 1
  }
}

pink$fullness <- fullness

########################################
# RAW DATA #
########################################

######   RAW DATA ###########
#####################################################################

# # plot raw data
plot(pink$Pink_total, pink$fullness)

# NORMAL BINS
bin.y <- vector(mode="numeric", length=6)
bin.y.sd <- vector(mode="numeric", length=6)
bin.x <- vector(mode="numeric", length=6)

for (i in 1:6){
  data <- pink[ which(pink$Pink_total >= (i-1)*166 & pink$Pink_total < i*166), ]
  bin.y[i] <- mean(data$fullness)
  bin.y.sd[i] <- sd(data$fullness)
  bin.x[i] <- mean(data$Pink_total)
}
temp1 <- bin.x[!is.na(bin.y)]
temp2 <- bin.y[!is.na(bin.y)]
plot(temp1, temp2, type="l", xlab="Pink total", ylab="Pink fullness")
temp3 <- bin.y.sd
error <- temp3

raw <- cbind(temp1, temp2, temp2 - error, temp2 + error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

write.csv(raw, "pink_sf_raw.csv")