### ORDINAL REGRESSION ANALYSIS ###
## COHO SALMON ##

final.fish <- read.csv("finalfish.csv", header=TRUE)
coho <- final.fish[ which(final.fish$Species=='Coho'), ]
myvars <- c("ID_code", "Sex", "Length_TSFT_mm", "Coho_total", "Stomach_volume", "Ocean_age","Set_time")
coho <- coho[myvars]

# only use data with sex identified
coho <- subset(coho, Sex %in% c("Female", "Male"))

# remove two outliers
coho <- coho[coho$ID_code!="1966FRIH065",]

# make sure data makes sense
#coho$Coho_total <- as.numeric(levels(coho$Coho_total))[coho$Coho_total]
coho <- coho[ which(coho$Coho_total > 0), ]
dim(coho)

fullness <- vector(mode="numeric", length=434)

for (i in 1:434){
  if (coho$Stomach_volume[i] == "Empty"){
    fullness[i] <- 0
  }
}

for (i in 1:434){
  if (coho$Stomach_volume[i] == "Trace" || coho$Stomach_volume[i] == "Medium" || 
      coho$Stomach_volume[i] == "Full" || coho$Stomach_volume[i] == "Distended"){
    fullness[i] <- 1
  }
}

coho$fullness <- fullness

########################################
# RAW DATA #
########################################
# NORMAL BINS

# plot raw data
plot(coho$Coho_total, coho$fullness)

# 10 and 60
# 70 and 4
# 4 and 60
# 41 and 6
bin.y <- vector(mode="numeric", length=4)
bin.y.sd <- vector(mode="numeric", length=4)
bin.x <- vector(mode="numeric", length=4)

for (i in 1:4){
  data <- coho[ which(coho$Coho_total >= (i-1)*60 & coho$Coho_total < i*60), ]
  bin.y[i] <- mean(data$fullness)
  bin.y.sd[i] <- sd(data$fullness)
  bin.x[i] <- mean(data$Coho_total)
}

temp1 <- bin.x[!is.na(bin.y)]
temp2 <- bin.y[!is.na(bin.y)]
plot(temp1, temp2, type="l", xlab="Coho total", ylab="Coho fullness")

temp3 <- bin.y.sd[!is.na(bin.y)]

upper.error <- temp2+error
lower.error <- temp2-error

raw <- cbind(temp1, temp2, lower.error, upper.error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

write.csv(raw, "chum_sf_raw.csv")
