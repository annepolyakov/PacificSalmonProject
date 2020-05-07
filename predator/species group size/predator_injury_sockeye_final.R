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

# plot raw data

plot(sockeye$Sockeye_total, sockeye$Injury)

# normal bins
bin.y <- vector(mode="numeric", length=7)
error <- vector(mode="numeric", length=7)
bin.x <- vector(mode="numeric", length=7)

for (i in 1:7){
  data <- sockeye[ which(sockeye$Sockeye_total >= (i-1)*71 & sockeye$Sockeye_total < i*71), ]
  #bin.y[i] <- (sum(data$Injury)+0.5)/(length(data$Injury)+1)
  bin.y[i] <- (sum(data$Injury))/(length(data$Injury))
  bin.x[i] <- mean(data$Sockeye_total)
  n <- length(data$Injury)
  error[i] <- 1.5*sqrt(bin.y[i]*(1-bin.y[i])/(length(data$Injury+1)))
  #error[i] <- qt(0.975,df=n-1)*sd(data$Injury)/sqrt(n)
}


lower <- bin.y-error
upper <- bin.y+error

plot(bin.x, bin.y, type="l")
lines(bin.x, lower, type="l")
lines(bin.x, upper, type="l")

raw <- cbind(bin.x, bin.y, bin.y-error, bin.y+error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

## run binomial regression ##

mod1 <- glm(Injury ~ Sockeye_total + Length_TSFT_mm, data=sockeye, family="binomial")
mod2 <- glm(Injury ~ Sockeye_total * Length_TSFT_mm, data=sockeye, family="binomial")
mod3 <- glm(Injury ~ Sockeye_total + Ocean_age, sockeye, family = "binomial")
mod4 <- glm(Injury ~ Sockeye_total * Ocean_age, sockeye, family = "binomial")
mod5 <- glm(Injury ~ poly(Sockeye_total, 2), sockeye, family = "binomial")
mod6 <- glm(Injury ~ poly(Sockeye_total, 2) + Ocean_age, sockeye, family = "binomial")
AIC(mod1, mod2, mod3, mod4, mod5, mod6)

# predict with species group size
pred_interval <- seq(min(bin.x),max(bin.x),1)
example <- data.frame(Sockeye_total=pred_interval, Length_TSFT_mm=rep(350, length(pred_interval)))
pred1 <- predict(mod1, newdata=example, re.form=NA, type="response")
plot(pred1, type="l", ylab="Probability of Predator Injury", xlab="Group Size")

pred <- predict(mod1, newdata=example, type="link", se.fit=TRUE)

crit <- qt(0.025,152-2,lower.tail = FALSE)
lower <- pred$fit - crit*pred$se.fit
upper <- pred$fit + crit*pred$se.fit
conv.preds <- 1/(1+exp(-(pred$fit)))
conv.lower <- 1/(1+exp(-(lower)))
conv.upper <- 1/(1+exp(-(upper)))

plot(example[,1],conv.preds,type='l')
lines(example[,1],conv.lower,lty=2)
lines(example[,1],conv.upper,lty=2)

pred <- as.data.frame(pred)
all <- cbind(example[,1], conv.preds, conv.lower, conv.upper)
colnames(all) <- c("x", "y", "ymin", "ymax")
all <- as.data.frame(all)

all$feature="dateset1"
raw$feature="dataset2"

df= rbind(all, raw)

g <- ggplot(df, aes(x, y, linetype=feature)) + geom_line(size=1.2) + geom_ribbon(aes(ymin=ymin, ymax=ymax, colour=feature), linetype=0, alpha=0.1)

g <- g + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        text = element_text(size=20), axis.text.x= element_text(size=15), 
        axis.text.y= element_text(size=15), legend.position = "none", axis.title.y=element_blank(), 
        axis.title.x=element_blank())

sockeye.graph <- g + annotate("text",  x=430, y = 0.016, label = "Sockeye", vjust=1, hjust=1, size=7)

sockeye.graph
