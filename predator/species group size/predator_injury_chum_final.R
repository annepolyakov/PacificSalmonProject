chum <- final.fish[ which(final.fish$Species=='Chum'), ]
myvars <- c("ID_code", "Length_TSFT_mm", "Chum_total", "total", "Injury","Sex", "Ocean_age", "Set_time")
chum <- chum[myvars]

# make sure data makes sense
chum <- chum[ which(chum$Chum_total > 0), ]
dim(chum)

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

# plot raw data

plot(chum$Chum_total, chum$Injury)

# normal bins
bin.y <- vector(mode="numeric", length=5)
error <- vector(mode="numeric", length=5)
bin.x <- vector(mode="numeric", length=5)

for (i in 1:5){
  data <- chum[ which(chum$Chum_total >= (i-1)*162 & chum$Chum_total < i*162), ]
  bin.y[i] <- (sum(data$Injury)+0.5)/(length(data$Injury)+1)
  #bin.y[i] <- (sum(data$Injury))/(length(data$Injury))
  bin.x[i] <- mean(data$Chum_total)
  n <- length(data$Injury)
  error[i] <- sqrt(bin.y[i]*(1-bin.y[i])/(length(data$Injury+1)))
  #error[i] <- qt(0.975,df=n-1)*sd(data$Injury)/sqrt(n)
}

plot(bin.x, bin.y, type="l")
lines(bin.x, lower)
lines(bin.x, upper)

lower <- bin.y-error
upper <- bin.y+error

raw <- cbind(bin.x, bin.y, bin.y-error, bin.y+error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

# run binomial regression
mod1 <- glm(Injury ~ Chum_total + Ocean_age, chum, family = "binomial")
mod4 <- glm(Injury ~ Chum_total, chum, family = "binomial")

mod5 <- glm(Injury ~ Chum_total + Length_TSFT_mm, chum, family = "binomial")
mod5a <- glm(Injury ~ Chum_total * Length_TSFT_mm, chum, family = "binomial")
summary(mod5a)

# predict with species group size
pred_interval <- seq(min(bin.x),max(bin.x),1)
example <- data.frame(Chum_total=pred_interval, Length_TSFT_mm=rep(450, length(pred_interval)))
pred1 <- predict(mod5a, newdata=example, re.form=NA, type="response")
plot(pred1, type="l", ylab="Probability of Predator Injury", xlab="Group Size")

pred <- predict(mod5a, newdata=example, type="link", se.fit=TRUE)

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

g <- g + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
     text = element_text(size=20), axis.text.x= element_text(size=15), axis.text.y= element_text(size=15),
     legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank())

chum.graph <- g + annotate("text",  x=700, y = 0.0165, label = "Chum", vjust=1, hjust=1, size=7)

chum.graph

