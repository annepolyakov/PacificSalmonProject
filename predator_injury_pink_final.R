pink <- final.fish[ which(final.fish$Species=='Pink'), ]
myvars <- c("ID_code", "Length_TSFT_mm", "Pink_total", "total", "Sex", "Injury", "Ocean_age", "Set_time")
pink <- pink[myvars]

# make sure data makes sense
#pink$Pink_total <- as.numeric(levels(pink$Pink_total))[pink$Pink_total]
pink <- pink[ which(pink$Pink_total > 0), ]

#pink <- subset(pink, Sex %in% c("Female", "Male"))

#pink.datap <- data.frame(pink$Pink_total, pink$total, pink$Injury)
#names(pink.datap) <- c("Species group size", "Total group size", "Predator Injury")
#write.csv(x=pink.datap, file = "pink_predator.csv")

# create unique id and add it to data (enter total number of data entries for id number)
id <- numeric(11809)
id[1] <- 1
pink <- pink[order(pink$ID_code), ]
for (i in 2:11809){
  if (pink$ID_code[i] == pink$ID_code[i-1]){
    id[i] <- id[i-1]
  }
  else{
    id[i] <- id[i-1]+1
  }
}

pink <- data.frame(pink,id)

# plot raw data

plot(pink$Pink_total, pink$Injury, xlim=c(0,1000))

# normal bins
bin.y <- vector(mode="numeric", length=6)
bin.x <- vector(mode="numeric", length=6)
error <- vector(mode="numeric", length=6)

# 33 and 6
# for longer fit, do 60 and 6

for (i in 1:6){
  data <- pink[ which(pink$Pink_total >= (i-1)*33 & pink$Pink_total < i*33), ]
  bin.y[i] <- (sum(data$Injury)+0.5)/(length(data$Injury)+1)
  #bin.y[i] <- (sum(data$Injury))/(length(data$Injury))
  bin.x[i] <- mean(data$Pink_total)
  n <- length(data$Injury)
  error[i] <- sqrt(bin.y[i]*(1-bin.y[i])/(length(data$Injury+1)))
  #error[i] <- qt(0.975,df=n-1)*sd(data$Injury)/sqrt(n)
}

plot(bin.x, bin.y, type="l")

error <- error*2

error <- error[!is.na(bin.y)]
bin.x <- bin.x[!is.na(bin.y)]
bin.y <- bin.y[!is.na(bin.y)]

raw <- cbind(bin.x, bin.y, bin.y-error, bin.y+error)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

# run binomial regression
require(lme4)
mod1 <- glm(Injury ~ Pink_total + Ocean_age, pink, family = "binomial")
mod4 <- glm(Injury ~ Pink_total, pink, family = "binomial")

mod5 <- glm(Injury ~ Pink_total + Length_TSFT_mm, pink, family = "binomial")
mod5a <- glm(Injury ~ Pink_total * Length_TSFT_mm, pink, family = "binomial")
AIC(mod5, mod5a)

# predict with species group size
pred_interval <- seq(min(bin.x),max(bin.x),1)
example <- data.frame(Pink_total=pred_interval, Length_TSFT_mm=rep(400, length(pred_interval)))
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

g <- g + theme_bw() + theme(panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), text = element_text(size=20), 
      axis.text.x= element_text(size=15), axis.text.y= element_text(size=15), 
      legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank())

pink.graph <- g + annotate("text",  x=175, y = 0.034, label = "Pink", vjust=1, hjust=1, size=7)

pink.graph
