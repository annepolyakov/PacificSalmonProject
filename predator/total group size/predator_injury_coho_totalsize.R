coho <- final.fish[ which(final.fish$Species=='Coho'), ]
myvars <- c("ID_code", "Length_TSFT_mm", "Coho_total", "total", "Ocean_age","Injury", "Sex","Set_time")
coho <- coho[myvars]

# make sure data makes sense
coho <- coho[ which(coho$Coho_total > 0), ]

coho.datap <- data.frame(coho$Coho_total, coho$total, coho$Injury)
names(coho.datap) <- c("Species group size", "Total group size", "Predator Injury")

# create unique id and add it to data (enter total number of data entries for id number)
id <- numeric(2010)
id[1] <- 1
coho <- coho[order(coho$ID_code), ]
for (i in 2:2010){
  if (coho$ID_code[i] == coho$ID_code[i-1]){
    id[i] <- id[i-1]
  }
  else{
    id[i] <- id[i-1]+1
  }
}

coho <- data.frame(coho,id)

# plot raw data
plot(coho$Coho_total, coho$Injury)

#normal bins
bin.y <- vector(mode="numeric", length=4)
bin.x <- vector(mode="numeric", length=4)
error <- vector(mode="numeric", length=4)

for (i in 1:4){
  data <- coho[ which(coho$total >= (i-1)*62.5 & coho$total < i*62.5), ]
  bin.y[i] <- (sum(data$Injury)+0.5)/(length(data$Injury)+1)
  bin.x[i] <- mean(data$total)
  n <- length(data$Injury)
  error[i] <- sqrt(bin.y[i]*(1-bin.y[i])/(length(data$Injury+1)))
  #error[i] <- qt(0.975,df=n-1)*sd(data$Injury)/sqrt(n)
}

upper <- bin.y+error
lower <- bin.y-error

plot(bin.x, bin.y, type="l")
lines(bin.x, lower)
lines(bin.x, upper)

raw <- cbind(bin.x, bin.y, lower, upper)
colnames(raw) <- c("x", "y", "ymin", "ymax")
raw <- as.data.frame(raw)

# run binomial regression
mod1 <- glm(Injury ~ total, coho, family = "binomial")
mod2 <- glm(Injury ~ total + Ocean_age, coho, family = "binomial")

mod3 <- glm(Injury ~ total + Length_TSFT_mm, coho, family = "binomial")
mod3a <- glm(Injury ~ total * Length_TSFT_mm, coho, family = "binomial")

mod5 <- glm(Injury ~ total, coho, family = "binomial")
mod6 <- glm(Injury ~ total + Length_TSFT_mm + Sex, coho, family = "binomial")
mod7 <- glm(Injury ~ total + Ocean_age + Sex + Set_time, coho, family = "binomial")
mod8 <- glm(Injury ~ total + Ocean_age + Set_time, coho, family = "binomial")
AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)

# predict with species group size
pred_interval <- seq(min(bin.x),max(bin.x),1)
example <- data.frame(total=pred_interval, Length_TSFT_mm=rep(500, length(pred_interval)))
pred1 <- predict(mod3, newdata=example, re.form=NA, type="response")
plot(pred1, type="l", ylab="Prob(predator injury)", xlab="Group Size", xlim=c(0,200))

pred <- predict(mod3, newdata=example, type="link", se.fit=TRUE)

crit <- qt(0.025,152-2,lower.tail = FALSE)
lower <- pred$fit - pred$se.fit
upper <- pred$fit + pred$se.fit
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
        text = element_text(size=20), axis.text.x= element_text(size=15), 
        axis.text.y= element_text(size=15), legend.position = "none", 
        axis.title.y=element_blank(), axis.title.x=element_blank())

coho.graph <- g + annotate("text",  x=200, y = 0.032, label = "Coho", vjust=1, hjust=1, size=7)

coho.graph

