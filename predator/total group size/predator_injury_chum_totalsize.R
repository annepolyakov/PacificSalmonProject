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
  
  # plot raw data
  # normal bins
  bin.y <- vector(mode="numeric", length=5)
  error <- vector(mode="numeric", length=5)
  bin.x <- vector(mode="numeric", length=5)
  
  for (i in 1:5){
    data <- chum[ which(chum$total >= (i-1)*200 & chum$total < i*200), ]
    bin.y[i] <- (sum(data$Injury)+0.5)/(length(data$Injury)+1)
    bin.x[i] <- mean(data$total)
    n <- length(data$Injury)
    error[i] <- sqrt(bin.y[i]*(1-bin.y[i])/(length(data$Injury+1)))
    #error[i] <- qt(0.975,df=n-1)*sd(data$Injury)/sqrt(n)
  }
  
  lower <- bin.y-error
  upper <- bin.y+error
  
  plot(bin.x, bin.y, type="l")
  
  raw <- cbind(bin.x, bin.y, bin.y-error, bin.y+error)
  colnames(raw) <- c("x", "y", "ymin", "ymax")
  raw <- as.data.frame(raw)
  
  # run binomial regression
  mod1 <- glm(Injury ~ total + Ocean_age, chum, family = "binomial")
  mod4 <- glm(Injury ~ total, chum, family = "binomial")
  
  mod5 <- glm(Injury ~ total + Length_TSFT_mm, chum, family = "binomial")
  mod5a <- glm(Injury ~ total * Length_TSFT_mm, chum, family = "binomial")
  summary(mod5a)
  
  # predict with species group size
  pred_interval <- seq(90,900,1)
  example <- data.frame(total=pred_interval, Length_TSFT_mm=rep(450, length(pred_interval)))
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
       legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank()) + 
       scale_y_continuous(breaks=seq(0,0.02,0.01))
  
  chum.graph <- g + annotate("text",  x=850, y = 0.02, label = "Chum", vjust=1, hjust=1, size=7)
  
  chum.graph