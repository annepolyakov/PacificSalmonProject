# binomial regression ##

mod1 <- glm(fullness ~ Pink_total, pink, family = "binomial")
mod2 <- glm(fullness ~ Pink_total + Length_TSFT_mm, pink, family = "binomial")
mod3 <- glm(fullness ~ Pink_total * Length_TSFT_mm, pink, family = "binomial")
mod4 <- glm(fullness ~ poly(Pink_total, 2), pink, family = "binomial")
mod5 <- glm(fullness ~ poly(Pink_total, 2) + Length_TSFT_mm, pink, family = "binomial")

exp.mod1 <- glm(fullness ~ Pink_total, pink, family=binomial(link=log), start=c(0,-0.0009))
exp.mod2 <- glm(fullness ~ Pink_total + Length_TSFT_mm, pink, family=binomial(link=log), start=c(0,0.001,-0.009))

segmented.mod <- segmented(mod2, seg.Z = ~Pink_total, data=pink, psi=500)
summary(segmented.mod)
slope(segmented.mod)
AIC(mod1, mod2, mod3, mod4, mod5, segmented.mod, exp.mod1, exp.mod2)

# predict with species group size
pred_interval <- seq(min(temp1),max(temp1),1)
example <- data.frame(Pink_total=pred_interval, Length_TSFT_mm=rep(480,length(pred_interval)))
pred <- predict(mod2, newdata=example, re.form=NA, type="response")
plot(pred, type="l", ylab="Probability of Stomach Fullness", xlab="Group Size")

pred <- predict(mod2, newdata=example, type="link", se.fit=TRUE)

crit <- qt(0.025,152-2,lower.tail = FALSE)
lower <- pred$fit - pred$se.fit
upper <- pred$fit + pred$se.fit
conv.preds <- 1/(1+exp(-(pred$fit)))
conv.lower <- 1/(1+exp(-(lower)))
conv.upper <- 1/(1+exp(-(upper)))

fullness.pink <- conv.preds
fullness.pink.lower <- conv.lower
fullness.pink.upper <- conv.upper

plot(example[,1],conv.preds,type='l')
lines(example[,1],conv.lower,lty=2)
lines(example[,1],conv.upper,lty=2)

pred <- as.data.frame(pred)
all <- cbind(example[,1], conv.preds, conv.lower, conv.upper)
colnames(all) <- c("x", "y", "ymin", "ymax")
all <- as.data.frame(all)

write.csv(all, "pink_sf_model.csv")
