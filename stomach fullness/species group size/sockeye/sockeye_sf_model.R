# binomial regression ##
require(segmented)
mod1 <- glm(fullness ~ Sockeye_total, sockeye, family = "binomial")
mod2 <- glm(fullness ~ Sockeye_total + Length_TSFT_mm, sockeye, family = "binomial")
mod3 <- glm(fullness ~ Sockeye_total * Length_TSFT_mm, sockeye, family = "binomial")
mod4 <- glm(fullness ~ poly(Sockeye_total,2), sockeye, family = "binomial")
mod5 <- glm(fullness ~ poly(Sockeye_total,2) + Length_TSFT_mm, sockeye, family = "binomial")

exp.mod1 <- glm(fullness ~ Sockeye_total, sockeye, family=binomial(link=log))
exp.mod2 <- glm(fullness ~ Sockeye_total + Length_TSFT_mm, sockeye, family=binomial(link=log), start=c(0,-0.0006,-0.004))

segmented.mod <- segmented(mod2, seg.Z = ~Sockeye_total, data=sockeye, psi=200)
summary(segmented.mod)
slope(segmented.mod)
AIC(mod1, mod2, mod3, mod4, mod5, segmented.mod, exp.mod1, exp.mod2)

# predict with species group size
pred_interval <- seq(min(bin.x),max(bin.x),1)
example <- data.frame(Sockeye_total=pred_interval, Length_TSFT_mm=rep(500,length(pred_interval)))
pred <- predict(mod3, newdata=example, re.form=NA, type="response")
plot(pred, type="l", ylab="Probability of Stomach Fullness", xlab="Group Size")
plot(pred$fit)

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

write.csv(all, file ="sockeye_sf_model.csv", row.names=TRUE)