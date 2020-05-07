# predict with fish relative size
# predict with polynomial logistic regression
mod1 <- glm(Injury ~ poly(relative.size, 2), sockeye, family="binomial")
mod2 <- glm(Injury ~ poly(relative.size, 2) + Length_TSFT_mm, sockeye, family="binomial")
mod3 <- glm(Injury ~ poly(relative.size, 2) + Ocean_age, sockeye, family="binomial")

# predict with normal logistic regression
mod4 <- glm(Injury ~ relative.size, sockeye, family="binomial")
mod5 <- glm(Injury ~ relative.size + Length_TSFT_mm, sockeye, family="binomial")
mod6 <- glm(Injury ~ relative.size + Ocean_age, sockeye, family="binomial")

# predict with piece-wise linear regression
require(segmented)
segmented.mod1 <- segmented(mod4, seg.Z = ~relative.size, data=sockeye, psi=1)
segmented.mod2 <- segmented(mod5, seg.Z = ~relative.size, data=sockeye, psi=1)
segmented.mod3 <- segmented(mod6, seg.Z = ~relative.size, data=sockeye, psi=1)
summary(segmented.mod1)
slope(segmented.mod)

AIC(mod1, mod2, mod3, mod4, mod5, mod6, segmented.mod1, segmented.mod2, segmented.mod3)

pred_interval <- seq(0.6,1.45,0.05)
# Ocean_age is 3 for mod3
example <- data.frame(relative.size=pred_interval, Ocean_age=rep(0.7, length(pred_interval)))
pred <- predict(segmented.mod1, newdata=example, re.form=NA, type="link", se.fit=TRUE)
plot(example$relative.size, pred$fit, type="l", ylab="Probability of Predator Injury", xlab="Relative Fish Size")

crit <- qt(0.025,152-2,lower.tail = FALSE)
lower <- pred$fit - pred$se.fit
upper <- pred$fit + pred$se.fit
conv.preds <- 1/(1+exp(-(pred$fit)))
conv.lower <- 1/(1+exp(-(lower)))
conv.upper <- 1/(1+exp(-(upper)))

pred <- as.data.frame(pred)
all <- cbind(example[,1], conv.preds, conv.lower, conv.upper)
colnames(all) <- c("x", "y", "ymin", "ymax")
all <- as.data.frame(all)

write.csv(all, "sockeye_rs_model.csv")