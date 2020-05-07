# binomial regression ##

mod1 <- glm(fullness ~ Coho_total, coho, family = "binomial")
mod2 <- glm(fullness ~ Coho_total + Length_TSFT_mm, coho, family = "binomial")
mod3 <- glm(fullness ~ Coho_total * Length_TSFT_mm, coho, family = "binomial")
mod4 <- glm(fullness ~ poly(Coho_total, 2), coho, family = "binomial")
mod5 <- glm(fullness ~ poly(Coho_total, 2) + Length_TSFT_mm, coho, family = "binomial")

exp.mod1 <- glm(fullness ~ Coho_total, coho, family=binomial(link=log), start=c(0,-0.01))
exp.mod2 <- glm(fullness ~ Coho_total + Length_TSFT_mm, coho, family=binomial(link=log), start=c(0,0.002,-0.003))

segmented.mod <- segmented(mod2, seg.Z = ~Coho_total, data=coho, psi=50)
summary(segmented.mod)
slope(segmented.mod)
AIC(mod1, mod2, mod3, mod4, mod5, segmented.mod, exp.mod1, exp.mod2)

# predict with species group size
pred_interval <- seq(min(temp1),max(temp1),1)
example <- data.frame(Coho_total=pred_interval, Length_TSFT_mm = rep(450,length(pred_interval)))
pred <- predict(mod2, newdata=example, re.form=NA, type="response")
plot(pred, type="l", ylab="Probability of Stomach Fullness", xlab="Group Size")

pred <- predict(mod2, newdata=example, type="link", se.fit=TRUE)

crit <- qt(0.025,152-2,lower.tail = FALSE)
lower <- pred$fit - 1.5*pred$se.fit
upper <- pred$fit + 1.5*pred$se.fit
conv.preds <- 1/(1+exp(-(pred$fit)))
conv.lower <- 1/(1+exp(-(lower)))
conv.upper <- 1/(1+exp(-(upper)))

fullness.coho <- conv.preds
fullness.coho.lower <- conv.lower
fullness.coho.upper <- conv.upper

plot(example[,1],conv.preds,type='l')
lines(example[,1],conv.lower,lty=2)
lines(example[,1],conv.upper,lty=2)

pred <- as.data.frame(pred)
all <- cbind(example[,1], conv.preds, conv.lower, conv.upper)
colnames(all) <- c("x", "y", "ymin", "ymax")
all <- as.data.frame(all)

write.csv(all, "coho_sf_model.csv")
