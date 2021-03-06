raw <- read.csv("pink_sf_raw.csv", header=TRUE)
model <- read.csv("pink_sf_model.csv", header=TRUE)

all$feature="dateset1"
raw$feature="dataset2"

df= rbind(model, raw)

g <- ggplot(df, aes(x, y, linetype=feature)) + geom_line(size=1.2) + geom_ribbon(aes(ymin=ymin, ymax=ymax, colour=feature), linetype=0, alpha=0.1)

g <- g + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        text = element_text(size=20), axis.text.x= element_text(size=15), 
        axis.text.y= element_text(size=15), legend.position = "none", axis.title.y=element_blank(), 
        axis.title.x=element_blank()) + scale_y_continuous(breaks = c(0.8, 0.9, 1.0))

pink_sf_binomial <- g + annotate("text",  x=825, y = 1, label = "Pink", vjust=1, hjust=1, size=7)

pink_sf_binomial