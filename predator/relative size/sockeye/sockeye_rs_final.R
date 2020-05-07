raw <- read.csv("sockeye_rs_raw.csv", header=TRUE)
model <- read.csv("sockeye_rs_model.csv", header=TRUE)

model$feature="dateset1"
raw$feature="dataset2"

df= rbind(model, raw)

g <- ggplot(df, aes(x, y, linetype=feature)) + geom_line(size=1.2) + geom_ribbon(aes(ymin=ymin, ymax=ymax, colour=feature), linetype=0, alpha=0.1)

g <- g + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                            text = element_text(size=20), axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)
                            ,legend.position = "none", axis.title.y=element_blank(), axis.title.x=element_blank())

sockeye_graph <- g + annotate("text",  x=1.4, y = 0.075, label = "Sockeye", vjust=1, hjust=1, size=7)

sockeye_graph