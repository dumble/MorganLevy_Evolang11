library(ggplot2)

load("roger.RData")

quartz(file="extremity.pdf",width=7,height=7,type="pdf")
#quartz(width=7,height=7)
q <- qplot(log(m),extremity,data=corpusdata,ylim=c(0,0.5),xlab="Log overall freq",ylab="Extremity")
q + geom_density(aes(x=log(m),y=..density..),size=3,linetype="dashed") + stat_smooth(size=5) + theme_bw() + theme(text=element_text(size=36))
dev.off()

#corpusdata$overall.freq is raw counts from Google books
#corpusdata$m is scaled based on 300 million total lifetime words