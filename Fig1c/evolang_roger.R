library(rms)
library(ggplot2)

plot.spline.contribution <- function(m,x.raw,y.raw,spline.basis,idx,x.axis,xlab=NULL,as.spline=T,x.discrete=T,...) {
        spline.params <- coef(m)[idx]
        spline.Sigma <- vcov(m)[idx,idx]
        if(as.spline) {
                tmp <- rcs(x.axis,parms=attr(spline.basis,"parms"))
                x.spline.basis <- cbind(rep(1,dim(tmp)[1]),matrix(tmp,dim(tmp)[1],dim(tmp)[2]))
        }
        else {
                x.spline.basis=cbind(rep(1,length(spline.basis)),spline.basis)
                print(x.spline.basis)
                print(spline.params)
        }
        mle.contribution <- x.spline.basis %*% spline.params
        ci.size <- qnorm(0.975)*sapply(1:length(mle.contribution),function(i) sqrt(t(x.spline.basis[i,]) %*% as.matrix(spline.Sigma) %*% x.spline.basis[i,]))
        print(cbind(mle.contribution,ci.size))
        nf <- layout(matrix(c(1,1,2,2),2,2,byrow=TRUE),heights=c(3,1))
        par(mar=c(-0.1,4,4,2)+0.1)
        plot(mle.contribution ~ x.axis,type="l",xlab="",xaxt="n",...)
        points(x.raw,y.raw,pch=".")
        lines(mle.contribution + ci.size ~ x.axis, lty=2)
        lines(mle.contribution - ci.size ~ x.axis, lty=2)
        par(mar=c(4,4,0,2)+0.1)
        if(x.discrete) {
                hist.vals <- table(x.raw)
                hist.props <- sapply(x.axis,function(x) {
                        tmp <- as.character(x)
                        return(ifelse(tmp %in% names(hist.vals), hist.vals[tmp],0))
                })/sum(hist.vals)
                plot(x.axis, hist.props,type="h",lwd=4,ylab="Freq",xlab=xlab,ylim=c(0,1.1*max(hist.props)))
        }
        else {
                plot(density(x.raw),ylab="Freq",xlab=xlab,main="")      
        }
}

log.m <- log(corpusdata$m)
spline.basis <- rcs(log.m)
spline.model <- lm(extremity ~ spline.basis,corpusdata)
x <- seq(min(log.m),max(log.m),by=0.05)

quartz(file="extremity-roger.pdf",height=7,width=7,type="pdf")
plot.spline.contribution(spline.model,log.m,corpusdata$extremity,1:5,x.axis=x,x.discrete=F,ylim=c(0,0.5))
dev.off()

# load("roger.RData")

# quartz(file="extremity.pdf",width=7,height=7,type="pdf")
# #quartz(width=7,height=7)
# q <- qplot(log(m),extremity,data=corpusdata,ylim=c(0,0.5),xlab="Log overall freq",ylab="Extremity")
# q + geom_density(aes(x=log(m),y=..density..),size=3,linetype="dashed") + stat_smooth(size=5) + theme_bw() + theme(text=element_text(size=36))
# dev.off()

#corpusdata$overall.freq is raw counts from Google books
#corpusdata$m is scaled based on 300 million total lifetime words