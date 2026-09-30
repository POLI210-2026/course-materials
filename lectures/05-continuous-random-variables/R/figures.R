# Original, reproducible figures. All numerical examples are hypothetical.
# This file supports the slides and print notes. The live QMD is self-contained.
blue <- "#176B87"
rust <- "#A44A3F"
green <- "#567D46"
light_blue <- "#CEE3EB"
ink <- "#172D42"

fig_par <- function() {
  par(mar=c(4.1,4.3,1.3,.8), mgp=c(2.6,.8,0), las=1,
      cex=1.15, col.axis=ink, col.lab=ink, fg=ink)
}

uniform_area <- function() {
  fig_par()
  plot(c(-1,11),c(0,.13),type="n",xlab="Waiting time (minutes)",
       ylab="Density (per minute)")
  rect(2,0,5,.1,col=light_blue,border=NA)
  segments(c(-1,0,10),c(0,.1,0),c(0,10,11),c(0,.1,0),col=blue,lwd=3)
  segments(c(0,10),0,c(0,10),.1,col=blue,lty=3)
  text(3.5,.05,"Area = 0.30",cex=1.05)
  text(7.5,.112,"Height = 0.10",col=blue)
}

triangle_area <- function() {
  fig_par()
  plot(c(-1,11),c(0,.23),type="n",xlab="Waiting time (minutes)",
       ylab="Density (per minute)")
  polygon(c(2,2,5,5),c(0,.04,.10,0),col=light_blue,border=NA)
  segments(0,0,10,.2,col=blue,lwd=3)
  segments(c(-1,10),0,c(0,11),0,col=blue,lwd=3)
  segments(10,0,10,.2,col=blue,lty=3)
  text(3.7,.024,"0.21",cex=1.1)
  text(7,.18,"f(t) = t / 50",col=blue)
}

uniform_cdf <- function() {
  fig_par()
  plot(c(-1,11),c(0,1),type="n",xlab="Threshold t (minutes)",
       ylab="Cumulative probability")
  lines(c(-1,0,10,11),c(0,0,1,1),col=blue,lwd=3)
  segments(c(2,5),0,c(2,5),c(.2,.5),lty=3,col=rust)
  points(c(2,5),c(.2,.5),pch=16,col=rust)
  text(c(2,5),c(.2,.5),c("F(2) = 0.2","F(5) = 0.5"),pos=4,cex=.95)
}

normal_shapes <- function() {
  fig_par()
  curve(dnorm(x,0,1),-7,9,col=blue,lwd=3,ylim=c(0,.43),
        xlab="Value",ylab="Density")
  curve(dnorm(x,0,2),add=TRUE,col=rust,lwd=3,lty=2)
  curve(dnorm(x,3,1),add=TRUE,col=green,lwd=3,lty=3)
  legend("topleft",c("Mean 0, SD 1","Mean 0, SD 2","Mean 3, SD 1"),
         col=c(blue,rust,green),lty=1:3,lwd=3,bty="n",cex=.9)
}

normal_area <- function(lower=0,upper=23,mu=3,sigma=5,
                         xlab="Vote margin (percentage points)") {
  fig_par()
  lim <- mu+c(-4,4)*sigma
  x <- seq(lim[1],lim[2],length.out=801)
  plot(x,dnorm(x,mu,sigma),type="l",lwd=3,col=blue,
       xlab=xlab,ylab="Density (per pp)")
  lo <- max(lower,lim[1]); hi <- min(upper,lim[2])
  if(lo < hi) {
    z <- seq(lo,hi,length.out=401)
    polygon(c(lo,z,hi),c(0,dnorm(z,mu,sigma),0),col=light_blue,border=NA)
    lines(x,dnorm(x,mu,sigma),lwd=3,col=blue)
  }
  abline(v=0,lty=3,col=rust)
}

normal_pdf_cdf <- function() {
  old <- par(no.readonly=TRUE)
  on.exit(par(old))
  par(mfrow=c(1,2))
  fig_par()
  par(mar=c(4.1,4.3,2,.8), cex=1.05)
  x <- seq(-17,23,length.out=801)
  f0 <- pnorm(0,3,5)
  plot(x,dnorm(x,3,5),type="l",lwd=3,col=blue,
       xlab="Margin (pp)",ylab="Density (per pp)",main="Normal PDF")
  z <- seq(min(x),0,length.out=401)
  polygon(c(min(z),z,0),c(0,dnorm(z,3,5),0),col=light_blue,border=NA)
  lines(x,dnorm(x,3,5),lwd=3,col=blue)
  segments(0,0,0,dnorm(0,3,5),lty=2,col=rust)
  text(-9,.072,"Area = 0.274",col=rust,cex=.95)
  plot(x,pnorm(x,3,5),type="l",lwd=3,col=blue,ylim=c(0,1),
       xlab="Threshold x (pp)",ylab="Cumulative probability",main="Normal CDF")
  segments(min(x),f0,0,f0,lty=2,col=rust)
  segments(0,0,0,f0,lty=2,col=rust)
  points(0,f0,pch=16,col=rust,cex=1.2)
  text(1,f0,"F(0) = 0.274",pos=4,col=rust,cex=.95)
}

normal_win_curve <- function() {
  fig_par()
  mu <- seq(-15,15,length.out=601)
  selected <- c(-5,0,5)
  probability <- pnorm(selected/5)
  plot(mu,pnorm(mu/5),type="l",lwd=3,col=blue,ylim=c(0,1),
       xlab="Assumed mean margin (pp)",ylab="Probability of victory")
  segments(selected,0,selected,probability,lty=3,col=rust)
  points(selected,probability,pch=16,col=rust,cex=1.2)
  text(selected[1:2],probability[1:2],c("16%","50%"),pos=c(3,2),col=rust)
  text(selected[3]+.4,probability[3]-.13,"84%",pos=4,col=rust)
  legend("topleft","SD = 5 pp; victory requires V > 0",bty="n",cex=.9)
}

normal_quantile_cdf <- function() {
  fig_par()
  x <- seq(-17,23,length.out=801)
  probability <- c(.1,.9)
  quantile <- qnorm(probability,3,5)
  plot(x,pnorm(x,3,5),type="l",lwd=3,col=blue,ylim=c(0,1),
       xlab="Margin threshold (pp)",ylab="Cumulative probability",yaxt="n")
  axis(2,at=c(.1,.5,.9),las=1)
  segments(min(x),probability,quantile,probability,lty=2,col=rust)
  segments(quantile,0,quantile,probability,lty=2,col=rust)
  points(quantile,probability,pch=16,col=rust,cex=1.2)
  text(quantile,c(.28,.68),sprintf("%.2f pp",quantile),
       pos=c(2,4),col=rust,cex=.95)
}

normal_central <- function() {
  q <- qnorm(c(.1,.9),3,5)
  normal_area(q[1],q[2])
  abline(v=q,lty=2,col=green)
  text(3,.038,"Middle 80%",cex=1.1)
}

binomial_overlay <- function(tie=FALSE) {
  fig_par()
  n <- 200; p <- .5; k <- 75:125
  pk <- dbinom(k,n,p); s <- sqrt(n*p*(1-p))
  plot(c(75,125),c(0,.062),type="n",xlab="Votes for candidate A",
       ylab="Mass / density per vote")
  rect(k-.5,0,k+.5,pk,col="#E5EDF1",border="#A6BBC7")
  curve(dnorm(x,n*p,s),add=TRUE,col=blue,lwd=3)
  if(tie) {
    z <- seq(99.5,100.5,length.out=101)
    polygon(c(99.5,z,100.5),c(0,dnorm(z,n*p,s),0),
            col=adjustcolor(rust,.6),border=NA)
    segments(c(99.5,100.5),0,c(99.5,100.5),dnorm(c(99.5,100.5),100,s),
             col=rust,lty=2)
    legend("topleft",c("Binomial mass in width-1 bars","Normal density",
                       "Tie bin: 99.5 to 100.5"),
           fill=c("#E5EDF1",NA,adjustcolor(rust,.6)),
           border=c("#A6BBC7",NA,NA),lty=c(NA,1,NA),
           col=c(NA,blue,NA),bty="n",cex=.85)
  } else {
    legend("topleft",c("Binomial mass in width-1 bars","Normal density"),
           fill=c("#E5EDF1",NA),border=c("#A6BBC7",NA),
           lty=c(NA,1),col=c(NA,blue),bty="n",cex=.9)
  }
}

payoff_cdfs <- function() {
  fig_par()
  # Toy population: costs 5, 15, 15, 45; project budget 80.
  # Offer 15: earnings 0 or 65; offer 45: earnings always 35.
  plot(c(-5,75),c(0,1),type="n",xlab="Earnings threshold (points)",
       ylab="Cumulative probability")
  segments(c(-5,0,65),c(0,.25,1),c(0,65,75),c(0,.25,1),col=blue,lwd=3)
  segments(c(-5,35),c(0,1),c(35,75),c(0,1),col=rust,lwd=3,lty=2)
  segments(c(0,65),c(0,.25),c(0,65),c(.25,1),lty=3,col=blue)
  segments(35,0,35,1,lty=3,col=rust)
  points(c(0,65),c(0,.25),pch=21,bg="white",col=blue)
  points(c(0,65),c(.25,1),pch=16,col=blue)
  points(35,0,pch=21,bg="white",col=rust)
  points(35,1,pch=16,col=rust)
  legend("topleft",c("Offer 15: mean 48.75","Offer 45: mean 35"),
         col=c(blue,rust),lty=c(1,2),lwd=3,bty="n",cex=.95)
}
