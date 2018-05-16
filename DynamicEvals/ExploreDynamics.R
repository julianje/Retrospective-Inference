library(tidyverse)
library(gtable)
library(grid)
library(gridExtra)
library(plgp)
library(MASS)

gaussprocess <- function(from = 0, to = 1, K,
                         start = NULL, m = 1000) {
  t <- seq(from = from, to = to, length.out = m)
  Sigma <- sapply(t, function(s1) {
    sapply(t, function(s2) {
      K(s1, s2)
    })
  })
  path <- mvrnorm(mu = rep(0, times = m), Sigma = Sigma)
  if (!is.null(start)) { path <- path - path[1] + start }
  return(data.frame("t" = t, "xt" = path))
}

SampleGPs <- function(K, Kb = NULL, samples = 5,
                      m = 1000, start = NULL,
                      from = 0, to = 1, plotit = TRUE){
  if (is.null(Kb)){
    Kb = K
  }
  samples <- do.call(rbind,lapply(1:samples,function(x){
    full_join(gaussprocess(from, to, K, start, m),rename(gaussprocess(from, to, Kb, start, m),yt=xt),by="t") %>% mutate(sample=x)
    }))
  if (plotit){
    samples %>% ggplot(aes(xt,yt,color=factor(sample),order=t,group=factor(sample)))+geom_path()+theme_classic()+
      scale_x_continuous("")+scale_y_continuous("")+ theme(legend.position="none")
  }
  else{return(samples)}
}

ExploreGP <- function(KernelGenerator, params, m=100, samples = 50){
  print("no this is very broken")
  return(0)
  AllSamples <- lapply(1:dim(params)[1],function(i){
    temp <- crossing(SampleGPs(do.call(KernelGenerator,unname(as.list(as.data.frame(t(params[i,])))[1])),m=m,samples=samples,plotit=FALSE),data.frame(params[i,]))
    return(temp)})
  AllSamples <- do.call(rbind,AllSamples)
  return(AllSamples)
}

# Brownian motion
WienerKernel <- function() {return(function(s, t){min(s, t)})}
# Linear kernel
# c is the offset
LinearKernel <- function(variance, cvariance, c) {return(function(s, t){variance*(s-c)*(t-c)+cvariance})}
# Gaussian kernel
GaussKernel <- function(x) {return(function(s,t){return(exp(-x*(s-t)^2))})}
# These are common in SVMs
# Squared-exponential kernel
SEKernel <- function(scale,variance) {return(function(s,t){return(variance*exp(-((s-t)^2)/(2*scale^2)))})}
# Rational quadratic kernel
# Variation is alpha, which is relative weighting of large and small scale variation
# as alpha -> infinity RQKernel converges to SEKernel.
RQKernel <- function(scale, variance, variation) {return(function(s,t){
  variance*(1 + ((s-t)^2/(2*variation*scale^2)))^(-variation)
})}
# Periodic Kernel. Takes lengthscale and period.
PeriodicKernel <- function(variance, l, p) {return(function(s,t){
  val <- variance*exp((-2/l^2)*sin(pi*(abs(s-t)/p))^2)
  return(val)
  })}

SampleGPs(LinearKernel(5,2,0.3),m=100,samples=50)
SampleGPs(WienerKernel(),m=100)
SampleGPs(GaussKernel(8),m=100,samples=30)
SampleGPs(SEKernel(3),m=100)
SampleGPs(PeriodicKernel(5,3,2),m=100,samples=50)

QuickPlotHack <- function(plots){
  do.call(grid.arrange,plots)
}

QuickPlotHack(list(SampleGPs(LinearKernel(5,2,0.3),m=100,samples=50),
              SampleGPs(LinearKernel(5,2,0.3),m=100,samples=50),
              SampleGPs(LinearKernel(5,2,0.3),m=100,samples=50),
              SampleGPs(LinearKernel(5,2,0.3),m=100,samples=50)))

QuickPlotHack(list(SampleGPs(SEKernel(1,1),m=100,samples=50),
                   SampleGPs(SEKernel(1,50),m=100,samples=50),
                   SampleGPs(SEKernel(0.5,1),m=100,samples=50),
                   SampleGPs(SEKernel(1,0.5),m=100,samples=50)))

QuickPlotHack(list(SampleGPs(RQKernel(1,1,1),m=100,samples=50),
                   SampleGPs(RQKernel(1,50,1),m=100,samples=50),
                   SampleGPs(RQKernel(0.5,1,1),m=100,samples=50),
                   SampleGPs(RQKernel(1,0.5,1),m=100,samples=50)))
