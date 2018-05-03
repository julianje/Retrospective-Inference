library(tidyverse)
library(gtable)
library(grid)
library(gridExtra)

PlotDiagnostics <- function(Network, Priors){
  p1 <- ggplot(Network,aes(xmin=x,ymin=y,xmax=dx,ymax=dy,fill=frame))+geom_rect(alpha=1/16)+
    theme_classic()+ggtitle("Mask_RCNN trace")
  p2 <- Priors %>% rename(height=dy,width=dx) %>%
    gather(variable,value,x,y,height,width) %>%
    ggplot(aes(value))+geom_density()+theme_classic()+
    facet_wrap(~variable)+
    ggtitle("Sampling distribution")
  p3 <- Priors %>% ggplot(aes(xmin=x,ymin=y,xmax=dx,ymax=dy))+
    geom_rect(alpha=0,color="#000000",alpha=1/64)+theme_classic()+
    geom_rect(data=Network,aes(xmin=x,ymin=y,xmax=dx,ymax=dy,fill=frame),alpha=1/16)+
    ggtitle("Sampled bounding boxes")
  p4 <- Priors %>% ggplot(aes(x,y))+geom_point(alpha=1/8)+
    theme_classic()+
    geom_point(data=Network,aes(x,y,color=frame))+
    ggtitle("Box anchors")
  grid.arrange(p1,p2,p3,p4,ncol=2)  
}

GaussianPrior <- function(Network, sampleno){
  Samples <- data.frame(
    x = rnorm(sampleno, mean(Network$x), sd(Network$x)),
    y = rnorm(sampleno, mean(Network$y), sd(Network$y)),
    distx = rnorm(sampleno, mean(Network$dx-Network$x), sd(Network$dx-Network$x)),
    disty = rnorm(sampleno, mean(Network$dy-Network$y), sd(Network$dy-Network$y))
  ) %>% mutate(dx=x+distx,dy=x+disty) %>% tbl_df
}

NetOutput_DivergenceA <- read_csv("../TestImages/Balls_2_DivergenceA/Balls_2_DivergenceA_DetectedObjects.csv")
NetOutput_ContactB <- read_csv("../TestImages/Balls_2_ContactB/Balls_2_ContactB_DetectedObjects.csv")

# Option 1: Gaussian distribution!
PlotDiagnostics(NetOutput_DivergenceA,GaussianPrior(NetOutput_DivergenceA,1000))
PlotDiagnostics(NetOutput_ContactB,GaussianPrior(NetOutput_ContactB,1000))


