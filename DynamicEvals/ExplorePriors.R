library(tidyverse)
library(gtable)
library(grid)
library(gridExtra)

PlotDiagnostics <- function(Network, Priors, StimName=""){
  p1 <- ggplot(Network,aes(xmin=x,ymin=y,xmax=x2,ymax=y2,fill=frame))+
    geom_rect(alpha=1/16,color="#000000")+
    theme_classic()+ggtitle("Mask_RCNN trace")+coord_fixed()
  TA <- Priors %>%
    gather(variable,value,x,y,x2,y2) %>%
    dplyr::select(-distx,-disty) %>% mutate(source="Prior")
  TB <- gather(Network,variable,value,x,y,x2,y2) %>%
    dplyr::select(-frame,-id,-score) %>% mutate(source="Mark_RCNN")
  Full <- rbind(TA,TB)
  p2 <- Full %>%
    ggplot(aes(value,fill=source))+geom_density(alpha=3/4)+theme_classic()+
    facet_wrap(~variable,scales="free")+
    ggtitle("Sampling distribution")+scale_fill_manual(values=c("#424242","#0099ff"))
  p3 <- Priors %>% ggplot(aes(xmin=x,ymin=y,xmax=x2,ymax=y2))+
    geom_rect(alpha=1/8,fill="#000000",colour="black",linetype="dashed")+theme_classic()+
    geom_rect(data=Network,aes(xmin=x,ymin=y,xmax=x2,ymax=y2,fill=frame),alpha=1/16)+
    ggtitle("Sampled bounding boxes")+coord_fixed()
  p4 <- Priors %>% ggplot(aes(x,y))+geom_point(alpha=1/8)+
    theme_classic()+
    geom_point(data=Network,aes(x,y,color=frame))+
    ggtitle("Box anchors ((x,y) samples)")+coord_fixed()
  Title = paste(StimName,"using",nrow(Priors),"samples from the Prior")
  grid.arrange(p1,p2,p3,p4,ncol=2,
               top = textGrob(Title,gp=gpar(fontsize=20,font=1)))  
}

GaussianPrior <- function(Network, sampleno){
  Samples <- data.frame(
    x = rnorm(sampleno, mean(Network$x), sd(Network$x)),
    y = rnorm(sampleno, mean(Network$y), sd(Network$y)),
    distx = rnorm(sampleno, mean(Network$x2-Network$x), sd(Network$x2-Network$x)),
    disty = rnorm(sampleno, mean(Network$y2-Network$y), sd(Network$y2-Network$y))
  ) %>% mutate(x2=x+max(0,distx),y2=y+max(0,disty))
}
LogNormalPrior <- function(Network, sampleno){
  Samples <- data.frame(
    x = rnorm(sampleno, mean(Network$x), sd(Network$x)),
    y = rnorm(sampleno, mean(Network$y), sd(Network$y)),
    distx = rlnorm(sampleno, log(mean(Network$x2-Network$x)), log(sd(Network$x2-Network$x))),
    disty = rlnorm(sampleno, log(mean(Network$y2-Network$y)), log(sd(Network$y2-Network$y)))
  ) %>% mutate(x2=x+distx,y2=x+disty) %>% tbl_df
}

NetOutput_DivergenceA <- read_csv("../Data/Balls_2_DivergenceA/Balls_2_DivergenceA_DetectedObjects.csv")
NetOutput_DivergenceB <- read_csv("../Data/Balls_2_DivergenceB/Balls_2_DivergenceB_DetectedObjects.csv")
NetOutput_ContactA <- read_csv("../Data/Balls_2_ContactA/Balls_2_ContactA_DetectedObjects.csv")
NetOutput_ContactB <- read_csv("../Data/Balls_2_ContactB/Balls_2_ContactB_DetectedObjects.csv")
NetOutput_ClownCar <- read_csv("../Data/Balls_4_Clowncar/Balls_4_Clowncar_DetectedObjects.csv")
NetOutput_Spread <- read_csv("../Data/Balls_4_Spread/Balls_4_Spread_DetectedObjects.csv")
NetOutput_NoContact <- read_csv("../Data/Balls_2_NoContact/Balls_2_NoContact_DetectedObjects.csv")

# Option 1: Gaussian distribution!
SampleNo = 1000
PlotDiagnostics(NetOutput_DivergenceA,GaussianPrior(NetOutput_DivergenceA,SampleNo),"DivergenceA")
PlotDiagnostics(NetOutput_DivergenceB,GaussianPrior(NetOutput_DivergenceB,SampleNo),"DivergenceB")
PlotDiagnostics(NetOutput_ContactA,GaussianPrior(NetOutput_ContactA,SampleNo),"ContactA")
PlotDiagnostics(NetOutput_ContactB,GaussianPrior(NetOutput_ContactB,SampleNo),"ContactB")
PlotDiagnostics(NetOutput_ClownCar,GaussianPrior(NetOutput_ClownCar,SampleNo),"ClownCar")
PlotDiagnostics(NetOutput_Spread,GaussianPrior(NetOutput_Spread,SampleNo),"Spread")
PlotDiagnostics(NetOutput_NoContact,GaussianPrior(NetOutput_NoContact,SampleNo),"NoContact")

# Option 2: Log-normal to prevent proposing zero-heigh boxes.
PlotDiagnostics(NetOutput_DivergenceA,LogNormalPrior(NetOutput_DivergenceA,500))
PlotDiagnostics(NetOutput_ContactB,LogNormalPrior(NetOutput_ContactB,500))
