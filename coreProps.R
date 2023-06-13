#Areas of concentric zones of an ellipse (used to assess the overall global compactness of a vertebral centrum)
#eli.amson@smns-bw.de

#####Dorsoventral core drill####
#unit = mm
width <- 235/2 #centrum half mediolateral width
SampledCoreHeight <- 77.4
InnerSegmentHeight <- 210/2-SampledCoreHeight# (height at drill site divided by two, minus sampled core height)
propNotSamp <- InnerSegmentHeight/SampledCoreHeight #proportion of the centrum's radius not sampled 
nCoreSegments <- 10 #number of sub-ROIs made on sampled core by the ImageJ macro
CoreSegmentHeight <- SampledCoreHeight/nCoreSegments
CoreSegmentWidth <- width/nCoreSegments*(1-propNotSamp)
InnerSegmentWidth <- width-CoreSegmentWidth*nCoreSegments

Areas <- c(); Areas[1] <- (InnerSegmentHeight*(InnerSegmentWidth))*pi

for (i in 1:(nCoreSegments)) {
  Areas[i+1] <- ((CoreSegmentHeight*i+InnerSegmentHeight)*(CoreSegmentWidth*i+InnerSegmentWidth))*pi-
  ((CoreSegmentHeight*(i-1)+InnerSegmentHeight)*(CoreSegmentWidth*(i-1)+InnerSegmentWidth)*pi)
}
Areas

#####Posteroanterior core drill####
posteriorCentrumHeight <- 182
S=(width)*(posteriorCentrumHeight/2)*pi
S
