#Given the partial uncertainty of the positions of the recovered vertebrae of the new species, we used random draws among a virtual complete skeleton (Cynthiacetus peruvianus' holotype, MNHN.F.PRU10)
#eli.amson@smns-bw.de

#Abbreviations: Arch, neural arch, Cy, Cynthiacetus peruvianus' holotype; L, lumbar vertebrae; Nsp, new species; Th, thoracic vertebrae; TransPs, transverse processes (both left and right); V, volume


####Scaling up Cynthiacetus holotype####
#Lengths in mm
Cy_4LastTh <- data.frame(W=c(135.1,138.4,138.65,145.2),H= c(123.2,120.7,124,131.52),L=c(126.2,128.4,129.7,131.6))

Cy_L<- data.frame(W= c(145.1,150.2,146,156.9,160.1,154.1,154.7,156.2,156.8,161.2,162.3,161,161.3,162.6,162.5,162,166.7)
                  ,H= c(130.1,137,136,154.7,153.7,147.3,144.3,149.5,151.5,158.9,160.2,160.4,165.7,166.8,169,169.1,172.6)
                  ,L=c(134.1,129.2,134,136,137.3,140.2,138,135.2,132,143.1,141.4,141.7,144.9,143.3,145,150,158.5))

nThoracicsToDraw <- 2
nTLumbarsToDraw <- 8

Cy_draw_Th <- Cy_draw_L <- data.frame()

for (i in 1:10000){
Cy_draw_Th[i,"W"] <- sum(sample(Cy_4LastTh$W,nThoracicsToDraw))
Cy_draw_Th[i,"H"] <- sum(sample(Cy_4LastTh$H,nThoracicsToDraw))
Cy_draw_Th[i,"L"] <- sum(sample(Cy_4LastTh$L,nThoracicsToDraw))

Cy_draw_L[i,"W"] <- sum(sample(Cy_L$W,nTLumbarsToDraw))
Cy_draw_L[i,"H"] <- sum(sample(Cy_L$H,nTLumbarsToDraw))
Cy_draw_L[i,"L"] <- sum(sample(Cy_L$L,nTLumbarsToDraw))

}

mean(Cy_draw_Th$W)
mean(Cy_draw_Th$H)
mean(Cy_draw_Th$L)

mean(Cy_draw_L$W)
mean(Cy_draw_L$H)
mean(Cy_draw_L$L)

####'Pachyostosis ratio'####
#Volumes in cm3

Cy_4LastTh_s <- data.frame(V=c(11509,12240,12513,12836))
Cy_L_s<- data.frame(V=c(13416,13795,14313,15388,15301,14559,14814,15372,15278,16548,16854,17053,17182,16691,17157,17970,18636))
Cy_draw_Th_V_s <- Cy_draw_L_V_s <-Cy_draw_R_s <-  c()

nThoracicsToDrawV <- 1
nTLumbarsToDrawV <- 4

for (i in 1:10000){
  Cy_draw_Th_V_s[i] <- sum(sample(Cy_4LastTh_s$V,nThoracicsToDrawV))
  Cy_draw_L_V_s[i] <- sum(sample(Cy_L_s$V,nTLumbarsToDrawV))
}

mean(Cy_draw_Th_V_s)
mean(Cy_draw_L_V_s)

Nsp_Vertebrae <- sum(c(81140.024,59857.908,69152.288,74449.936,63259.288))
Nsp_Rib<- 10617


RatioVertebrae <- Nsp_Vertebrae/(mean(Cy_draw_Th_V_s)+mean(Cy_draw_L_V_s))
RatioVertebrae
RatioRib <- Nsp_Rib/mean(c(2796,2821))
RatioRib

####Proportions of vertebral parts (based on Cynthiacetus' holotype####
# see Supplementary Data 4
Cy_L_p <-data.frame(Centrum=c(0.773444721,0.782393295,0.774847309,0.786385147,0.805008502,0.792410337,0.798030079,0.786904308,0.800693455,0.809664419,0.814464236,0.819248993,0.809578795,0.823178362,0.83624909,0.864128587,0.859419042),
Arch= c(0.169651847,0.157968328,0.167918908,0.151427055,0.136261937,0.141054768,0.144631208,0.15130877,0.141113352,0.129281933,0.122318213,0.115755321,0.116065188,0.107965102,0.107661743,0.093627358,0.093374848),
TransPs=c(0.10767953,0.112563641,0.108270819,0.117093791,0.110943461,0.124768342,0.108458551,0.116382903,0.109985951,0.115081172,0.118917432,0.12205812,0.138419698,0.128841493,0.106220513,0.081063653,0.090156293)) 

nLumbarsToDraw_p <- 4


Cy_draw_L_p <- data.frame()

for (i in 1:10000){
  Cy_draw_L_p[i,"Centrum"] <- sum(sample(Cy_L_p$Centrum,nLumbarsToDraw_p))
  Cy_draw_L_p[i,"Arch"] <- sum(sample(Cy_L_p$Arch,nLumbarsToDraw_p))
  Cy_draw_L_p[i,"TransPs"] <- sum(sample(Cy_L_p$TransPs,nLumbarsToDraw_p))
  
}

Nsp_4Lumbar_Prop_centrum <- 0.775667754219882# see Supplementary Data 4
Nsp_4Lumbar_Prop_Arch <- 1.1725743168624# see Supplementary Data 4
Nsp_4Lumbar_Prop_TransPs <- 2.16323590575472# see Supplementary Data 4
  
Nsp_4Lumbar_Prop_centrum/mean(Cy_draw_L_p$Centrum)#mean(Cy_draw_L_p$Centrum)/Nsp_4Lumbar_Prop_centrum
Nsp_4Lumbar_Prop_Arch/mean(Cy_draw_L_p$Arch)
Nsp_4Lumbar_Prop_TransPs/mean(Cy_draw_L_p$TransPs)
