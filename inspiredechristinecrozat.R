source("https://raw.githubusercontent.com/Valexandre/Appels/master/useit.R")
library(concaveman)
library(smoothr)

## Inspiration Christine Crozat

#Fond blanc, trois à quatre niveau de profondeurs, un fond de couleur, une couche de forme transparente, et deux couches de formes de blanc semi opaques suivant plus ou moins les contours.

Dimensions<-c(800,600)
NombreDeSommets<-12
LargeurMax<-Dimensions[1]
HauteurMax<-Dimensions[2]
degredevariation2<-20
degredevariation3<-30
xplusoumoinspourycent<-function(x,y){
  #plus ou moins?
  ifelse(sample(1:2,1)==1,round(x+(x*y/100)),round(x-(x*y/100)))
}
xplusoumoinspourycent(Points$x,20)
library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
fond=sample(col_vector,1)


CreeUnPolygone<-function(NombreDeSommets,LargeurMax,HauteurMax,fond,degredevariation2,degredevariation3,degredetransparence2,degredetransparence3){
  Points<-data.frame(x=sample(1:LargeurMax,size = NombreDeSommets),
                     y=sample(1:HauteurMax,size = NombreDeSommets))
  Points[NombreDeSommets+1,]<-Points[1,]
conc<-concaveman(st_as_sf(Points,coords = c("x","y")))
conc_coord<-as.data.frame(st_coordinates(conc))
conc_smooth_chaikin <- smooth(conc, method = "chaikin")
cadre=data.frame(x=c(0,0,LargeurMax,LargeurMax,0),
                 y=c(0,HauteurMax,HauteurMax,0,0))
cadre_sf<-concaveman(st_as_sf(cadre,coords=c("x","y")))
diff_conc=st_difference(cadre_sf,conc_smooth_chaikin)

formes<-conc_coord%>%
  rowwise()%>%
  mutate(x2=xplusoumoinspourycent(X,sample(1:degredevariation2,1)),
         y2=xplusoumoinspourycent(Y,sample(1:degredevariation2,1)),
         x3=xplusoumoinspourycent(X,sample(1:degredevariation3,1)),
         y3=xplusoumoinspourycent(Y,sample(1:degredevariation3,1)))
conc2<-concaveman(st_as_sf(formes,coords = c("x2","y2")))
#conc2<-as.data.frame(st_coordinates(conc2))
conc2_smooth <- smooth(conc2, method = "chaikin")
diff_conc2=st_difference(cadre_sf,conc2_smooth)
conc3<-concaveman(st_as_sf(formes,coords = c("x3","y3")))
#conc3<-as.data.frame(st_coordinates(conc3))
conc3_smooth <- smooth(conc3, method = "chaikin")
diff_conc3=st_difference(cadre_sf,conc3_smooth)

plot(ggplot()+
  geom_rect(aes(xmin=0,xmax=LargeurMax,ymin=0,ymax=HauteurMax),fill=fond,colour=NA)+
  geom_sf(data=diff_conc,fill="#FFFFFF80",colour=NA)+theme_void()+
  geom_sf(data=diff_conc2,fill="white",colour=NA,alpha=degredetransparence2)+
  geom_sf(data=diff_conc3,fill="white",colour=NA,alpha=degredetransparence3))

}
params<-c(sample(12:40,1),Dimensions[1],Dimensions[2],sample(col_vector,1),60,90,0.3,0.1)

sortunjpeg(
CreeUnPolygone(NombreDeSommets = as.numeric(params[1]),
               LargeurMax =  as.numeric(params[2]),
              HauteurMax = as.numeric(params[3]),
               fond = params[4],
               degredevariation2= as.numeric(params[5]),
              degredevariation3 = as.numeric(params[6]),
               degredetransparence2 = as.numeric(params[7]),
              degredetransparence3 = as.numeric(params[8])),
               Dimensions[1]*2,Dimensions[2]*2,paste0("TestCrozat",gsub("\\.","",gsub("#","",paste0(params,collapse = "-")))))
