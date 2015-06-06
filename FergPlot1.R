library(grid)
library(ggplot2)
#change htis later to a dirname call
img<-readPNG("./PCVenn.png")

LoadingSort<-function(df,cutoff,numarr){
  #Creating a list of sorted variables by PC, then choosing top (numArr) ones to display)
  data<-df[order(-abs(df[,1])), ,drop=FALSE]
  data.head<-head(data,numarr)
  #INITIALIZING A COUNTER
  count<-0
  for(i in 1:nrow(data.head)){
    if(abs(data.head[i,1])>cutoff){
      count=count+1
    }
  }
  #Returns a dataframe that has a max of the highest  loading scores (+/-) above a cutoff, 
  data.head<-head(data.head,count)
  return(data.head)
}



#Run LoadingSort First before running FergusonPlot
#df is a sorted single column of loading,
FergusonPlotCoordinates<-function(df){
  div<-2*pi/(nrow(df))
  x<-vector()
  y<-vector()
  txtangle<-vector()
  val<-vector()
  center<-vector()
  angle<-vector()
  row<-vector()
  pc<-vector()
  xmid<-vector()
  ymid<-vector()
  for(i in 1:nrow(df)){
    disp<-div*i
    angle<-c(angle,as.numeric(disp),as.numeric(disp))
    
    x1<-5*cos(disp)
    x2<-1.5*cos(disp)
    xm<-(x1+x2)/2
    x<-c(x,as.numeric(x1),as.numeric(x2))
    xmid<-c(xmid,as.numeric(xm),as.numeric(xm))
    
    row<-c(row,rownames(df)[i],rownames(df)[i])
    
    
    y1<-5*sin(disp)
    y2<-1.5*sin(disp)
    ym<-(y1+y2)/2
    y<-c(y,as.numeric(y1),as.numeric(y2))
    ymid<-c(ymid,as.numeric(ym),as.numeric(ym))
    
    if((disp>pi/2)&(disp<=pi))
      tang<-(pi-disp)
    else if((disp>pi)&(disp<=3*pi/2))
    {
      tang<-disp-pi
      
    }else
    {  tang<-disp
       
    }
    txtangle<-c(txtangle,as.numeric(tang),as.numeric(tang))
    
    value<-c(df[i,1])
    val<-c(val,(round(as.numeric(value),digits=3)),(round(as.numeric(value),digits=3)))
    
    
    center<-c(center,as.numeric(0),as.numeric(0))
    
    lab<-colnames(df)
    pc<-c(pc,lab,lab)
  }
  arr.dat<-data.frame(row,x,y,xmid,ymid,val,center,angle,pc,txtangle,stringAsFactors=1)
  return(arr.dat)
}

#Takes a Loading from FergusonCoord, and the variance explained by this pc
FergusonPlot<-function(arr.dat,var_explained){
  b<-arr.dat[seq(2,nrow(arr.dat),by=2),]
  
  g<-rasterGrob(img,interpolate=TRUE)
  plot.grid<-ggplot(data=arr.dat,aes(x,y,colour=val,group=row,size=val))+scale_colour_gradient2(low="blue",mid="purple",high="red",midpoint=0,guide="colourbar")+theme_bw()#+coord_fixed()# +facet_grid(pc ~.)
  plot.grid<-plot.grid+
    scale_x_continuous(name="",limits=c(-7,7))+
    scale_y_continuous(name="",limits=c(-7,7))+
    geom_line()+
    # geom_segment(data=arr.dat, aes(x=x2,y=y2,xend=x1,yend=y1,size=abs(val)*4),arrow=arrow(type='closed',length=unit(.5,"cm")))+
    geom_text(data=b,aes(x=center+6*cos(angle), y=center+5.3*sin(angle),label=row),colour="black",size=3)+
    annotation_custom(g,xmin=-1,ymin=-1,xmax=1,ymax=1)+
    #geom_point(data=arr.dat,aes(x=center,y=center),colour="black",fill="black",shape=17,size=10)+
    geom_text(data=b,aes(x=center,y=center,label=pc),colour="black",size=4,fontface="bold",family="Times")+
    geom_text(data=b,aes(x=xmid,y=ymid,hjust=1,vjust=1,angle=(txtangle*180/pi),label=val),colour="black",size=4)+
    #geom_point(data=b,aes(x=x,y=y,size=val*5,colour=val),shape=0))+
    theme(line=element_blank(),text=element_blank(),line=element_blank())
  return(plot.grid)
  
}





#Creates Melted Ferguson Plot Coordinates across all pc's, input is f, pca type
MeltCoord<-function(pca,cutoff,numarr){
  loadings<-pca$pca.loadings
  coord<-data.frame()
  for(i in 1:ncol(loadings)){
    a<-LoadingSort(loadings[i],cutoff,numarr)
    acoord<-FergusonPlotCoordinates(a)
    coord<-rbind(coord,acoord)
  }
  return(coord)
}