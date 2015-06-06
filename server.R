library(shiny)
library(gdata)
library(corrplot)
library(missMDA)
library(FactoMineR)
library(pcaMethods)
library(gridExtra)
library(png)
library(grid)


source("./PCAFunc.R")
source("./FergPlot.R")
#devium source file super useful
source("http://pastebin.com/raw.php?i=UyDBTA57")

#Define server logic

shinyServer(function(input,output){
  pp<- theme(
    axis.line = element_line(colour = 'gray', size = .75), 
    panel.background = element_blank(),  
    plot.background = element_blank()
  )	
#get the data from input window
  data<-reactive({
    dataFile <- input$file1
    if(is.null(dataFile))
      return(NULL)
  read.csv(dataFile$datapath, header=input$header,sep=',',quote="'")
    
  })
#parse data
  parse<-reactive({
    if(is.null(data()))
      return(NULL)
    
   p2<-removeZero(removeNA(removeString(data())))
  
    p2
  })
# impute data
comp<-reactive({
  #nb<-estim_ncpPCA(parse(),ncp.max=5)
  
  if(is.null(parse()))
    return(NULL)
  
  if(input$all){
    df.comp<-imputePCA(parse(),3)
    
  }
  if(!input$all){
    p<-parse()
    list<-input$colNames
    df.inc<-p[,list]
    df.comp<-imputePCA(df.inc,3)
  }
    
  complete<-df.comp$completeObs
  complete
  
})
PCA<-reactive({
  if(is.null(data()))
    return(NULL)
  
  pca.inputs<-list()
  pca.inputs$pca.data<-comp()
  pca.inputs$pca.algorithm<-input$method
  pca.inputs$pca.components<-input$PrCs
  pca.inputs$pca.center<-input$center
  pca.inputs$pca.scaling<-input$scaling
  devium.pca.calculate(pca.inputs,return="list",plot=F) 
  
  
})



    
#number of PCs
output$PCs<-renderUI({
  if (is.null(parse())) { return(NULL) }
  maxPCs<-ncol(comp())
  numericInput("PrCs", "Number of Principal Components",
               2, min = 2, max = maxPCs)
})     
output$FPlot<-renderUI({
  if(is.null(PCA()))
    return(NULL)
  maxNum<-10
 sliderInput("Pc2Plot","Choose which PC to create your FPlot from",min=1,max=input$PrCs,value=1,step=1)
})
output$NumArr<-renderUI({
  if(is.null(PCA()))
    return(NULL)
  numericInput("NumArr", "Number of Maximum Arrows for Ferg Plot, Likely will be less as all numbers <.3 will be dropped",5,min=2,max=15)
  
})
output$CutOff<-renderUI({
  if(is.null(PCA())){
    return(NULL)
  }
  numericInput("cutoff","Choose a value of significance for Loadings, between 0-1",.3,min=0,max=1)
})

output$contents<-renderPrint({
  if (is.null(PCA()))
    return(NULL)
  else {
    x<-PCA()
    x<-as.data.frame(x$pca.loading, names=rownames(x$pca.loadings))
  dim(x)
  }
})
output$parsed<-renderPrint({
    dim(parse())

  })
#create a listnames for input box 
output$col<-renderUI({
    p<-parse()
    if(is.null(p)){
      return()
  }
  if(input$all)
    return()
  compcol<-colnames(complete(p))
  icompcol<-colnames(incomplete(p))
          selectizeInput("colNames","select variables for analysis",
                   choices=list(Complete=c(setNames(compcol,compcol)),
                                incomplete=c(setNames(icompcol,icompcol))),
                   multiple=TRUE)
  
  })
#make screeplot
output$screeplot <- renderPlot({
  if (is.null(PCA()))
    return(NULL)
  else {
    x<-PCA()
    screepl(x)

  }
})

BasicPlot<-reactive({
  if (is.null(PCA())) {
    return(NULL)
  } else {
    tmp<-PCA()
    i<-input$Pc2Plot
    varexp<-f$pca.eigenvalues$eigenvalues[i]
    a<-LoadingSort(tmp$pca.loadings[i],input$cutoff,input$NumArr)
    b<-FergusonPlotCoordinates(a)
    plot<-FergusonPlot1(b,varexp)
    plot
}

})

output$FergPlot<-renderPlot({
    BasicPlot()
    
  })


#loadings plot
output$loadings <- renderPlot({
  if (is.null(PCA())) {
    return(NULL)
  } else {
    tmp<-PCA()
    al<-data.frame(tmp$pca.loadings,names=rownames(tmp$pca.loadings))
    p<-ggplot(al, mapping = aes_string(x = names(al)[1], y = names(al)[2], label = "names")) + 
      geom_text(size=4,alpha=0.75)
    print(p)
  }
}) 


    
  })
