#Removes all columns with only NaN values
library(grid)
library(ggplot2)
removeNA <- function(df){
  df[ , colSums(is.na(df))!=nrow(df)]
  
}
#Removes columns that have any instance of string
removeString <-function(df){
 
  df[!sapply(df, is.factor)]
  
  
}
#Remove columns that sum to zero
removeZero<-function(df){
  df[ ,colSums(df,na.rm=TRUE)!=0]
}
#Returns a list of all columns with no missing data
complete <-function(df){
  dq<-df[,!(colSums(is.na(df))>0)]
}
#Returns a list of all columns with missing values
incomplete<-function(df){
  dq<-df[,colSums(is.na(df)>0)]
}
#will take a  list, and make a dictionary where the colnames=colnames

#Standardizes based off z score across columns
stdMean <- function(df){
  df<-scale(df,center=TRUE,scale=TRUE)
}
#convert vector to named list
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
} 
#screeplot takes input of type from make.devium.pca
screepl<-function(pca){
  x<-c(1:nrow(pca$pca.eigenvalues))
  y<-pca$pca.eigenvalues$eigenvalues
  plot(x,y,type="b",main="Scree Plot of Eigenvalues vs. Principal Components", xlab="Principal Components",ylab="EigenValues")
  
}


