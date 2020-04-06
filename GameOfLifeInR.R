setwd("C:/Users/Avina Kalle/Documents/MAT_NonlinearDynamics/FinalProject")
library(caTools)
N= 50
T<-7
CS<-array(rep(0,(N*N*T)), dim = c(N, N, T))
n0 = (N^2)/8
i0x<-sample(N, n0, replace=TRUE)
i0y<-sample(N, n0, replace=TRUE)

# for(i in 1:n0){
#   CS[i0x[i], i0y[i], 1]<-1
# }
mid<-floor(N/2)
CS[mid,mid,1]<-1
CS[mid+1,mid,1]<-1
CS[mid,mid+1,1]<-1
CS[mid,mid+2,1]<-1
t<-1

rotate <- function(x){
  t(apply(x, 2, rev))
}

neighbourhood<-function(M,x,y){
  #all middle cells
  if(x>1 && x<ncol(M) && y>1 && y<nrow(M)){
    neighbours<-c(M[x-1,y+1],M[x,y+1], M[x+1,y+1], M[x-1,y],M[x+1,y],M[x-1,y-1],M[x,y-1],M[x+1,y-1]) 
  }
  
  #corners
  if(x==1 && y==1){
    neighbours<-c(M[1,2], M[2,1], M[2,2])
  }  
  if(x==nrow(M) && y==ncol(M)){
    neighbours<-c(M[x-1, y], M[x, y-1], M[x-1, y-1])
  }
  if(x==nrow(M) && y==1){
    neighbours<-c(M[x-1, y], M[x, y+1], M[x-1,y+1])
  }
  if(x==1 && y==ncol(M)){
    neighbours<-c(M[x, y-1], M[x+1, y], M[x+1, y-1])
  }
  
  #1st row
  if(x==1 && y!=1 && y!=ncol(M)){
    neighbours<-c(M[x, y-1], M[x, y+1], M[x+1, y+1], M[x+1, y], M[x+1, y+1])
  }
  
  #last row
  if(x==nrow(M) && y!=1 && y!=ncol(M)){
    neighbours<-c(M[x-1, y-1], M[x-1, y], M[x-1, y+1], M[x,y-1], M[x, y+1])
  }
  
  #1st column
  if(y==1 && x!=1 && x!=nrow(M)){
    neighbours<-c(M[x-1, y], M[x-1, y+1], M[x, y+1], M[x+1, y], M[x+1, y+1])
  }
  
  #last column
  if(y==ncol(M) && x!=1 && x!=nrow(M)){
    neighbours<-c(M[x-1, y], M[x+1, y], M[x, y-1], M[x-1, y-1], M[x+1, y-1])
  }
  return(neighbours)
}

while(t<(T)){
  for(i in 1:nrow(CS[,,t])){
    for(j in 1:ncol(CS[,,t])){
      neighbours<-neighbourhood(as.matrix(CS[,,t]),i,j)
      no_neigh_alive<-length(which(neighbours==1))
      no_neigh_dead<-length(which(neighbours==0))
      
      #alive cell
      if(CS[i,j,t]==1){
        if(no_neigh_alive<2){
          CS[i,j,t+1]<-0
        }
        if(no_neigh_alive==2 || no_neigh_alive==3){
          CS[i,j,t+1]<-1
        }
        if(no_neigh_alive>3){
          CS[i,j,t+1]<-0
        }
      }
      
      #dead cell
      if(no_neigh_alive==3){
        CS[i,j,t+1]<-1
      }
  
    }
  }
  t<-t+1
}

image(rotate(CS[,,t]), axes=FALSE, col=gray.colors(2, 0.1, 1))

#write.gif(CS, 'GoL.gif',delay=50, col=gray.colors(2, 1, 0.1))
