cor_heatmap <- function(data){
  symbols <- names(data)
  ns <- ncol(data)
  par(mfrow=c(1,1),mar=c(3,3,3,3),cex.axis=.75)
  R <- cor(data); R[R==1] <-  NA
  image(1:ns,1:ns,R,col=c("cyan","green","brown","yellow","red"), breaks=c(0,.25,.5,.7,.75,1),xlab="",ylab="",axes=F)
  axis(side=1, at = 1:ns, labels = symbols, font = 2)
  axis(side=2, at = 1:ns, labels = symbols, srt = 90, font = 2)
  axis(side=3, at = 1:ns, labels = symbols, font = 2)
  axis(side=4, at = 1:ns, labels = symbols, font = 2)
  for(i in 1:ns){
    text(rep(i,ns),1:ns,round(R[i,],2),font=2)
  }
}

data_cleaned <- data_cleaned %>% 
  select(-date)

cor_heatmap(data_cleaned, names(data_cleaned))

p_cor_heatmap <- function(data){
  symbols <- names(data)
  ns <- ncol(data)
  par(mfrow=c(1,1),mar=c(3,3,3,3),cex.axis=.75)
  R=cor(data)
  iR=iiR=solve(R)
  for(i in 1:ns){
    for(j in 1:ns){
      iR[i,j]=-iiR[i,j]/sqrt(abs(iiR[i,i]*iiR[j,j]))
    }
  }
    
  diag(iR)=rep(NA,ns)
  cl=c("cyan","green","yellow","red")
  image(1:ns,1:ns,iR,col=cl,breaks=c(-.25,0,.25,.5,.75),xlab="",ylab="",axes=F)
  axis(side=1,at=1:ns,labels=symbols,font=2)
  axis(side=2,at=1:ns,labels=symbols,srt=90,font=2)
  axis(side=3,at=1:ns,labels=symbols,font=2)
  axis(side=4,at=1:ns,labels=symbols,font=2)
  for(i in 1:ns){
    text(rep(i,ns),1:ns,round(iR[i,],2),font=2)
  }
    
}

p_cor_heatmap(data_cleaned, names(data_cleaned))
