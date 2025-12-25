# ==================== Installing + importing needed libraries ====================
require("ggplot2")
require("plotly")

library(ggplot2)
library(plotly)
N=1000 #  Number of simulations

# ==================== Simulations of C with sum C = 1 ====================
C=array(dim=c(N,6))
for( i in 1:N){
a=runif(5) # Ci are chosen randomly following a uniform distribution
a=c(0,a,1)
C[i,]=diff(sort(a))
rm(a)
}

#===================== simulation of D ====================

attaque=rbinom(N,6,0.2) # chosen randomly but following a binomial distribution
D=array(dim=c(N,6))
for(i in 1:N){

b=c(0,0,0,0,0,0)
b[sample(x = 1:6,size = attaque[i])]=1

a=runif(6)
D[i,]=a*b
}

#===================== coding  of the function E =====================
E=array(dim=c(N,7))
E[,1]=1
attack=function(E,D,C){
  
  
  for(i in 1:N){
    for(j in 2:7){
      E[i,j]=E[i,j-1]*(1-sqrt(D[i,j-1])*sqrt(C[i,j-1]))
      
    }
  }
  #print(E)
  return(E[,7])
}

#===================== formatting the results for the plot generation =====================

Result=cbind(C,D)
Result=as.data.frame(cbind(Result,attack(E,D,C)))
names(Result)=c("C1","C2","C3","C4","C5","C6","D1","D2","D3","D4","D5","D6","E")
Result$varC=apply(Result[,1:6],1,var)

myfun=function(x){
  return(cor(x[1:6],x[7:12]))
}

Result$cor=apply(Result[,1:12],1,myfun)
Result$cor[is.na(Result$cor)]=0
Result$SumD=apply(Result[,7:12],1,sum)


#===================== plot ==========================================

ggplot(Result)+geom_point(aes(SumD,E)) # check


fig <- plot_ly(Result, x = ~varC, y = ~cor, type = 'scatter', mode = 'markers',
               marker = list(size = ~(20*E),  # Adjusted for clarity
                             color = ~SumD,
                             colorscale = 'RdYlBu',
                             colorbar = list(title = 'Sum(δi)',
                                             thickness=10,
                                             len=0.5),# Colorblind-friendly palette
                             opacity = 0.6))  # Slightly higher opacity for print clarity
fig <- fig %>% layout(#title = 'Your Title Here',
                      xaxis = list(title = '(Variance(Ci)'),
                      yaxis = list(title = 'Correlation(Ci,δi)'),
                      legend = list(title = 'Legend Title'),
                      font = list(family = "Arial, sans-serif", size = 12),
                      plot_bgcolor = 'white',  # Clean background
                      paper_bgcolor = 'white')

fig






















