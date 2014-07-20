#Basic functions for entropy
MyLog2p<-function(x){if(x==0) 0 else x*log(x,2)} 
entropy<-function(x){-sum(sapply(x,MyLog2p))}
#Calculator of information, diversity and noise
InfoBulkBinary<-function(p,n)
{n<-2*n;v1<-(1-p)^n;v2<-1-v1;mymat<-cbind(v1,v2);
div<-entropy(c(mean(v1),mean(v2)));a<-NULL;
for (i in 1:length(p)){a[i]<-entropy(mymat[i,])};
noise<-mean(a);c(div,noise,div-noise)}
#p is the vector of frequencies of a given allele across populations
#n is the bulk size in number of diploid plants.
#output: diversity, noise, information
#Humberto Reyes-Valdes