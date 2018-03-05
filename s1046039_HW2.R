library(amap)
coh<-function(cltable,data)
{
  cl1 = cltable[, 1]
  couv = NULL
  for (node in 1:nrow(data)) 
  {
    beclI =scl (cl1 , node) 
    becl = cl1[[beclI]]  
    dist = NULL
    if (length(becl) > 1)
    {
      for (i in 1:length(becl)) {
        node2 = -becl[i]
        dist = c(dist,data[node, node2])
      }
      co = mean(dist , na.rm = T)
    }
    else    
    {
      co = 0
    }
    couv = c(couv , co)
  }
  return(couv)
}
coupling <- function(table  , data)
{
  cl1 = table[, 1]
  vector= NULL
  for (node in 1:nrow(data)) {
    becl =scl (cl1, node)
    otcl = cl1[-becl]
    dist = NULL
    if (length(otcl) > 0)
    {
      for (i_otherC in 1:length(otcl)) {
        decl = otcl[[i_otherC]]
        discl = NULL
        for (index in 1:length(decl)) {
          node2 = -decl[index]
          discl = c(discl , data[node, node2])
        } 
        avgdicl = mean(discl , na.rm = T)
        dist = c(dist , avgdicl)
      }
      coup = min(dist , na.rm = T)
    }
    else   
    {
      coup = 0
    }
    vector= c(vector, coup)
  }
  return(vector)
}
scl <- function(cl , n)
{
  for (i in 1:length(cl)) {
    if ((-n) %in% cl[[i]])
    {
      return(i)
    }
  }
}
s = read.table("input.csv" , sep = ",", header = T)
s
data = as.dist(s)
m = hcluster(data)
plot(m)######################################Ã¸¹Ï
threshold = 15

slic =  which((m$height) < threshold)
m$merge
obj = m$labels
obj = as.factor(obj)

cl1 = list()
lm = NULL

for (i in 1:length(m$labels)) {
  cl1 = c(cl1 , list(-i))
  lm[i] = 0
}

for (i in 1:length(slic)) 
{
  mer = (m$merge)[i,]
  if (mer[1] > 0)
  {cla = which(lm == mer[1])}
  else if (mer[1] < 0)
  {   cla = which(cl1 %in% mer[1])}
  if (mer[2] > 0)
  {clb = which(lm == mer[2])}
  else if (mer[2] < 0)
  {clb = which(cl1 %in% mer[2])}
  cl1[[cla]] = c(cl1[[cla]] , cl1[[clb]])
  cl1 = cl1[-clb]
  lm[cla] = i
  lm = lm[-clb]
}

cltable = cbind(cl1 , lm)
x = coupling(cltable,s)
o = coh(cltable,s)
lar = c()
for (i in 1:length(o)) {
  if (o[i] > x[i]) {
    lar[i] = o[i]
  }
  else {
    lar[i] = x[i]
  }
}
lar

sc = (x - o) / lar
sc[sc %in% c(1, -1)] = 0
output = NULL
for (i in 1:length(sc)) {output = cbind(output , sc[i])}
colnames(output) = colnames(s)
output = data.frame(output)
output######################################################output
write.table(output ,file = "output.csv" ,append = F ,sep = "," ,row.names = F)



