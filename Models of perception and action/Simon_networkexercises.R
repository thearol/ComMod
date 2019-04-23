###
setwd(paste(getwd(),'/Models of perception and action', sep = ''))
library(igraph)

m<-matrix(rbinom(10*10, size=1,prob = 0.1),10,10)
diag(m)=0

g<-graph_from_adjacency_matrix(m,mode='undirected')
plot(g)

degree(g)
mean(degree(g))
dens(degree(g))


m<-matrix(0,10,10)
diag(m)
c()

m<-rbinom(1, size=1,prob = 0.1)

install.packages('FastKNN')
library(FastKNN)

install.packages('RANN')
library(RANN)

m <- as.matrix(nn(data.frame(x=x, y=y, z=rep(0,length(x))), p=30)$nn.idx)
m <- get.knn(data.frame(x=x, y=y), 30)$nn.index

df <- as.data.frame(cbind(x = seq(from=1, to=10, by=1), y=seq(from=1, to=10, by=1)))
dist_mat <- as.matrix(dist(df, method = "maximum", upper = TRUE, diag=TRUE))

x <- matrix(rnorm(20),nc=2)
y <- x/sqrt(rowSums(x^2))
y
circleGrob(x=0.5, y=0.5, r=0.5, default.units="npc", name=NULL,
           gp=gpar(), vp=NULL)

library(grid)

x1 = runif(5, min = -1, max = 1)
x2 = x1
y1 = sqrt(1 - x1^2)
y2 = (-1)*y1
x = c(x1,x2)
y = c(y1,y2)


dist_mat<-matrix(rev(dist_mat),10,10)

df <- as.data.frame(cbind(x = x, y=y))
dist_mat <- as.matrix(dist(df, method = "maximum", upper = TRUE, diag=TRUE))
?dist
dist_mat
## Let's say k = 1...
k <- 4
nrst <- lapply(1:nrow(dist_mat), function(i) k.nearest.neighbors(i, dist_mat, k = k))

## Build w
w <- matrix(nrow = dim(dist_mat), ncol=dim(dist_mat)) ## all NA right now
w[is.na(w)] <- 0 ## populate with 0
for(i in 1:length(nrst)) for(j in nrst[[i]]) w[i,j] = 1

#w[1,10]<-1

g<-graph_from_adjacency_matrix(w,mode='undirected')
plot(g)

graph_from_edgelist()

