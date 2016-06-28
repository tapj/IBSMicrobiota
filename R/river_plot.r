# river plot for IBS data

river.ibs=function(assigned, ngroup=3, palette=NULL){
V4V5=prop.table(table(assigned$fac[assigned$visites=="V4"],assigned$fac[assigned$visites=="V5"] ))
V5V6=prop.table(table(assigned$fac[assigned$visites=="V5"],assigned$fac[assigned$visites=="V6"] ))

V4V5.edges=NULL
for(i in rownames(V4V5)) {for(j in colnames(V4V5) ) {tmp=c(i, j, V4V5[i,j]); V4V5.edges=rbind(V4V5.edges, tmp)       }}

V5V6.edges=NULL
for(i in rownames(V5V6)) {for(j in colnames(V5V6) ) {tmp=c(i, j, V5V6[i,j]); V5V6.edges=rbind(V5V6.edges, tmp)       }}

edges=rbind(V4V5.edges, V5V6.edges)
rownames(edges)=1:nrow(edges)
edges=data.frame(edges)
edges[,3]=as.numeric(as.character(edges[,3]))
names(edges)=c("N1","N2","Value")
edges=edges[-which(edges$Value==0),]


nodes=data.frame(ID=levels(as.factor(assigned$fac)), x=sort(rep(1:3,ngroup)), y=rep(0:(ngroup-1),3))

library(RColorBrewer)

if(length(palette)==0) {

palette = paste0(brewer.pal(ngroup, "Set1"), "60")

} else { palette = paste0(palette, "60") }

styles = lapply(nodes$y, function(n) {
   list(col = palette[n+1], lty = 0, textcol = "black")
 })
names(styles) = nodes$ID


rp <- list(nodes = nodes, edges = edges, styles = styles)
class(rp) <- c(class(rp), "riverplot")

plot(rp, plot_area = 0.95, yscale=0.06)

}
