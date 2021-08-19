con.vis<-function(pl.no){
sc<-0 #counts consecutive visits to same plant
#res<-c()# which plants are visited consecutively
for (a in 1:(length(pl.no)-1)){
  if (pl.no[a]==pl.no[a+1]){
    sc<-sc+1
    #res<-c(res,pl.no[a])
  }
}
freq.con.vis<-sc/length(pl.no)
freq.con.vis
#res
}


