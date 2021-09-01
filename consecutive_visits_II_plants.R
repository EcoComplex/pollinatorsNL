con.vis.plants<-function(pl.no){
res<-c()
for (i in 1:(length(pl.no)-1)){
  if(is.na(pl.no) || (pl.no==0) || length(pl.no)==1) 
  {
  res<-0
  }
  else{
    if(pl.no[i]==pl.no[i+1])
      res<-c(res,pl.no[i])
  }
}
res
}

