con.vis<-function(pl.no){
sc<-0 #counts consecutive visits to same plant
for (i in 1:(length(pl.no)-1)){
  if(is.na(pl.no) || (pl.no==0) || length(pl.no)==1)  
  {sc<-0
  }
  else{
    if(pl.no[i]==pl.no[i+1])
      sc<-sc+1
  }
}
freq.con.vis<-sc/length(pl.no)
freq.con.vis
}

