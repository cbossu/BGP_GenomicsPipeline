##Rscript
comarg     <- commandArgs()
var  <- comarg[6]

#HETHres.noHZ.K5.loop.3_s20.5.zscore
z.table = NULL
for (i in 1:5){
  file.name = paste("coyeres_K7/coyeresK7.loop.",i,"_s",var,".5.zscore", sep="")
  z.table = cbind(z.table, read.table(file.name)[,1])
}
z.score = apply(z.table, MARGIN = 1, median)
lambda = median(z.score^2) / 0.456
ap.values = pchisq(z.score^2 / lambda, df = 1, lower = F)

write.table(ap.values,file=paste("coyeres_K7/coyeresK7.lfmm.apvalues.var",var,".5.csv",sep=""),row.names=FALSE, append=FALSE)

q = 0.1
L = length(ap.values)
w = which(sort(ap.values) < q * (1:L) / L)
candidates= order(ap.values)[w]

candidates

write.table(candidates,file=paste("coyeres_K7/coyeresK7.lfmm.candidates.var",var,".5.csv",sep=""),row.names=FALSE)

