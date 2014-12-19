##########################################
#	Simulator for Angler Avidity Bias
#	and correction
#	Robert Ahrens Dec 2014
#	See Habib 2012 IJRRAS 11: 419-432 
##########################################
nanglers=1000
ntrips=10000
# Create a distribution of angler ability
# log-normal
cpues=rlnorm(nanglers,log(1),0.6)
hist(cpues)
#bias the sampling - avidity in proportion to expected cpue
bias=TRUE
# sampling randomly
probsamp=rep(1/nanglers,length(cpues))
# avidity in proportion to skill
if (bias) probsamp=cpues 
# who is sampled
isamp=sample(1:nanglers,ntrips,replace=TRUE,prob=probsamp)
# what is the individual's mean cpue
means=cpues[isamp]
# given the mean cpue what was the reported catch rate assuming poisson.
cpuesampled=sapply(means,FUN=function(x)rpois(1,x))
#true estimate of mean cpue
exp(mean(log(cpues)))
#Calcuate the weighted geometeric mean accounting for zeros that arise in the poisson sampling 
ii=which(cpuesampled>0)
# geometic mean accounting for zeros but not the avidity bias
length(ii)/ntrips*exp(mean(log(cpuesampled[ii])))
#correcting for sample bias
wgt=rep(1/length(ii),length(ii))
if(bias)
{
	#have not found a paper to prove that this is the appropriate way to
	#calcuate the weights but it should be a good approximation
	wgt=1/probsamp[isamp[ii]]
	wgt=wgt/sum(1/probsamp[isamp])
}
#geometic mean accounting for zeros and avidity bias
length(ii)/ntrips*exp(sum(wgt*log(cpuesampled[ii])))
# to simulate loss of poor angler as population declines cpue can be raised to a power >1

