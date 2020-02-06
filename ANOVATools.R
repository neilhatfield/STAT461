require(sjstats)

.PS<- function(d){
	pnorm(-0.7071067*d, mean = 0, sd = 1, lower.tail = FALSE)
}

.postHoc <- function (aov.obj){
	std.err <- sqrt(deviance(aov.obj) / df.residual(aov.obj))
	diffs <- TukeyHSD(aov.obj)$group[,1]
	ds <- lapply(diffs,.f<-function(x){x / std.err})
	ps<- lapply(ds,.PS)
	name.list<-names(diffs)
	results<-data.frame(Cohens.D=matrix(unlist(ds)),Prob.Super=matrix(unlist(ps)),row.names=name.list)
	results
}

anova.effects <- function(aov.obj){
	eta2 <- sjstats::eta_sq(aov.obj)$etasq
	omega2 <- sjstats::omega_sq(aov.obj)$omegasq
	epsilon2 <- sjstats::epsilon_sq(aov.obj)$epsilonsq
	cohensf <- sjstats::cohens_f(aov.obj)$cohens.f
	omnibus <- data.frame(Eta.Sq=eta2,Omega.Sq=omega2,Epsilon.Sq=epsilon2,Cohens.f=cohensf)
	posthoc <-.postHoc(aov.obj)
	list(Omnibus.Effects=omnibus,Post.Hoc.Effects=posthoc)
}