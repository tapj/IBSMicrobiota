
# classification liblinear with bootstrap only

library(LiblineaR)

trainset.select = function(dataframe, r) {

	label        = names(dataframe)
	label.length = length(label)
	r.label      = round(label.length*r)
	label.sample = sample(label, r.label, replace=F)

	return(dataframe[,label.sample])


}


logistic.classification = function(dataframe, groups, r=NULL, nboot=100) {
	require(LiblineaR)

	result.matrix           = matrix(nr = ncol(dataframe), nc = nboot)
	rownames(result.matrix) = names(dataframe)
	metadata                = data.frame(row.names=names(dataframe), groups)

	if(is.null(r)) r = 0.5

	for (i in 1:nboot){
		# select train set and test set
		xTrain = t(trainset.select(dataframe,r))
		yTrain = metadata[rownames(xTrain),"groups"]

		xTest  = t(dataframe[,!(names(dataframe) %in% rownames(xTrain))])
		yTest  = metadata[rownames(xTest),"groups"]




		# Center and scale data
		#s=scale(xTrain,attr(s,"scaled:center"),attr(s,"scaled:scale"))
		s = log10(xTrain+0.000001)


		# Logistic Regression
		ty = 6 #L1 logistic regression

		# Tune the cost parameter of a logistic regression via a 10-fold cross-validation
		#try=c(10000, 3000, 1000,300, 100, 30, 10, 3, 1, 0.3, 0.1)
		#tryTypes=c(0:7)
		try = c(10000,  1000, 100,  10,  1, 0.1)
		res = c()
		#for(ty in tryTypes){
			for(co in try){
				acc = LiblineaR(data=s,labels=yTrain,type=ty,cost=co,bias=TRUE,cross=10,verbose=FALSE)
				res = c(res,acc)
				#cat("type",ty,"Results for C=",co," : ",acc," accuracy.\n",sep="")
				#flush.console()
			}
		#}

		# Re-train a model with best C value.
		best = which.max(res)
		co   = try[best]
		m    = LiblineaR(data=s,labels=yTrain,type=ty,cost=co,bias=TRUE,verbose=FALSE)


		# Scale the test data
		#s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
		s2 = log10(xTest+0.000001)


		# Make prediction
		pred = predict(m,s2,proba=TRUE) 
		p    = pred$predictions

		result.matrix[rownames(xTest),i] = p

		cat(i,"\r")
		flush.console()

	}

	result.table = apply(result.matrix,1, function(x) {x=factor(x, levels=levels(factor(groups)));  table(x)})
	result.prob  = t(prop.table(result.table,2))

	groups.est  = apply(result.prob,1, function(x) names(  which(x==max(x))[1]   ))
	groups.prob = apply(result.prob,1, function(x) x[which(x==max(x))[1]]   )

	result = data.frame(result.prob, groups.input=groups, groups.output=groups.est, confidence=groups.prob)


	return(result)

}


# classification liblinear function with CV, bootstrap and model performance estimator

cost_param = function(data, labels, type, cost,co, cross, verbose=FALSE, cost.try=c(10000,  1000, 100,  10,  1, 0.1)) {

	res = c()

	for(co in cost.try){
				acc = LiblineaR(data=data,labels=labels,type=type,cost=co,bias=TRUE,cross=10,verbose=FALSE)
				res = c(res,acc)
				
			}
	
  best = which.max(res)
	co   = cost.try[best]
  
	return(co) #best cost
}


create_folds = function(data, nfolds) {

perm = sample(1:nrow(data), nrow(data)) / nrow(data)
id   = rep(0, nrow(data))

for (f in nfolds:1) { id[perm <= f/nfolds] = f }

return(id)

}


logistic.CV = function(data, labels, nfolds, ty, cost.try, sparse=FALSE) {

require(LiblineaR)

if(length(levels(as.factor(labels))) != 2) stop("length of labels is not equal 2")

list          = 1:nfolds
id            = create_folds(data, nfolds)
prediction    = NULL
probabilities = NULL

actual = NULL
weight = vector("list", nfolds)
model  = vector("list", nfolds)

	for (i in 1:nfolds) {
	
	xTrain = subset(data, id %in% list[-i])
	
	if(!sparse) {
		xTrain = scale(xTrain,center=TRUE,scale=TRUE) #scale the data	
	} else {
		xTrain = t(t(scale(xTrain, scale=FALSE))/(apply(xTrain,2,sd)+quantile(apply(xTrain,2,sd),0.1))) # add 10th sd
		xTrain[,attr(na.omit(t(xTrain)), "na.action")] = 0
	}
	
	yTrain = subset(labels, id %in% list[-i])
	xTest  = subset(data,   id %in% c(i))
	
	if(!sparse) {
	  xTest = scale(xTest,center=TRUE,scale=TRUE) # independently scale the test set
	} else {
		xTest = t(t(scale(xTest, scale=FALSE))/(apply(xTest,2,sd)+quantile(apply(xTest,2,sd),0.1))) # add 10th sd
		xTest[,attr(na.omit(t(xTest)), "na.action")] = 0
	}
	
	yTest = subset(labels, id %in% c(i))
	
	
	#print(dim(xTest))
	
	
	# Tune the cost parameter of a logistic regression via a nested 10-fold cross-validation
	co = cost_param(data=xTrain, labels=yTrain, type=ty,cost=co, cross=10,verbose=FALSE, cost.try)
	#print(co)
	
	# Re-train a model with best C value.
	mymodel     = LiblineaR(data=xTrain,labels=yTrain,type=ty,cost=co,bias=TRUE,verbose=FALSE)
	weight[[i]] = mymodel$W
	model[[i]]  = mymodel
	
	pred = predict(mymodel,xTest,proba=TRUE) 
	p    = pred$predictions
	prob = pred$probabilities[,1]
	
	prediction    = c(prediction,p)
	probabilities = c(probabilities,prob)
	actual        = c(actual, as.character(yTest))
	
	}

	pred = data.frame(prediction=prediction,actual=actual,probabilities=probabilities)
	
	W            = sapply(weight, rbind, simplify=TRUE)
	row.names(W) = colnames(weight[[1]])
	
return(list(pred = pred,W = W, model = model))

}


logistic.CV.boot = function(data=dd, labels=lab, nfolds=10, ty=6, cost.try=c(100000, 10000,  1000, 100,  10,  1, 0.1,0.01,0.001), sparse=TRUE, nboot=10, p.bar=TRUE){
	
  res = vector("list",nboot)

	if(p.bar) {pb <- txtProgressBar(min = 0, max = nboot, style = 3)}
  
	for (b in 1:nboot) {

	#do logistic CV
		res[[b]] = logistic.CV(data=dd, labels=lab, nfolds=10, ty=6, cost.try=c(100000, 10000,  1000, 100,  10,  1, 0.1,0.01,0.001), sparse=sparse)
		if(p.bar) {setTxtProgressBar(pb, b)}
    
	}
  
	if(p.bar) {close(pb)}

	return(res)
}




model.performance = function(res, nboot=10){

	perf    = vector("list",nboot)
	model.W = vector("list",nboot)
	
  for(b in 1:nboot) {
	# calculate performance
		#levels(res[[b]]$pred$actual) = c(1,-1)
		pre = prediction(prediction=res[[b]]$pred$probabilities+(rnorm(length(res[[b]]$pred$probabilities))/(10^9)), labels=res[[b]]$pred$actual) #add some noise to fix a bug
		per = performance(pre,"tpr","fpr")
		AUC = (performance(pre,"auc"))@y.values[[1]]
		

		fpr = per@"x.values"[[1]]
		tpr = per@"y.values"[[1]]

		perf[[b]]    = list(AUC=AUC,fpr=fpr,tpr=tpr)
		model.W[[b]] = res[[b]]$W
		
	}

	AUC = unlist(lapply(perf, function(x){x$AUC}))

	tpr = sapply(perf, function(x){x$tpr})
	fpr = sapply(perf, function(x){x$fpr})

	#w=apply(sapply(model.W, function(x) { apply(x, 1, mean) }), 1, mean)
	#w=sapply(model.W, function(x) { apply(x, 1, mean) })
	#w=t(w/sum(abs(w)))*nboot

	w1 = NULL

	for( i in 1:length(model.W)) {

    w1 = cbind(w1,model.W[[i]])

  }

	w2 = w1
	w3 = matrix(nr=nrow(w2), nc=5)
	rownames(w3)=row.names(w2)
	colnames(w3)=c("q25","q50","q75","support","abs.w")

	for(i in 1:nrow(w2)) {
		abs.w = 0
		q25   = quantile(na.omit(w2[i,]),0.25)
		q50   = quantile(na.omit(w2[i,]),0.50)
		q75   = quantile(na.omit(w2[i,]),0.75)
		
    w2[i,which(w2[i,]==0)]=NA
		
    support=(ncol(w2)-length(attr(na.omit(w2[i,]), "na.action")))/ncol(w2)
		
    w3[i,] = c(q25,q50,q75,support,abs.w)

	}

	w3[,"abs.w"] = abs(w3[,"q50"])/sum(abs(w3[,"q50"]))
	features.w   = w3

	model.perf=list(AUC=AUC, tpr=tpr, fpr=fpr, features.w=features.w)

	return(model.perf)

}



model.ext.validation=function(res.ext, nboot=10){

	perf=vector("list",nboot)
	
	for(b in 1:nboot) {
	# calculate performance
		
		pre = prediction(prediction=res.ext[[b]]$pred$probabilities+(rnorm(length(res.ext[[b]]$pred$probabilities))/(10^9)), labels=res.ext[[b]]$pred$actual) #add some artificial noise to fix a bug
		per = performance(pre,"tpr","fpr")
		AUC = (performance(pre,"auc"))@y.values[[1]]
		

		fpr = per@"x.values"[[1]]
		tpr = per@"y.values"[[1]]

		perf[[b]] = list(AUC=AUC,fpr=fpr,tpr=tpr)
		
		
	}

	AUC = unlist(lapply(perf, function(x){x$AUC}))

	tpr = sapply(perf, function(x){x$tpr})
	fpr = sapply(perf, function(x){x$fpr})


	model.perf = list(AUC=AUC, tpr=tpr, fpr=fpr)

	return(model.perf)

}


plot.lasso_roc=function(x, explore=FALSE, main="ROC curve", ...) {

	if(!explore) {
		
		plot(x$fpr.med, x$tpr.med,main=main,ylab="True Positive Rate",xlab="False Positive Rate",sub=paste("AUC:",round(median(x$AUC),2)), type="n")
		lines(x$fpr.lci, x$tpr.lci, lty=2)
		lines(x$fpr.med, x$tpr.med, lwd=2)
		lines(x$fpr.hci, x$tpr.hci, lty=2)
	} else {
		
		par(mfrow=c(1,4))
		plot((1:nrow(x$model.perf$features.w))/nrow(model.perf$features.w),cumsum(x$model.perf$features.w[rev(order(x$model.perf$features.w[,5])),5]), 
type="l", ylab="total cummulative weight in the model", xlab="relative rank species", main=main)
		abline(h=0.5, lty=2)
		abline(v=length(x$selected.species)/nrow(x$model.perf$features.w), lty=2)
		legend("bottomright", legend=paste(length(x$selected.species),"(",round(length(x$selected.species)/nrow(x$model.perf$features.w)*100,2),"%)","species account for\n ~50% of the model"), bty="n")
		barplot(x$model.perf$features.w[order(x$model.perf$features.w[,5]),2][cumsum(x$model.perf$features.w[order(x$model.perf$features.w[,5]),5]) > 0.50], horiz=T, las=2, xlim=c(-1,1), border=0, cex.names=0.5, main="Median log odds of features") #median log odds
		barplot(x$model.perf$features.w[order(x$model.perf$features.w[,5]),5][cumsum(x$model.perf$features.w[order(x$model.perf$features.w[,5]),5]) > 0.50]*100, horiz=T, las=2, border=0, cex.names=0.5, main="features % absolute weight in the model") #absolute weight in the model
		barplot(x$model.perf$features.w[order(x$model.perf$features.w[,5]),4][cumsum(x$model.perf$features.w[order(x$model.perf$features.w[,5]),5]) > 0.50]*100, horiz=T, las=2, xlim=c(0,100), border=0, cex.names=0.5, main="Features % Jakknife support") #Jacknife support
		par(mfrow=c(1,1))

	}



}


external_validation=function(res, dd, lab) {

	nboot   = length(res)
	ncv     = length(res[[1]]$model)
	res.ext = vector( "list", nboot*ncv)
	lab     = lab

	xTrain = dd.ext = dd
	xTrain = t(t(scale(xTrain, scale=FALSE))/(apply(xTrain,2,sd)+quantile(apply(xTrain,2,sd),0.1))) # add 10th sd
	xTrain[,attr(na.omit(t(xTrain)), "na.action")] = 0
	dd.ext = xTrain		
			
			
			

	for(i in 1:nboot){

		res.tmp = lapply(res[[i]]$model ,function(x) {predict(x, dd.ext, prob=TRUE)})

		for(j in 1:ncv){
			res.ext[[((i-1)*10)+j]]$pred = data.frame(prediction    = res.tmp[[j]]$prediction,
													  actual        = lab,
													  probabilities = res.tmp[[j]]$probabilities[,1])
		}

	}



	model.perf.ext = model.ext.validation(res.ext, nboot = 100)


	return(model.perf.ext)

}
