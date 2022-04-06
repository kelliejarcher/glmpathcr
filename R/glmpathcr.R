glmpathcr <-
function(x,y,data=NULL,method="backward",weight = rep(1, n), offset = rep(0, n), lambda2 = 1e-05, 
    max.steps = 10 * min(n, m), max.norm = 100 * m, min.lambda = (if (m >= 
        n) 1e-06 else 0), max.vars = Inf, max.arclength = Inf, 
    frac.arclength = 1, add.newvars = 1, bshoot.threshold = 0.1, 
    relax.lambda = 1e-08, standardize = TRUE, function.precision = 3e-13, 
    eps = .Machine$double.eps, trace = FALSE, nopenalty=NULL) {
    if (!is.null(data)) {
    	x<-data$x
    	y<-data$y
    }
    	n <- nrow(x)
	p<-m<-dim(x)[2]
	k<-length(unique(y))
	if (k==2) stop("Binary response: Use glmpath with family=binomial parameter")
	x<-as.matrix(x)
	if (c("backward","forward")[charmatch(method,c("backward","forward"))]=="backward") {
		restructure<-cr.backward(x = x, y = y, weight = weight)
	}
	if (c("backward","forward")[charmatch(method,c("backward","forward"))]=="forward") {
		restructure<-cr.forward(x = x, y = y, weight = weight)
	}		
   glmpath.data<-list(x=restructure[,-c(1,2)],y=restructure[,"y"])
   weight <- restructure[,"weight"]
    if (is.null(nopenalty)) {
        nopenalty.subset = (p +  1):(p + k - 1)
    } else {
        nopenalty.subset = c((p +  1):(p + k - 1),nopenalty)
    }

   object<-glmpath(x, y, weight=weight, data=glmpath.data, family=binomial, standardize=standardize, nopenalty.subset=nopenalty.subset,
         lambda2 = lambda2, max.steps = max.steps, max.norm = max.norm, min.lambda = min.lambda, max.vars = max.vars, max.arclength = max.arclength, 
	 frac.arclength = frac.arclength, add.newvars  = add.newvars, bshoot.threshold = bshoot.threshold, relax.lambda = relax.lambda, trace = trace)
   object$x<-x
   object$y<-y
   object$method<-method
   class(object)<-"glmpathcr"
   object
}

