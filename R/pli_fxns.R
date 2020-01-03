#### Functions for continuous power law or Pareto distributions
# Revision history at end of file

### Standard R-type functions for distributions:
# dpareto		Probability density
# ppareto		Probability distribution (CDF)
# qpareto		Quantile function
# rpareto		Random variable generation
### Functions for fitting:
# pareto.fit			Fit Pareto to data
# pareto.fit.ml			Fit Pareto to data by maximum likelihood
#                               --- not for direct use, call pareto.fit instead
# pareto.loglike		Calculate log-likelihood under Pareto
# pareto.fit.regression.cdf	Fit Pareto data by linear regression on
#				log-log CDF (disrecommended)
#                               --- not for direct use, call pareto.fit instead
# loglogslope			Fit Pareto via regression, extract scaling
#				exponent
# loglogrsq			Fit Pareto via regression, extract R^2
### Functions for visualization:
# plot.eucdf.loglog		Log-log plot of the empirical upper cumulative
#				distribution function, AKA survival function
# plot.survival.loglog		Alias for plot.eucdf.loglog
### Back-stage functions, not intended for users:
# unique_values			Find the indices representing unique values
#				in a sorted list (used in regression fit)

# Probability density of Pareto distributions
# Gives NA on values below the threshold
# Input: Data vector, lower threshold, scaling exponent, "log" flag
# Output: Vector of (log) probability densities
dpareto <- function(x, threshold = 1, exponent, log=FALSE) {
  # Avoid doing limited-precision arithmetic followed by logs if we want
  # the log!
  if (!log) {
    prefactor <- (exponent-1)/threshold
    f <- function(x) {prefactor*(x/threshold)^(-exponent)}
  } else {
    prefactor.log <- log(exponent-1) - log(threshold)
    f <- function(x) {prefactor.log -exponent*(log(x) - log(threshold))}
  }
  d <- ifelse(x<threshold,NA,f(x))
  return(d)
}

# Cumulative distribution function of the Pareto distributions
# Gives NA on values < threshold
# Input: Data vector, lower threshold, scaling exponent, usual flags
# Output: Vector of (log) probabilities
ppareto <- function(x, threshold=1, exponent, lower.tail=TRUE, log.p=FALSE) {
  if ((!lower.tail) && (!log.p)) {
    f <- function(x) {(x/threshold)^(1-exponent)}
  }
  if ((lower.tail) && (!log.p)) {
    f <- function(x) { 1 - (x/threshold)^(1-exponent)}
  }
  if ((!lower.tail) && (log.p)) {
    f <- function(x) {(1-exponent)*(log(x) - log(threshold))}
  }
  if ((lower.tail) && (log.p)) {
    f <- function(x) {log(1 - (x/threshold)^(1-exponent))}
  }
  p <- ifelse(x < threshold, NA, f(x))
  return(p)
}

# Quantiles of Pareto distributions
# Input: vector of probabilities, lower threshold, scaling exponent, usual flags
# Output: Vector of quantile values
qpareto <- function(p, threshold=1, exponent, lower.tail=TRUE, log.p=FALSE) {
  # Quantile function for Pareto distribution
  # P(x) = 1 - (x/xmin)^(1-a)
  # 1-p = (x(p)/xmin)^(1-a)
  # (1-p)^(1/(1-a)) = x(p)/xmin
  # xmin*((1-p)^(1/(1-a))) = x(p)
  # Upper quantile:
  # U(x) = (x/xmin)^(1-a)
  # u^(1/(1-a)) = x/xmin
  # xmin * u^(1/(1-a)) = x
  # log(xmin) + (1/(1-a)) log(u) = log(x)
  if (log.p) {
    p <- exp(p)
  }
  if (lower.tail) {
    p <- 1-p
  }
  # This works, via the recycling rule
  # q<-(p^(1/(1-exponent)))*threshold
  q.log <- log(threshold) + (1/(1-exponent))*log(p)
  q <- exp(q.log)
  return(q)
}

# Generate Pareto-distributed random variates
# Input: Integer size, lower threshold, scaling exponent
# Output: Vector of real-valued random variates
rpareto <- function(n, threshold=1, exponent) {
  # Using the transformation method, because we know the quantile function
  # analytically
  # Consider replacing with a non-R implementation of transformation method
  ru <- runif(n)
  r<-qpareto(ru,threshold,exponent)
  return(r)
}

# Estimate scaling exponent of Pareto distribution
# A wrapper for functions implementing actual methods
# Input: data vector, lower threshold, method (likelihood or regression,
#        defaulting to former)
# Output: List indicating type of distribution ("exponent"), parameters,
#         information about fit (depending on method), OR a warning and NA
#         if method is not recognized
pareto.fit <- function(data, threshold, method="ml") {
  switch(method,
    ml = { return(pareto.fit.ml(data,threshold)) },
    regression.cdf = { return(pareto.fit.regression.cdf(data,threshold)) },
    { cat("Unknown method\n"); return(NA)}
  )
}

# Estimate scaling exponent of Pareto distribution by maximum likelihood
# Input: Data vector, lower threshold
# Output: List giving distribution type ("pareto"), parameters, log-likelihood
pareto.fit.ml <- function (data, threshold) {
  data <- data[data>threshold]
  n <- length(data)
  x <- data/threshold
  alpha <- 1 + n/sum(log(x))
  loglike = pareto.loglike(data,threshold,alpha)
  fit <- list(type="pareto", exponent=alpha, xmin=threshold, loglike = loglike)
  return(fit)
}

# Calculate log-likelihood under a Pareto distribution
# Input: Data vector, lower threshold, scaling exponent
# Output: Real-valued log-likelihood
pareto.loglike <- function(x, threshold, exponent) {
  L <- sum(dpareto(x, threshold = threshold, exponent = exponent, log = TRUE))
  return(L)
}

# Log-log plot of the survival function (empirical upper CDF) of a data set
# Input: Data vector, lower limit, upper limit, graphics parameters
# Output: None (returns NULL invisibly)
plot.survival.loglog <- function(x,from=min(x),to=max(x),...) {
	plot.eucdf.loglog(x,from,to,...)
}
plot.eucdf.loglog <- function(x,from=min(x),to=max(x),...) {
	# Exploit built-in R function to get ordinary (lower) ECDF, Pr(X<=x)
	x.ecdf <- ecdf(x)
	# Now we want Pr(X>=x) = (1-Pr(X<=x)) + Pr(X==x)
        # If x is one of the "knots" of the step function, i.e., a point with
	# positive probability mass, should add that in to get Pr(X>=x)
	# rather than Pr(X>x)
	away.from.knot <- function(y) { 1 - x.ecdf(y) }
	at.knot.prob.jump <- function(y) {
		x.knots = knots(x.ecdf)
		# Either get the knot number, or give zero if this was called
		# away from a knot
		k <- match(y,x.knots,nomatch=0)
		if ((k==0) || (k==1)) { # Handle special cases
			if (k==0) {
				prob.jump = 0 # Not really a knot
			} else {
				prob.jump = x.ecdf(y) # Special handling of first knot
			}
		} else {
			prob.jump = x.ecdf(y) - x.ecdf(x.knots[(k-1)]) # General case
		}
		return(prob.jump)
	}
	# Use one function or the other
	x.eucdf <- function(y) {
		baseline = away.from.knot(y)
		jumps = sapply(y,at.knot.prob.jump)
		ifelse (y %in% knots(x.ecdf), baseline+jumps, baseline)
	}
	plot(x.eucdf,from=from,to=to,log="xy",...)
	invisible(NULL)
}


### The crappy linear regression way to fit a power law
# The common procedure is to fit to the binned density function, which is even
# crappier than to fit to the complementary distribution function; this
# currently only implements the latter

# First, produce the empirical complementary distribution function, as
# a pair of lists, {x}, {C(x)}
# Then regress log(C) ~ log(x)
# and report the slope and the R^2
# Input: Data vector, threshold
# Output: List with distributional parameters and information about the
#         fit
pareto.fit.regression.cdf <- function(x,threshold=1) {
  x <- x[x>threshold]
  n <- length(x)
  x <- sort(x)
  uniqs <- unique_values(x)
  distinct_x <- x[uniqs]
  upper_probs <- ((n+1-uniqs))/n
  log_distinct_x <- log(distinct_x)
  log_upper_probs <- log(upper_probs)
  # so if one unique index was n, this would give prob 1/n there, and if one
  # unique index is 1 (which it should always be!), it will give prob 1 there
  loglogfit <- lm(log_upper_probs ~ log_distinct_x)
  intercept <- coef(loglogfit)[1] # primarily useful for plotting purposes
  slope <- -coef(loglogfit)[2] # Remember sign of parameterization
  # But that's the exponent of the CDF, that of the pdf is one larger
  # and is what we're parameterizing by
  slope <- slope+1
  r2 <- summary(loglogfit)$r.squared
  loglike <- pareto.loglike(x, threshold, slope)
  result <- list(type="pareto", exponent = slope, rsquare = r2,
                 log_x = log_distinct_x, log_p = log_upper_probs,
                 intercept = intercept, loglike = loglike, xmin=threshold)
  return(result)
}

# Wrapper function to just get the exponent estimate
loglogslope <- function(x,threshold=1) {
  llf <- pareto.fit.regression.cdf(x,threshold)
  exponent <- llf$exponent
  return(exponent)
}

# Wrapper function to just get the R^2 values
loglogrsq <- function(x,threshold=1) {
  llf <- pareto.fit.regression.cdf(x,threshold)
  r2 <- llf$rsquare
  return(r2)
}

# Function to take a sorted list of values, and return only the indices
# to unique values
# Called in finding the empirical complementary distribution function
# If a value is unique, return its index
# If a value is repeated, return its lowest index --- this is intended
# for working with the empirical complementary distribution function
# Input: a SORTED list of (real) numbers
# Output: a list of the indices of the input which mark distinct values
unique_values <- function(a_sorted_list) {
    # See which members of the list are strictly less than their successor
    n <- length(a_sorted_list)
    is_lesser <- a_sorted_list[2:n] > a_sorted_list[1:(n-1)]
    # convert to index numbers
    most_indices <- 1:(n-1)
    my_indices <- most_indices[is_lesser]
    # Remember that we've checked a list shortened by one from the start
    my_indices <- my_indices+1
    # Remember that the first item in the list has to be included
    my_indices <- c(1,my_indices)
    return(my_indices)
}


# Revision history:
# no release	2003		First draft
# v 0.0		2007-06-04	First release
# v 0.0.1	2007-06-29	Fixed "not" for "knot" typo, thanks to
#				Nicholas A. Povak for bug report
# v 0.0.2	2007-07-22	Fixed bugs in plot.survival.loglog, thanks to
#						Stefan Wehrli for the report


# Functions for estimation of an exponential distribution

# The use of an exponential to model values above a specified
# lower threshold can be done in one of two ways.  The first is simply to
# shift the standard exponential, i.e, to say that x-threshold ~ exp.
# The other is to say that Pr(X|X>threshold) = exp(X|X>threshold), i.e.,
# that the right tail follows the same functional form as the right tail of an
# exponential, without necessarily having the same probability of being in the
# tail.
# These will be called the "shift" and "tail" methods respectively.
# The shift method is, computationally, infinitely easier, but not so suitable
# for our purposes.

# The basic R system provides dexp (density), pexp (cumulative distribution),
# qexp (quantiles) and rexp (random variate generation)

### Functions for fitting:
# exp.fit		Fit exponential to data via likelihood maximization,
#			with choice of methods
### Backstage functions, not intended for users:
# exp.fit.tail		Fit exponential via "tail" method (default)
# exp.fit.moment	Fit exponential via "shift" method, starting with
#                       appropriate moments
# exp.loglike.shift	Calculate log likelihood of shifted exponential
# exp.loglike.tail	Calculate log likelihood of tail-conditional exponential

# Fit exponential distribution to data
# A wrapper for actual methods, defaulting to the "tail" method
exp.fit <- function(x,threshold=0,method="tail") {
  switch(method,
    tail = { fit <- exp.fit.tail(x,threshold) },
    shift = { fit <- exp.fit.shift(x,threshold) },
    {
       cat("Unknown method in exp.fit\n")
       fit <- NA}
  )
  return(fit)
}

exp.fit.tail <- function(x,threshold = 0) {
  # Start with a global estimate of the parameter
  lambda_0 <- exp.fit.moment(x,method="tail")$rate
  x <- x[x>threshold]
  # The function just called ignores values of method other than "shift"
  # but let's not take chances!
  negloglike <- function(lambda) { -exp.loglike.tail(x,lambda,threshold) }
  fit <-nlm(f=negloglike,p=lambda_0)
  list(type="exp", rate=fit$estimate, loglike=-fit$minimum, datapoints.over.threshold=length(x))
}

exp.fit.moment <- function(x, threshold = 0, method="shift") {
  x <- x[x>threshold]
  if (method=="shift") { x <- x-threshold }
  lambda <- 1/mean(x)
  loglike <- exp.loglike.shift(x, lambda, threshold)
  list(type="exp", rate=lambda, loglike=loglike, datapoints.over.threshold=length(x))
}

exp.loglike.shift <- function(x, lambda, threshold=0) {
  # Assumes (X-threshold) is exponentially distributed
  # See Johnson and Kotz, ch. 18 for more on this form of the distribution
  x <- x[x>threshold]
  x <- x-threshold
  sum(dexp(x,rate=lambda,log=TRUE))
}

exp.loglike.tail <- function(x, lambda, threshold=0) {
  # We want p(X=x|X>=threshold) = p(X=x)/Pr(X>=threshold)
  x <- x[x>threshold]
  n <- length(x)
  Like <- exp.loglike.shift(x,lambda)
  ThresholdProb <- pexp(threshold, rate=lambda, log=TRUE, lower.tail=FALSE)
  Like - n*ThresholdProb
}
# Functions for estimation of lognormal distributions

# The use of a log-normal distribution to model values above a specified
# lower threshold can be done in one of two ways.  The first is simply to
# shift the standard log-normal, i.e, to say that x-threshold ~ lnorm.
# The other is to say that Pr(X|X>threshold) = lnorm(X|X>threshold), i.e.,
# that the right tail follows the same functional form as the right tail of a
# lognormal, without necessarily having the same probability of being in the
# tail.
# These will be called the "shift" and "tail" methods respectively.
# The shift method is, computationally, infinitely easier, but not so suitable
# for our purposes.

# The basic R system provides dlnorm (density), plnorm (cumulative
# distribution), qlnorm (quantiles) and rlnorm (random variate generation)

### Function for fitting:
# lnorm.fit		Fit log-normal to data, with choice of methods
### Back-stage functions not intended for users:
# lnorm.fit.max		Fit log-normal by maximizing likelihood ("tail")
# lnorm.fit.moments	Fit log-normal by moments of log-data ("shift")
# lnorm.loglike.tail	Tail-conditional log-likelihood of log-normal
# lnorm.loglike.shift	Log-likelihood of shifted log-normal
### Tail distribution functions (per R standards):
# dlnorm.tail		Tail-conditional probability density
# plnorm.tail		Tail-conditional cumulative probability


# Fit log-normal to data over threshold
# Wrapper for the shift (log-moment) or tail-conditional (maximizer) functions
# Input: Data vector, lower threshold, method flag
# Output: List giving distribution type ("lnorm"), parameters, log-likelihood
lnorm.fit <- function(x,threshold=0,method="tail") {
  switch(method,
   tail = {
    if (threshold>0) { fit <- lnorm.fit.max(x,threshold) }
    else { fit <- lnorm.fit.moments(x) }
  },
  shift = {
    fit <- lnorm.fit.moments(x,threshold)
  },
  {
    cat("Unknown method\n")
    fit <- NA
  })
  return(fit)
}

# Fit log-normal by direct maximization of tail-conditional log-likelihood
# Note that direct maximization of the shifted lnorm IS lnorm.fit.moments, so
# that should be used instead for numerical-accuracy reasons
# Input: Data vector, lower threshold
# Output: List giving distribution type ("lnorm"), parameters, log-likelihood
lnorm.fit.max <- function(x, threshold = 0) {
  # Use moment-based estimator on whole data as starting point
  initial.fit <- lnorm.fit.moments(x)
  theta_0 <- c(initial.fit$meanlog,initial.fit$sdlog)
  x <- x[x>threshold]
  negloglike <- function(theta) {
    -lnorm.loglike.tail(x,theta[1],theta[2],threshold)
  }
  est <- nlm(f=negloglike,p=theta_0)
  fit <- list(type="lnorm",meanlog=est$estimate[1],sdlog=est$estimate[2],
              datapoints.over.threshold = length(x), loglike=-est$minimum)
  return(fit)
}

# Fit log-normal via moments of the log data
# This is the maximum likelihood solution for the shifted lnorm
# Input: Data vector, lower threshold
# Output: List giving distribution type ("lnorm"), parameters, log-likelihood
lnorm.fit.moments <- function(x, threshold = 0) {
  x <- x[x>threshold]
  x <- x-threshold
  LogData <- log(x)
  M = mean(LogData)
  SD = sd(LogData)
  Lambda = lnorm.loglike.shift(x,M,SD,threshold)
  fit <- list(type="lnorm", meanlog=M, sdlog=SD, loglike=Lambda)
  return(fit)
}

# Tail-conditional log-likelihood of log-normal
# Input: Data vector, distributional parameters, lower threshold
# Output: Real-valued log-likelihood
lnorm.loglike.tail <- function(x, mlog, sdlog, threshold = 0) {
  # Compute log likelihood of data under assumption that the generating
  # distribution is a log-normal with the given parameters, and that we
  # are only looking at the tail values, x >= threshold
  # We want p(X=x|X>=threshold) = p(X=x)/Pr(X>=threshold)
  x <- x[x> threshold]
  n <- length(x)
  Like <- lnorm.loglike.shift(x,mlog, sdlog)
  ThresholdProb <- plnorm(threshold, mlog, sdlog, log.p=TRUE, lower.tail=FALSE)
  L <- Like - n*ThresholdProb
  return(L)
}

# Loglikelihood of shifted log-normal
# Input: Data vector, distributional parameters, lower threshold
# Output: Real-valued log-likelihood
lnorm.loglike.shift <- function(x, mlog, sdlog, x0=0) {
  # Compute log likelihood under assumption that x-x0 is lognromally
  # distributed
  # This (see Johnson and Kotz) the usual way of combining a lognormal
  # distribution with a hard minimum value.  (They call the lower value theta)
  x <- x[x>x0]
  x <- x-x0
  L <- sum(dlnorm(x,mlog,sdlog,log=TRUE))
  return(L)
}


# Tail-conditional density function
# Returns NA if given values below the threshold
# Input: Data vector, distributional parameters, lower threshold, log flag
# Output: Vector of (log) probability densities
dlnorm.tail <- function(x, meanlog, sdlog, threshold=0,log=FALSE) {
  # Returns NAs at positions where the values in the input are < threshold
  if (log) {  
    f <- function(x) {dlnorm(x,meanlog,sdlog,log=TRUE) - plnorm(threshold,meanlog,sdlog,log=TRUE)}
  } else {
    f <- function(x) {dlnorm(x,meanlog,sdlog)/plnorm(threshold,meanlog,sdlog)}
  }
  d <- ifelse(x<threshold,NA,f(x))
  return(d)
}

# Tail-conditional cumulative distribution function
# Returns NA if given values below the threshold
# Input: Data vector, distributional parameters, lower threshold, usual flags
# Output: Vector of (log) probabilities
plnorm.tail <- function(x,meanlog,sdlog,threshold=0,lower.tail=TRUE,log.p=FALSE) {
  c <- plnorm(threshold,meanlog,sdlog,lower.tail=FALSE)
  c.log <- plnorm(threshold,meanlog,sdlog,lower.tail=FALSE,log.p=TRUE)
  if ((!lower.tail) && (!log.p)) {
    f <- function(x) {plnorm(x,meanlog,sdlog,lower.tail=FALSE)/c}
  }
  if ((lower.tail) && (!log.p)) {
    f <- function(x) {1 - plnorm(x,meanlog,sdlog,lower.tail=FALSE)/c}
  }
  if ((!lower.tail) && (log.p)) {
    f <- function(x) {plnorm(x,meanlog,sdlog,lower.tail=FALSE,log.p=TRUE) - c.log}
  }
  if ((lower.tail) && (log.p)) {
    f <- function(x) {log(1 - plnorm(x,meanlog,sdlog,lower.tail=FALSE)/c)}
  }
  p <- ifelse(x<threshold,NA,f(x))
  return(p)
}
# Code for testing fits to power-law distributions against other heavy-tailed
# alternatives

# Code for ESTIMATING the heavy-tailed alternatives live in separate files,
# which you'll need to load.

# Tests are of two sorts, depending on whether or not the power law is embeded
# within a strictly larger class of distributions ("nested models"), or the two
# alternatives do not intersect ("non-nested").  In both cases, our procedures
# are based on those of Vuong (1989), neglecting his third case where the two
# hypothesis classes intersect at a point, which happens to be the best-fitting
# hypothesis.

# In the nested case, the null hypothesis is that the best-fitting distribution
# lies in the smaller class.  Then 2* the log of the likelihood ratio should
# (asymptotically) have a chi-squared distribution.  This is almost the
# classical Wilks (1938) situation, except that Vuong shows the same results
# hold even when all the distributions are "mis-specified" (hence we speak of
# best-fitting distribution, not true distribution).

# In the non-nested case, the null hypothesis is that both classes of
# distributions are equally far (in the Kullback-Leibler divergence/relative
# entropy sense) from the true distribution.  If this is true, the
# log-likelihood ratio should (asymptotically) have a Gaussian distribution
# with mean zero; our test statistic is the sample average of the log
# likelihood ratio, standardized by a consistent estiamte of its standard
# deviation.  If the null is false, and one class of distributions is closer to
# the truth, his test statistic goes to +-infinity with probability 1,
# indicating the better-fitting class of distributions.  (See, in particular,
# Theorem 5.1 on p. 318 of his paper.)

# Vuong, Quang H. (1989): "Likelihood Ratio Tests for Model Selection and
#	Non-Nested Hypotheses", _Econometrica_ 57: 307--333 (in JSTOR)
# Wilks, S. S. (1938): "The Large Sample Distribution of the Likelihood Ratio
#	for Testing Composite Hypotheses", _The Annals of Mathematical
#	Statistics_ 9: 60--62 (in JSTOR)

# All testing functions here are set up to work with the estimation functions
# in the accompanying files, which return some meta-data about the fitted
# distributions, including likelihoods, cut-offs, numbers of data points, etc.,
# much of which is USED by the testing routines.

### Function for nested hypothesis testing:
# power.powerexp.lrt		Test power-law distribution vs. power-law with
#				exponential cut-off
### Functions for non-nested hypothesis testing:
# vuong				Calculate mean, standard deviation, Vuong's
#				test statistic, and Gaussian p-values on
#				log-likelihood ratio vectors
# pareto.exp.llr		Makes vector of pointwise log-likelihood
#				ratio between fitted continuous power law
#				(Pareto) and exponential distributions
# pareto.lnorm.llr		Pointwise log-likelihood ratio, Pareto vs.
#				log-normal
# pareto.weibull.llr		Pointwise log-likelihood ratio, Pareto vs.
#				stretched exponential (Weibull)
# zeta.exp.llr 			Pointwise log-likelihood ratio, discrete power
#				law (zeta) vs. discrete exponential
# zeta.lnorm.llr		Pointwise log-likelihood ratio, zeta vs.
#				discretized log-normal
# zeta.poisson.llr		Pointwise log-likelihood ratio, zeta vs. Poisson
# zeta.weib.llr			Pointwise log-likelihood ratio, zeta vs. Weibull
# zeta.yule.llr			Pointwise log-likelihood ratio, zeta vs. Yule



# Test power law distribution vs. a power law with an exponential cut-off
# This is meaningful ONLY if BOTH distributions are continuous or discrete,
# and, of course, both were estimated on the SAME data set, with the SAME
# cut-off
# TODO: Check whether the distributions are comparable!
# Input: fitted power law distribution, fitted powerexp distribution
# Output: List giving log likelihood ratio and chi-squared p-value
# Recommended: pareto.R, powerexp.R, zeta.R, discpowerexp.R
power.powerexp.lrt <- function(power.d,powerexp.d) {
  lr <- (power.d$loglike - powerexp.d$loglike)
  p <- pchisq(-2*lr,df=1,lower.tail=FALSE)
  Result <- list(log.like.ratio = lr, p_value = p)
  Result
}



# Apply Vuong's test for non-nested models to vector of log-likelihood ratios
# Sample usage:
#### vuong(pareto.lnorm.llr(wealth,wealth.pareto,wealth.lnorm))
# The inner function produces a vector, giving the log of the likelihood
# ratio at every point; the outer function then reduces this and calculates
# both the normalized log likelihood ratio and the p-values.
# Input: Vector giving, for each sample point, the log likelihood ratio of
#        the two models under consideration
# Output: List giving total log likelihood ratio, mean per point, standard
#         deviation per point, Vuong's test statistic (normalized pointwise
#	  log likelihood ratio), one-sided and two-sided p-values (based on
#	  asymptotical standard Gaussian distribution)
vuong <- function(x) {
	n <- length(x)
	R <- sum(x)
	m <- mean(x)
	s <- sd(x)
	v <- sqrt(n)*m/s
	p1 <- pnorm(v)
	if (p1 < 0.5) {p2 <- 2*p1} else {p2 <- 2*(1-p1)}
	list(loglike.ratio=R,mean.LLR = m, sd.LLR = s, Vuong=v, p.one.sided=p1, p.two.sided=p2)
}


# Pointwise log-likelihood ratio between continuous power law (Pareto) and
# exponential distributions
# Input: Data vector, fitted Pareto distribution, fitted exponential
#	 distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  Pareto's cut-off
# Requires: pareto.R
# Recommended: exp.R
pareto.exp.llr <- function(x,pareto.d,exp.d) {
	xmin <- pareto.d$xmin
	alpha <- pareto.d$exponent
	lambda <- exp.d$rate
	x <- x[x>=xmin]
	dpareto(x,threshold=xmin,exponent=alpha,log=TRUE) - dexp(x,lambda,log=TRUE) + pexp(xmin,lambda,lower.tail=FALSE,log.p=TRUE)
}


# Pointwise log-likelihood ratio between General Pareto and
# exponential distributions
# Input: Data vector, fitted General Pareto distribution, fitted exponential
#	 distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  General Pareto's cut-off
# Requires: pareto.R
# Recommended: exp.R

gpareto.exp.llr <- function(x, gpareto.d, exp.d) {
	xmin <- gpareto.d$xmin
	scale <- gpareto.d$scale
	shape <- gpareto.d$shape
	lambda <- exp.d$rate
	x <- x[x > xmin]
	dgpd(x, loc = xmin, scale = scale, shape = shape, log = TRUE) - dexp(x, lambda, log = TRUE) + pexp(xmin, lambda, lower.tail = FALSE, log.p = TRUE)
}

# Pointwise log-likelihood ratio between continuous power law (Pareto) and
# log-normal distributions
# Input: Data vector, fitted Pareto distribution, fitted lognormal distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  Pareto's cut-off
# Requires: pareto.R
# Recommended: lnorm.R
pareto.lnorm.llr <- function(x,pareto.d,lnorm.d) {
        xmin <- pareto.d$xmin
        alpha <- pareto.d$exponent
        m <- lnorm.d$meanlog
        s <- lnorm.d$sdlog
	x <- x[x>=xmin]
	dpareto(x,threshold=xmin,exponent=alpha,log=TRUE) - dlnorm(x,meanlog=m,sdlog=s,log=TRUE) + plnorm(xmin,meanlog=m,sdlog=s,lower.tail=FALSE,log.p=TRUE)
}


# Pointwise log-likelihood ratio between General Pareto and
# log-normal distributions
# Input: Data vector, fitted General Pareto distribution, fitted lognormal distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  General Pareto's cut-off
# Requires: pareto.R
# Recommended: lnorm.R
gpareto.lnorm.llr <- function(x,gpareto.d,lnorm.d) {
        xmin <- gpareto.d$xmin
        scale <- gpareto.d$scale
        shape <- gpareto.d$shape
        m <- lnorm.d$meanlog
        s <- lnorm.d$sdlog
	x <- x[x>xmin]
	dgpd(x,loc=xmin,scale=scale,shape=shape,log=TRUE) - dlnorm(x,meanlog=m,sdlog=s,log=TRUE) + plnorm(xmin,meanlog=m,sdlog=s,lower.tail=FALSE,log.p=TRUE)
}

lnorm.exp.llr <- function(xmin, x, lnorm.d, exp.d) {
	m <- lnorm.d$meanlog
	s <- lnorm.d$sdlog
	lambda <- exp.d$rate
	x <- x[x > xmin]
	dlnorm(x,meanlog=m,sdlog=s,log=TRUE) - dexp(x, lambda, log = TRUE) + pexp(xmin, lambda, lower.tail = FALSE, log.p = TRUE) 	
}


# Pointwise log-likelihood ratio between continuous power law (Pareto) and
# stretched exponential (Weibull) distributions
# Input: Data vector, fitted Pareto distribution, fitted Weibull distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  Pareto's cut-off
# Requires: pareto.R
# Recommended: weibull.R
pareto.weibull.llr <- function(x,pareto.d,weibull.d) {
	xmin <- pareto.d$xmin
	alpha <- pareto.d$exponent
	shape <- weibull.d$shape
	scale <- weibull.d$scale
	x <- x[x>=xmin]
	dpareto(x,threshold=xmin,exponent=alpha,log=TRUE) - dweibull(x,shape=shape,scale=scale,log=TRUE) + pweibull(xmin,shape=shape,scale=scale,lower.tail=FALSE,log.p=TRUE)
}

# Pointwise log-likelihood ratio between discrete power law (zeta) and discrete
# exponential distributions
# Input: Data vector, fitted zeta distribution, fitted discrete exponential
#	 distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  zeta's cut-off
# Requires: zeta.R, exp.R
zeta.exp.llr <- function(x,zeta.d,exp.d) {
  xmin <- zeta.d$threshold
  alpha <- zeta.d$exponent
  lambda <- exp.d$lambda
  x <- x[x>=xmin]
  dzeta(x,xmin,alpha,log=TRUE) - ddiscexp(x,lambda,xmin,log=TRUE)
}

# Pointwise log-likelihood ratio between discrete power law (zeta) and discrete
# log-normal distributions
# Input: Data vector, fitted zeta distribution, fitted discrete log-nromal
#	 distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  zeta's cut-off
# Requires: zeta.R, disclnorm.R
zeta.lnorm.llr <- function(x,zeta.d,lnorm.d) {
  xmin <- zeta.d$threshold
  alpha <- zeta.d$exponent
  meanlog <- lnorm.d$meanlog
  sdlog <- lnorm.d$sdlog
  x <- x[x>=xmin]
  dzeta(x,xmin,alpha,log=TRUE) - dlnorm.tail.disc(x,meanlog,sdlog,xmin,log=TRUE)
}

# Pointwise log-likelihood ratio between discrete power law (zeta) and Poisson
# distributions
# Input: Data vector, fitted zeta distribution, fitted Poisson distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  zeta's cut-off
# Requires: zeta.R, poisson.R
zeta.poisson.llr <- function(x,zeta.d,pois.d) {
  xmin <- zeta.d$threshold
  alpha <- zeta.d$exponent
  rate <- pois.d$rate
  x <- x[x>=xmin]
  dzeta(x,threshold=xmin,exponent=alpha,log=TRUE) - dpois.tail(x,threshold=xmin,rate=rate,log=TRUE)
}

# Pointwise log-likelihood ratio between discrete power law (zeta) and discrete
# stretched exponential (Weibull) distributions
# Input: Data vector, fitted zeta distribution, fitted discrete Weibull
#	 distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  zeta's cut-off
# Requires: zeta.R, discweib.R
zeta.weib.llr <- function(x,zeta.d,weib.d) {
   xmin <- zeta.d$threshold
   alpha <- zeta.d$exponent
   shape <- weib.d$shape
   scale <- weib.d$scale
   x <- x[x>=xmin]
   dzeta(x,xmin,alpha,log=TRUE) - ddiscweib(x,shape,scale,xmin,log=TRUE)
}

# Pointwise log-likelihood ratio between discrete power law (zeta) and Yule
# distributions
# Input: Data vector, fitted zeta distribution, fitted Yule distribution
# Output: Vector of pointwise log-likelihood ratios, ignoring points below the
# 	  zeta's cut-off
# Requires: zeta.R, yule.R
zeta.yule.llr <- function(x,zeta.d,yule.d) {
  xmin <- zeta.d$threshold
  alpha <- zeta.d$exponent
  beta <- yule.d$exponent
  x <- x[x>=xmin]
  dzeta(x,threshold=xmin,exponent=alpha,log=TRUE) - dyule(x,beta,xmin,log=TRUE)
}
