
####################################################
###############  EXPRESSIONLEVEL  ##################
####################################################

# LowExpressionLevel.setValues(object, double vector)
.setValuesLow <- function(object, values) {
	#maxim <- max(values, na.rm=T); maxim
	#minim <- min(values, na.rm=T); minim
	rval <- mean(values, na.rm=TRUE); rval # Skipping NA
	Cm <- rval
	Cl <- mean(values[values<Cm], na.rm=TRUE); Cl
	object@center <- Cl
	#Ch <- mean(values[values>Cm], na.rm=T); Ch
	#Lm <- (Ch-Cl)/2; Lm
	Ll <- Cm-Cl; Ll
	#Lh <- Ch-Cm; Lh
	object@width <- Ll
	#object@part1 = 1 / (object@width * sqrt(2*pi)) #; part1
	#object@part2 = 2 * object@width^2 #; part2
	#object@part3 = object@width / 2 #; part3
	return(object)
}

# LowExpressionLevel.computeMembership(double)
.computeMembershipLow <- function(object, x) {
  y <- NULL
	centerDist = x - object@center
  for(i in 1:length(x)) {
  	if (centerDist[i] <= 0) {
      val <- 1
    } else if ((0 < centerDist[i]) & (centerDist[i] <= (object@width / 2))) {
  	        temp1 = centerDist[i] / object@width
  	        val <- (1 - (2 * (temp1 * temp1)))
  	} else if (((object@width / 2) <= centerDist[i]) & (centerDist[i] <= object@width)) {
  	        temp1 = 1 - ((x[i] - object@center) / object@width)
  	        val <- (2 * (temp1 * temp1))
  	} else {
  	 val <- 0
    }
    y <- c(y,val)
  }
	return (y)
}

# MediumExpressionLevel.setValues(object, double vector)
.setValuesMedium <- function(object, values) {
	#maxim <- max(values, na.rm=T); maxim
	#minim <- min(values, na.rm=T); minim
	rval <- mean(values, na.rm=TRUE); rval # Skipping NA
	Cm <- rval
	object@center <- Cm
	Cl <- mean(values[values<Cm], na.rm=TRUE); Cl
	Ch <- mean(values[values>Cm], na.rm=TRUE); Ch
	Lm <- (Ch-Cl)/2; Lm
	#Ll <- Cm-Cl; Ll
	#Lh <- Ch-Cm; Lh
	object@width <- Lm
	#object@part1 = 1 / (object@width * sqrt(2*pi)) #; part1
	#object@part2 = 2 * (object@width^2) #; part2
	#object@part3 = object@width / 2 #; part3
	return(object)
}

# MediumExpressionLevel.computeMembership(object, double)
.computeMembershipMedium <- function(object, x) {
  y <- NULL
	centerDist = abs(x - object@center)
  for(i in 1:length(x)) {
  	if ( (0 <= centerDist[i]) & (centerDist[i] <= (object@width/2)) ) { # TODO: (0<=centerDist) always TRUE?
  	        temp1 = centerDist[i] / object@width
  	        val <- (1 - (2 * (temp1 * temp1)))
  	} else if (((object@width/2) <= centerDist[i]) & (centerDist[i] <= object@width)) {
  	        temp1 = 1 - (centerDist[i] / object@width)
  	        val <- (2 * (temp1 * temp1))
  	} else {
  		val <- 0
  	}
    y <- c(y,val)
	}
	return (y)
}

# HighExpressionLevel.setValues(object, double vector)
.setValuesHigh <- function(object, values) {
	#maxim <- max(values, na.rm=T); maxim
	#minim <- min(values, na.rm=T); minim
	rval <- mean(values, na.rm=TRUE); rval # Skipping NA
	Cm <- rval
	#Cl <- mean(values[values<Cm], na.rm=T); Cl
	Ch <- mean(values[values>Cm], na.rm=TRUE); Ch
	object@center <- Ch
	#Lm <- (Ch-Cl)/2; Lm
	#Ll <- Cm-Cl; Ll
	Lh <- Ch-Cm; Lh
	object@width <- Lh
	#object@part1 = 1 / (object@width * sqrt(2*pi)) #; part1
	#object@part2 = 2 * (object@width^2) #; part2
	#object@part3 = object@width / 2 #; part3
	return(object)
}

# HighExpressionLevel.computeMembership(object, double)
.computeMembershipHigh <- function(object, x) {
  y <- NULL
	centerDist = x - object@center
  for(i in 1:length(x)) {
  	if (centerDist[i] >= 0) {
      val <- 1
    } else if ( (((-1)*(object@width / 2)) <= centerDist[i]) & (centerDist[i] <= 0) ) {
  		temp1 = centerDist[i] / object@width
  		val <- (1 - (2 * (temp1 * temp1)))
  	} else if ( (((-1)*(object@width)) <= centerDist[i]) & (centerDist[i] <= (-1)*(object@width / 2)) ) { # (object@width/2) = part3
  		temp1 = 1 + ((x[i] - object@center) / object@width)
  		val <- (2*(temp1*temp1))
  	} else {
      val <- 0
    }
    y <- c(y,val)
	}
	return (y)
}

# Virtual Class ExpressionLevel
#setGeneric("setValues", function(object, values) standardGeneric("setValues"))
#setGeneric("computeMembership", function(object,x) standardGeneric("computeMembership"))

#setClass("ExpressionLevel", representation(center="numeric",width="numeric", "VIRTUAL"))#, where=where

# Class LowExpressionLevel
#setClass("LowExpressionLevel", contains="ExpressionLevel")
#setMethod("setValues","LowExpressionLevel",.setValuesLow)
#setMethod("computeMembership","LowExpressionLevel",.computeMembershipLow)

# Class MediumExpressionLevel
#setClass("MediumExpressionLevel", contains="ExpressionLevel")
#setMethod("setValues","MediumExpressionLevel",.setValuesMedium)
#setMethod("computeMembership","MediumExpressionLevel",.computeMembershipMedium)

# Class HighExpressionLevel
#setClass("HighExpressionLevel", contains="ExpressionLevel")
#setMethod("setValues","HighExpressionLevel",.setValuesHigh)
#setMethod("computeMembership","HighExpressionLevel",.computeMembershipHigh)

#setMethod("show","ExpressionLevel",function(object){
#	cat("Class Type:", class(object), "\n")
#	cat("Center:", object@center, "\n")
#	cat("Width:", object@width, "\n")
#})

####################################################
###############  EXPRESSIONLEVEL  ##################
####################################################




# Read a comma separated CSV file into an ExpressionSet object
readCSV <- function(fileExprs, filePhenodata) {

  exprsFile <- NULL
  if(file.exists(fileExprs)) {
  	exprsFile <- fileExprs
  } else {
  	if(file.exists(file.path(system.file("extdata", package="DFP"), fileExprs))) {
  		exprsFile <- file.path(system.file("extdata", package="DFP"), fileExprs)
  	}
  }
  
  pDataFile <- NULL
  if(file.exists(filePhenodata)) {
  	pDataFile <- filePhenodata
  } else {
  	if(file.exists(file.path(system.file("extdata", package="DFP"), filePhenodata))) {
  		pDataFile <- file.path(system.file("extdata", package="DFP"), filePhenodata)
  	}
  }

  if(is.null(exprsFile) | is.null(pDataFile)) {
  	if(is.null(exprsFile))
  		paste("ERROR: Expression file '",fileExprs,"' not found.", sep="")
  	if(is.null(pDataFile))
  		paste("ERROR: Phenotypic file '",filePhenodata,"' not found.", sep="")
  
  } else {
  	#exprsFile <- "c:/path/to/exprsData.txt"
  	exprs <- as.matrix(read.table(exprsFile, header=TRUE, sep=",", 
  		comment.char="#", row.names=2, as.is=TRUE)[,(-1)]); head(exprs)
  	colnames(exprs)
  	pData <- read.table(pDataFile, row.names = 1, header = TRUE,
  		sep = ","); pData
  	if( !all(rownames(pData) == colnames(exprs)) )
  		colnames(exprs) <- rownames(pData)
  	metadata <- data.frame(labelDescription = c("Disease type",
  		"Patient age", "Patient gender"),
  		row.names = c("class", "age", "sex")); metadata
  	library(Biobase)
  	phenoData <- new("AnnotatedDataFrame", data = pData, 
  		varMetadata = metadata); phenoData
  	exampleSet <- new("ExpressionSet", exprs = exprs, phenoData = phenoData); exampleSet
  	return(exampleSet)
  }
}







####################################################
#################      DFP      ####################
####################################################

discriminantFuzzyPattern <- function(rmadataset, skipFactor=3, zeta=0.5, overlapping=2, piVal=0.9) {
	# Extract data from ExpressionSet
	rmam <- exprs(rmadataset); rmam[c(1:8),c(1:4)]
	rmav <- as.vector(pData(phenoData(rmadataset))$class); rmav
	names(rmav) <- sampleNames(rmadataset); rmav
	rmaf <- factor(rmav, levels=unique(rmav)); rmaf
	gene.names <- rownames(rmam); gene.names

	if(overlapping == 1) {
		disc.alphab <- c("Low", "Medium", "High")
	} else if(overlapping == 2) {
		disc.alphab <- c("Low", "Low-Medium", "Medium", "Medium-High", "High")
	} else {
		disc.alphab <- c("Low", "Low-Medium", "Low-Medium-High", "Medium", "Medium-High", "High")
	}
	params <- list("skipFactor"=skipFactor, "zeta"=zeta, "piVal"=piVal,
		"overlapping"=overlapping, "disc.alphab"=disc.alphab); params

	lel <- new("LowExpressionLevel"); lel
	mel <- new("MediumExpressionLevel"); mel
	hel <- new("HighExpressionLevel"); hel

	discriminants <- NULL
	mfs <- list()
	fps <- NULL
	ifs <- NULL
	dvs <- NULL
	
	# Progress bar
	#p <- 0; i <- 0;	l <- length(gene.names)/100
	# Progress bar
	for (ig in gene.names) {
		# Progress bar
		#i <- i+1; if(i>=l) {i <- 0; p <- p+1;cat(p, "% completado\n")}
		# Progress bar
		values <- rmam[ig,]; values
	
		# Skip odd values
		outliers <- .skipOddValues(values, skipFactor); outliers
		notoutliers <- values[!outliers]; notoutliers
	
		lel <- .setValuesLow(lel, notoutliers); lel
		mel <- .setValuesMedium(mel, notoutliers); mel
		hel <- .setValuesHigh(hel, notoutliers); hel
	
		# List of Membership Functions (list of 3 objects for each gene)	
		mfs[[ig]] <- list(lel=lel, mel=mel, hel=hel); mfs

		# Creates a matrix with the discrete labels
		disc.values <- .fuzzyDiscretization(lel, mel, hel, values, zeta, overlapping); disc.values
		dvs <- rbind(dvs, disc.values); dvs

		# Assign a label (from 'disc.alphab') for each 'type' depending on de 'piVal'
		attr(disc.values, "types") <- rmav; disc.values
		fuzzypat <- .fuzzyPatterns(disc.values, disc.alphab, piVal); fuzzypat
	
		# Matrices: Fuzzy Patterns for each disease type and gene (impact factors and fuzzy patterns)
		fps <- rbind(fps, fuzzypat); fps
		ifs <- rbind(ifs, attr(fuzzypat, "ifs")); ifs
		#fps[[ig]] <- fuzzypat; fps
	
		# Test if there is a Discriminant Fuzzy Pattern in the current gene	
		table.facFP <- table(factor(fuzzypat, levels=disc.alphab)); table.facFP
		max.facFP <- max(table.facFP); max.facFP
		#if( max(table(facFP)) > 0 & max(table(facFP)) < sum(table(facFP)) )
		if( max.facFP > 0 & max.facFP < sum(table.facFP) ) {
			discriminants <- c(discriminants, ig)
		}

	} # End for

	rownames(dvs) <- gene.names; head(dvs)
	attr(dvs, "types") <- rmav; dvs

	rownames(fps) <- gene.names; head(fps)
	rownames(ifs) <- gene.names; head(ifs)
	attr(fps, "ifs") <- ifs; head(fps)
	dfp <- fps[discriminants,]; dfp
	attr(dfp,"ifs") <- ifs[discriminants,]; dfp

	res <- list(membership.functions=mfs, discrete.values=dvs, fuzzy.patterns=fps,
		discriminant.fuzzy.pattern=dfp, params=params)
	return(res)
}


# Skip odd values
.skipOddValues <- function(values, skipFactor=3) {
	# If skipFactor==0 do NOT skip
	if(skipFactor>0) {
  	orderv <- order(values); orderv
  	vals <- values[orderv]; vals # Sort vector
  	first <- trunc(length(vals)/4); first
  	#medium <- trunc(length(vals)/2); medium
  	third <- trunc(length(vals)/4)*3; third
  	firstValue <- vals[first+1]; firstValue
  	thirdValue <- vals[third+1]; thirdValue
  	RIC <- thirdValue-firstValue; RIC
  	lowBarrier <- firstValue-(skipFactor*RIC); lowBarrier
  	highBarrier <- thirdValue+(skipFactor*RIC); highBarrier
  	isOutlier <- values<lowBarrier | values>highBarrier; isOutlier # Condition to be skipped
  	#values <- values[!isOutlier]; values
	}
	return(isOutlier)
}


calculateMembershipFunctions <- function(rmadataset, skipFactor=3) {
	rmam <- exprs(rmadataset); rmam[c(1:8),c(1:4)]
	rmav <- as.vector(pData(phenoData(rmadataset))$class); rmav
	names(rmav) <- sampleNames(rmadataset); rmav
	gene.names <- rownames(rmam); gene.names
	
	lel <- new("LowExpressionLevel"); lel
	mel <- new("MediumExpressionLevel"); mel
	hel <- new("HighExpressionLevel"); hel
	mfs <- list()
	
	for (ig in gene.names) {
		values <- rmam[ig,]; values
		outliers <- .skipOddValues(values, skipFactor); outliers
		notoutliers <- values[!outliers]; notoutliers
		lel <- .setValuesLow(lel, notoutliers); lel
		mel <- .setValuesMedium(mel, notoutliers); mel
		hel <- .setValuesHigh(hel, notoutliers); hel
		mfs[[ig]] <- list(lel=lel, mel=mel, hel=hel); mfs
	}
	
	return(mfs)
}


# Plot Membership Functions of a gene in graphical mode
# values: vector of expression values; attribute with the classes the samples belong in
# gene.mfs: list the 3 membership functions (low, medium, high)
# legends: boolean to show a legend in the plot or not
# samples: boolean to show vertical coloured lines, representing the samples
.plotGeneMF <- function(values, gene.mfs, legends=FALSE, samples=FALSE) {
	# Unlist the nested list of 1 element
	gene <- names(gene.mfs); gene
	gene.mfs <- gene.mfs[[gene]]; gene.mfs

	# Plots the 3 Membership Functions (low, medium, high)
	fromto <- c(range(values)[1] - .02, range(values)[2] + .02); fromto	
	curve(.computeMembershipLow(gene.mfs$lel,x), xlab="", ylab="", main=gene, xlim=fromto, n=2000, col="green", lwd=3, lab=c(10,5,5))
	curve(.computeMembershipMedium(gene.mfs$mel,x), add=TRUE, xlim=fromto, n=2000, col="black", lwd=3)
	curve(.computeMembershipHigh(gene.mfs$hel,x), add=TRUE, xlim=fromto, n=2000, col="red", lwd=3)
	
	# Plots vertical and horizontal lines
	abline(h = c(0.2,0.4,0.6,0.8), col = "lightgray", lty=3) #v = seq(fromto[1],fromto[2],length.out=10),
	#abline(v=gene.mfs$lel@center, col="green", lwd=1)
	#abline(v=gene.mfs$mel@center, col="black", lwd=1)
	#abline(v=gene.mfs$hel@center, col="red", lwd=1)

	# Plots vertical lines representing each sample
	if(samples) {
		lev <- levels(attr(values,"classes")); lev
		n <- length(lev); n
		# The first time doesn't get the right colours
		pal <- palette(rainbow(n, start=.5, end=.85))#; print(pal)
		pal <- palette(rainbow(n, start=.5, end=.85))#; print(pal)
		for(i in 1:n) {
			abline(v=values[attr(values,"classes")==lev[i]], col=pal[i], lwd=1)
		}
	}
	
	# Shows the legend
	if(legends) {
		xlab1 <- paste("Low(C:",round(gene.mfs$lel@center,2),", W:",round(gene.mfs$lel@width,2),")", sep=""); xlab1
		xlab2 <- paste("Medium(C:",round(gene.mfs$mel@center,2),", W:",round(gene.mfs$mel@width,2),")", sep=""); xlab2
		xlab3 <- paste("High(C:",round(gene.mfs$hel@center,2),", W:",round(gene.mfs$hel@width,2),")", sep=""); xlab3
		legend("bottomleft", c(xlab1,xlab2,xlab3), 
			col = c("green","black","red"), bg="gray90",
			lty=1, lwd=4, cex=0.75, inset=.02)
	}
}


# Plot Membership Functions of several genes in graphical and/or text mode
plotMembershipFunctions <- function(rmadataset, mfs, genes) {
	# Graphical representation
	l <- length(genes); l
	if(l<37) {
		# Row-columns distribution
		cols <- floor(sqrt(l)); cols
		rows <- floor(l/cols); rows
		if(rows*cols < l) {
			if(rows==cols) {
				rows <- rows + 1; rows
			} else {
				cols <- cols + 1; cols
			}
		}
		op <- par(mfrow=c(rows,cols))
	
		ifelse(l<5, legends<-TRUE, legends<-FALSE)
		ifelse(l<4, samples<-TRUE, samples<-FALSE)

		for(gene in genes) {
			# Creates a vector with the expression values and classes for a gene
			values <- exprs(rmadataset)[gene,]; values
			classes <- phenoData(rmadataset)$class; classes
			names(classes) <- sampleNames(rmadataset); classes
			attr(values, "classes") <- classes; values
			# Plots an individual gene
			.plotGeneMF(values, mfs[gene], legends, samples)
		}
	} else {
		cat("\n######################################\nToo many genes for graphical plotting.\n######################################\n\n")
	}
	par(mfrow=c(1,1))

	# Text representation
	cw <- NULL
	for(gen in genes) {
		gen.mfs <- unlist(mfs[gen]); gen.mfs
		names(gen.mfs) <- c("lel","mel","hel"); gen.mfs
		cw <- c(cw,
			round(gen.mfs$lel@center,2),round(gen.mfs$lel@width,2),
			round(gen.mfs$mel@center,2),round(gen.mfs$mel@width,2),
			round(gen.mfs$hel@center,2),round(gen.mfs$hel@width,2))
	}; cw
	cw <- matrix(cw, ncol=6, byrow=TRUE); cw
	cw <- data.frame(cw, row.names=genes); cw
	colnames(cw) <- c("Center(Low)","Width(Low)","Center(Medium)","Width(Medium)","Center(High)","Width(High)"); cw
	return(cw)
}


# Returns a vector with the discrete labels corresponding to the expression values
# Uses de Membership Functions
.fuzzyDiscretization <- function(lel, mel, hel, values, zeta, overlapping) {
	disc.alphab <- c("Low", "Medium", "High"); disc.alphab
	disc.values <- vector(); disc.value <- NULL

	for (i in values) {
		lmh = c(.computeMembershipLow(lel, i),.computeMembershipMedium(mel, i), .computeMembershipHigh(hel, i))
		names(lmh) <- disc.alphab; lmh

	    # Discrete values upon the threshold
	    disc.value.vec <- names(lmh)[lmh>0 & lmh>zeta]; disc.value.vec
	    if(length(disc.value.vec) == 0) { # Avoids the vector becoming shorter
	    	disc.value <- NA
	    } else {
		    if(overlapping == 1) {
		  		# First value label upon the threshold
		  		disc.value <- disc.value.vec[1]
		    } else {
		  		# Concatenate discrete values in 1
	    		disc.value <- paste(disc.value.vec, collapse='-'); disc.value
		    }
	    }
		disc.values = c(disc.values, disc.value)
	}
	names(disc.values) <- names(values)

	return (disc.values)
}


# Returns a matrix with the discrete labels corresponding to the expression values
# Uses de Membership Functions
discretizeExpressionValues <- function(rmadataset, mfs, zeta=0.5, overlapping=2) {
	# Extract data from ExpressionSet
	rmam <- exprs(rmadataset); rmam[c(1:8),c(1:4)]
	rmav <- as.vector(pData(phenoData(rmadataset))$class); rmav
	names(rmav) <- sampleNames(rmadataset); rmav
	gene.names <- rownames(rmam); gene.names

	# Creates a matrix with the discrete labels
	dvs <- NULL	
	for (ig in gene.names) {
		values <- rmam[ig,]; values
		disc.values <- .fuzzyDiscretization(mfs[[ig]]$lel, mfs[[ig]]$mel, mfs[[ig]]$hel, values, zeta, overlapping); disc.values
		dvs <- rbind(dvs, disc.values); dvs
	}
	rownames(dvs) <- gene.names; head(dvs)
	attr(dvs, "types") <- rmav; dvs

	return(dvs)
}


showDiscreteValues <- function(dvs, genes, classes) {
	cond1 <- TRUE
	cond2 <- TRUE
	if(!missing(genes)) {
		cond1 <- genes
	}
	if(!missing(classes)) {
		cond2 <- attr(dvs, "types") %in% classes
	}
	return(dvs[cond1,cond2])
}


# CaseBaseFuzzyPatterns.calculateFuzzyPatterns
.fuzzyPatterns <- function(disc.values, disc.alphab, piVal) {

  fuzzypat <- NULL # Fuzzy patterns vector
  vifs <- NULL # Impact factors vector
  types <- attr(disc.values,"types"); types
  utypes <- unique(types); utypes

  for(i in utypes) { #i<-"healthy"

  	# First category with the greatest number of occurrences
  	samp <- disc.values[types == i]; samp
  	fac <- factor(samp, levels=disc.alphab); fac
  	table.fac <- table(fac); table.fac # Optimizing time
  	max.factor <- max(table.fac); max.factor # Optimizing time
  	categ <- names(table.fac[table.fac==max.factor])[1]; categ

  	# Fuzzy pattern
  	impact.factor <- max.factor / length(fac); impact.factor
  	ifelse( impact.factor > piVal, fuzzypat <- c(fuzzypat, categ), fuzzypat <- c(fuzzypat, NA) )# Count NA values
  	vifs <- c(vifs, impact.factor); vifs
  }
  names(fuzzypat) <- utypes; fuzzypat
  names(vifs) <- utypes; vifs
  attr(fuzzypat, "ifs") <- vifs; fuzzypat

  return(fuzzypat)
}


calculateFuzzyPatterns <- function(rmadataset, dvs, piVal=0.9, overlapping=2) {

	if(overlapping == 1) {
		disc.alphab <- c("Low", "Medium", "High")
	} else if(overlapping == 2) {
		disc.alphab <- c("Low", "Low-Medium", "Medium", "Medium-High", "High")
	} else {
		disc.alphab <- c("Low", "Low-Medium", "Low-Medium-High", "Medium", "Medium-High", "High")
	}

	fps <- NULL
	ifs <- NULL	

	for (ig in featureNames(rmadataset)) {
		disc.values <- dvs[ig,]; disc.values
		attr(disc.values,"types") <- attr(dvs,"types"); disc.values
		fuzzypat <- .fuzzyPatterns(disc.values, disc.alphab, piVal); fuzzypat

		# Matrices: Fuzzy Patterns for each disease type and gene (impact factors too)
		fps <- rbind(fps, fuzzypat); fps
		ifs <- rbind(ifs, attr(fuzzypat, "ifs")); ifs
	}
	rownames(fps) <- featureNames(rmadataset); head(fps)
	rownames(ifs) <- featureNames(rmadataset); head(ifs)
	attr(fps, "ifs") <- ifs; head(fps)

	return(fps)
}


showFuzzyPatterns <- function(fps, class) {
	return (fps[,class][!is.na(fps[,class])])
}


calculateDiscriminantFuzzyPattern <- function(rmadataset, fps) {
	# Works out the discriminant genes
	discriminants <- NULL
	for (ig in featureNames(rmadataset)) {
		table.facFP <- table(factor(fps[ig,]))
		max.facFP <- ifelse(sum(table.facFP)==0,0,max(table.facFP)); max.facFP
		if( max.facFP > 0 & max.facFP < sum(table.facFP) )
			discriminants <- c(discriminants, ig)
	}
	dfp <- fps[discriminants,]; dfp
	
	# Adheres the impact factors corresponding to the discriminant fuzzy pattern
	attr(dfp,"ifs") <- attr(fps,"ifs")[discriminants,]; dfp
	
	return(dfp)
}


plotDiscriminantFuzzyPattern <- function(dfp, overlapping=2) {

	if(overlapping == 1) {
		disc.alphab <- c("Low", "Medium", "High")
	} else if(overlapping == 2) {
		disc.alphab <- c("Low", "Low-Medium", "Medium", "Medium-High", "High")
	} else {
		disc.alphab <- c("Low", "Low-Medium", "Low-Medium-High", "Medium", "Medium-High", "High")
	}

	discriminants <- rev(rownames(dfp)); discriminants

	lfps <- NULL
	for(i in discriminants) {
		fp <- factor(dfp[i,], levels=disc.alphab); fp
		lfps <- c(lfps, as.numeric(fp)); lfps
	}
	ncol <- length(lfps)/length(discriminants); ncol
	lfps <- matrix(lfps, nrow=length(discriminants), ncol=ncol, byrow=TRUE); lfps
	lfps[is.na(lfps)] <- 0; lfps
	colnames(lfps) <- colnames(dfp); lfps
	rownames(lfps) <- rownames(dfp); lfps

	image(t(lfps), axes=FALSE, main="Discriminant Fuzzy Pattern",
		col=c("gray","#00BB00","#009900","#000099","#990000","red"))
	
	lx <- length(colnames(dfp)); lx
	ly <- length(rownames(dfp)); ly
	x <- seq(0,1,length=lx); x
	y <- seq(0,1,length=ly); y
	mx <- matrix(x ,nr=ly, nc=lx, byrow=TRUE); mx
	my <- matrix(y, nr=ly, nc=lx); my

	par(mfrow=c(1,1))
	par(cex=4/7)
	axis(1, at=x, labels=colnames(dfp))
	if(length(discriminants) <= 50) {
		axis(2, at=y, labels=discriminants, las=2)
		dat <- round(attr(dfp,"ifs"),2); dat
		text(mx, my, dat[discriminants,])
	}
	#print(dfp); cat("\n")
	#print(attr(dfp,"ifs"))
	return(dfp)
	par(cex=1)
	#heatmap(lfps, Rowv=NA, Colv=NA, scale="none", main="Fuzzy Patterns",
	#	col=c("gray","green","#00BB00","black","#00BB00","red"))
}
