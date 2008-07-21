setMethod("show","ExpressionLevel",function(object){
	cat("Class Type:", class(object), "\n")
	cat("Center:", object@center, "\n")
	cat("Width:", object@width, "\n")
})
