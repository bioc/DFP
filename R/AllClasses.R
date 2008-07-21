setClass("ExpressionLevel", representation(center="numeric",width="numeric", "VIRTUAL"))#, where=where
setClass("LowExpressionLevel", contains="ExpressionLevel")
setClass("MediumExpressionLevel", contains="ExpressionLevel")
setClass("HighExpressionLevel", contains="ExpressionLevel")
