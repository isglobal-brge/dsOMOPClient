# Closed arithmetic AST constructors used visibly in the formula lesson.
book_col <- function(name) list(col=name)
book_lit <- function(value) list(lit=value)
book_op <- function(op,left,right) list(op=op,args=list(left,right))
book_expr <- function(expr) list(version=1L,assign=list(list(name="result",expr=expr)))
