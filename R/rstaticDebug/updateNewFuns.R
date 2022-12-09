update =
function(fun = updateNew, ...)    
{
    ns = getNamespace("rstatic")
    els = as.list.environment(ns, TRUE)
    w = sapply(els, is, "R6ClassGenerator")
    klasses = lapply(names(ns)[w], function(x) get(x, ns))
    names(klasses) = names(ns)[w]
#    inh = !sapply(klasses, function(x) is.null(x$inherit))
    # c("SCCHelper", "Stack", "ASTNode", "Counter")
    invisible(lapply(names(klasses), function(x, ...) { fun(get(x, ns), ...)}, ...))
}

