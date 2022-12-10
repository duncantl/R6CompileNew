update =
    #
    # apply fun to each of the R6ClassGenerator objects
    # The intention is to "compile" methods for these classes
    # in the sense of creating new R functions that have the specific class
    # information baked into the methods rather than computing this generically
    # in each call to that method.
    # Specifically, by default, fun creates a different version of the new function
    # for each class that has the information about the specific public and private methods, the active bindings,
    #  the inheritance structure and from which classes the methods come.
    #
function(fun = updateNew, ..., namespace = "rstatic")    
{
    ns = getNamespace(namespace)
    els = as.list.environment(ns, TRUE)
    w = sapply(els, is, "R6ClassGenerator")
    klasses = lapply(names(ns)[w], function(x) get(x, ns))
    names(klasses) = names(ns)[w]
#    inh = !sapply(klasses, function(x) is.null(x$inherit))
    # c("SCCHelper", "Stack", "ASTNode", "Counter")
    invisible(lapply(names(klasses), function(x, ...) { fun(get(x, ns), ...)}, ...))
}

