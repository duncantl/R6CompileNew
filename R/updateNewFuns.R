updateR6 =
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
function(namespace, fun = mkNew, ...)    
{

    if(is.environment(namespace))
        ns = namespace
    else if(is.character(namespace))
        ns = getNamespace(namespace)

    if(is.environment(ns))
        els = as.list.environment(ns, TRUE)
    else
        els = ns
    
    w = sapply(els, inherits, "R6ClassGenerator")
    klasses = lapply(names(ns)[w], function(x) get(x, ns))
    names(klasses) = names(ns)[w]

    invisible(structure(lapply(names(klasses),
                               function(x, ...) {
                                   fun(get(x, ns), ...)
                               }, ...),
                        names = names(klasses)))
}

