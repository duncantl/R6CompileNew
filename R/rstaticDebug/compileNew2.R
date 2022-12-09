mkNew =
    # k is the R6 class object, e.g.  ArgumentList, i.e. the R6ClassGenerator.
function(k)
{

    classDefs = list()
    tmp = k$get_inherit()
    while(length(tmp)) {
        classDefs[[tmp$classname]] = tmp
        tmp = tmp$get_inherit()
    }
    classDefs = rev(classDefs)
    
    
   #          inherit = ArgumentList  # inline/insert object    
    envs = quote({
             enclos_parent = parent_env    
             private_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
             public_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
             enclos_env <- new.env(parent = enclos_parent, hash = FALSE)
             enclos_env$self <- public_bind_env
             enclos_env$private <- private_bind_env # if there are private fields or methods
    })

    envs[[2]][[3]] = k$parent_env

    private_fields = recursive_merge(k, "private_fields")
    public_fields = recursive_merge(k, "public_fields")
    
    has_priv = length(private_fields)
    if(!has_priv)
        envs = envs[ - length(envs) ]


    flds = substitute({
                   public_fields = pub
                   private_fields = priv
                 }, list(pub = public_fields, priv = private_fields))
    

    supEnvs = lapply(names(classDefs), mkSuperClassEnvCode)
    

    end = quote({
                  enclos_env$super <- Container.super_bind_env
                  public_bind_env$.__enclos_env__ <- enclos_env
                  #!! lock bindings - private_methods, public_methods,
                  class(public_bind_env) <- klasses
                  initialize(...)
                  #!! finalizers if there are any?
                  public_bind_env
           })
    classNames = parent.env(environment(k$new))$get_superclassnames(k)
    end[[4]][[3]] = classNames
    end[[2]][[3]] = as.name(paste0(classNames[2], ".super_bind_env"))
    
    b = c(as.list(envs)[-1],
          as.list(flds)[-1],
          as.list(supEnvs)[-1],          
          unlist(supEnvs, recursive = FALSE),
          as.list(end)[-1])

    new = function(...) {}    
    body(new)[1L + seq(along.with = b)] = b
    
    new    
}


mkSuperClassEnvCode =
function(className, hasPriv)
{
   ans =  substitute({
        super_enclos_env <- new.env(parent = enclos_parent, hash = FALSE)
        super_bind_env <- new.env(parent = emptyenv(), hash = FALSE)
        super_bind_env$.__enclos_env__ = super_enclos_env
        super_enclos_env$self = public_bind_env
        super_enclos_env$private = private_bind_env        
    }, list(super_enclos_env = as.name(paste0(className, ".super_enclos_env")),
            super_bind_env = as.name(paste0(className, ".super_bind_env"))
            ))

   if(!hasPriv)
       ans = ans[ - length(ans) ]

   ans
}

mkSuperChainCode =
function(classNames)
{
    ans = vector("list", length(classNames))
    tmp = quote( x$super <- val )
    for(i in seq(along.with = classNames)[-1]) {
        tmp[[2]][[2]] = as.name(paste0(classNames[i], ".super_enclos_env"))
        tmp[[3]] = as.name(paste0(classNames[i - 1L], ".super_bind_env"))
        ans[[i - 1L]] = tmp
    }
        
    ans[[length(ans)]] = substitute(enclos_env$super <- val, list(val = ans[[ length(ans) - 1L ]][[2]][[2]]))
    ans
}



mkMethodsCode =
    # ipublic and iprivate are named character vectors
    # with the name of the method and the name on the element being the class
    # from which it is inherited
    # These are cumulated
function(k, ipublic = character(), iprivate = character(), className = k$classname)    
{
    pub = k$public_methods
    priv = k$private_methods

    pub.inh = setdiff(names(ipublic), names(pub))
    priv.inh = setdiff(names(iprivate), names(priv))    
    
    tmp = quote({
        var = fun
        environment(var) = env
    })
    tmp[[3]][[3]] = as.name(paste0(className, ".super_bind_env")) #XXX className == ""
    methodNames = paste(className, names(pub), sep = ".")
    methodVars = lapply(methodNames, as.name)
    code1 = mapply(function(mname, fun) {
                         tmp[[2]][[2]] = mname
                         tmp[[2]][[3]] = fun
                         tmp[[3]][[2]][[2]] = mname
                         tmp
                    }, methodVars, pub)


    # add inherited method names.
    defs = c(names(pub), pub.inh)
    tmp = substitute(structure(vars, names = ids), list(ids = defs))
    ex = quote(list())
    ex[ seq(along.with = methodVars) + 1L] = methodVars
    tmp[[2]] = ex
    e = substitute(list2env( xxx, envir = e), list(xxx = tmp, e = as.name(paste0(className, ".public_bind_env"))))    
    browser()    
    code = c(code1, e)
    
    list(code = code, ipublic = ipublic, iprivate = iprivate)
}
