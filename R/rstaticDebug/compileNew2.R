mkNew =
    # k is the R6 class object, e.g.  ArgumentList, i.e. the R6ClassGenerator.
function(k, update = FALSE)
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

    recursive_merge = parent.env(environment(k$new))$re
    
    private_fields = recursive_merge(k, "private_fields")
    public_fields = recursive_merge(k, "public_fields")
    
    has_priv = k$has_private() # length(private_fields) > 0

    if(!has_priv)
        envs = envs[ - length(envs) ]


    flds = substitute({
                   public_fields = pub
                   private_fields = priv
                 }, list(pub = public_fields, priv = private_fields))
    

    supEnvs = lapply(names(classDefs), mkSuperClassEnvCode, has_priv)

    supChainDef = mkSuperChainCode(names(classDefs))


    pubMethodsCode = list()
    inh = character()
    for(def in classDefs) {
        o = mkMethodsCode(def, inh)
        inh = o$inherited
        pubMethodsCode[[def$classname]] = o$code
    }
    pubMethodsCode[[length(classDefs) + 1L]] = mkMethodsCode(k, inh, className = "", bindEnv = "public_bind_env")$code

# repeating the code from above for now.    
    privMethodsCode = list()
    inh = character()
    for(def in classDefs) {
        o = mkMethodsCode(def, inh, what = "private_methods")
        inh = o$inherited
        privMethodsCode[[def$classname]] = o$code
    }
    privMethodsCode[[length(classDefs) + 1L]] = mkMethodsCode(k, inh, className = "", bindEnv = "private_bind_env")$code
    
    

    

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
          unlist(supEnvs, recursive = FALSE),
          supChainDef,
          unlist(pubMethodsCode, recursive = FALSE),
          unlist(privMethodsCode, recursive = FALSE),
          as.list(end)[-1])

    new = function(...) {}    
    body(new)[1L + seq(along.with = b)] = b

    if(update)
        k$new = new
    
    invisible(new)
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
    }, list(super_enclos_env = as.name(paste0(className, ".super_enclos_env")), #XXX when top-level class remove .
            super_bind_env = as.name(paste0(className, ".super_bind_env"))
            ))

   if(!hasPriv)
       ans = ans[ - length(ans) ]

   as.list(ans)[-1]
}

mkSuperChainCode =
function(classNames)
{
    if(length(classNames) < 2)
        return(list())
    
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
function(k, inherited = character(), what = "public_methods",
         className = paste0(k$classname, "."),
         bindEnv = "super_bind_env")
{
    pub = get(what, k)

    inh = !(inherited %in%  names(pub))
    
    tmp = quote({
        var = fun
        environment(var) = env
    })
    tmp[[3]][[3]] = as.name(paste0(className, if(className != "") "super_enclos_env" else "enclos_env")) #XXX className == ""
    methodNames = paste0(className, names(pub))
    methodVars = lapply(methodNames, as.name)
    code1 = mapply(function(mname, fun) {
                         tmp[[2]][[2]] = mname
                         tmp[[2]][[3]] = fun
                         tmp[[3]][[2]][[2]] = mname
                         as.list(tmp)[-1]
                    }, methodVars, pub)


    # add inherited method names.
    ids = if(length(pub))
              c(structure(names(pub), names = rep(k$classname, length(pub))), inherited[inh])
          else
              inherited[inh]

    
    env = as.name(paste0(className, bindEnv))
    if(what == "private_methods")
        env = substitute( x$private, list(x = env))
    
    if(length(ids) == 1) {
        e = substitute( w$id <- val, list(w = env, id = as.name(ids),
                                          val = as.name(paste(names(ids), ids, sep = "."))))

    } else {
    
        tmp = substitute(structure(vars, names = ids), list(ids = unname(ids)))
    
        ex = quote(list())
        if(any(inh))
            methodVars = c(methodVars, lapply(paste(names(inherited)[inh], inherited[inh], sep = "."), as.name))
        ex[ seq(along.with = methodVars) + 1L] = methodVars
        tmp[[2]] = ex

        e = substitute(list2env( xxx, envir = e), list(xxx = tmp, e = env))
    }

    code = c(code1, e)    
    
    inherited2 = if(length(pub))
                     structure(names(pub), names = rep(k$classname, length(pub)))
                 else
                     character()
    if(any(inh))
        inherited2 = c(inherited2, inherited[inh])
    
    list(code = code, inherited = inherited2)
}



# From R6
recursive_merge <-
function(obj, which) 
{
    if (is.null(obj)) 
        return(NULL)
    merge_vectors(recursive_merge(obj$get_inherit(), which), obj[[which]])
}

merge_vectors =
function(a, b)
{
    a[names(b)] = b
    a
}

