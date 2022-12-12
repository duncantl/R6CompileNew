mkNew =
    # k is the R6 class object, e.g.  ArgumentList, i.e. the R6ClassGenerator.
    #
    #
function(k, update = TRUE)
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
    tmp = mkMethodsCode(k, inh, className = "", bindEnv = "public_bind_env")

    w = ("initialize" == tmp$inherited)
    
    initialize = if(!any(w))
                     NULL
                 else if(names(tmp$inherited)[w] != k$classname)
                        as.name(paste0(names(tmp$inherited)[w], ".initialize"))
                 else
                     as.name("initialize")
    pubMethodsCode[[length(classDefs) + 1L]] = tmp$code


    
# repeating the code from above for now.    
    privMethodsCode = list()
    inh = character()
    for(def in classDefs) {
        o = mkMethodsCode(def, inh, what = "private_methods")
        inh = o$inherited
        privMethodsCode[[def$classname]] = o$code
    }
    privMethodsCode[[length(classDefs) + 1L]] = mkMethodsCode(k, inh, className = "", bindEnv = "private_bind_env")$code


    activeBindings = mkActiveBindingCode(if(length(classDefs)) c(classDefs, k) else structure(list(k), names = ""))
    
    
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

    if(is.null(initialize))
        end = end[-5]
    else    
        end[[5]][[1]] = initialize
    

    b = c(as.list(envs)[-1],
          as.list(flds)[-1],
          setFieldsCode(public_fields),
          setFieldsCode(private_fields, "private_bind_env"),          
          unlist(supEnvs, recursive = FALSE),
          supChainDef,
          unlist(pubMethodsCode, recursive = FALSE),
          unlist(privMethodsCode, recursive = FALSE),
          activeBindings,
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
        
        ex = quote(list())
        if(any(inh))
            methodVars = c(methodVars, lapply(paste(names(inherited)[inh], inherited[inh], sep = "."), as.name))
        ex[ seq(along.with = methodVars) + 1L] = methodVars
        # use c(a = fun, b = fun) rather than structure(c(fun, fun), names = c("a", "b"))
        # as the former is about 3.14 times faster.
        names(ex)[seq(along.with = ids) + 1L] = ids

#        browser()
        e = lapply(names(ex)[-1], function(x) substitute( e$v <- val, list(e = env, v = as.name(x), val = ex[[x]])))
#        e = substitute(list2env( xxx, envir = e), list(xxx = ex, e = env))
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


mkActiveBindingCode =
    #
    #  Create a variable for each function for 
    #
function(cdefs)    
{
#    browser()
    code = list()
    ids = character()
    vars = character()

    rhs = quote(list())    

    for(i in seq(along.with = cdefs)) {
        k = cdefs[[i]]
        act =  k$active

        class = names(cdefs)[i]
        if(class != "")
            class = paste0(class, ".")
        
        if(class == "") {
            enclosEnv = as.name("enclos_env")
            bindEnv = as.name("public_bind_env")
        } else {
            enclosEnv = as.name(paste0(class, "super_enclos_env"))
            bindEnv = as.name(paste0(class, "super_bind_env"))
        }

        # add bindings in this super_bind_env for the inherited bindings from earlier classes
        code = c(code, mapply(function(id, var)
                                 substitute(makeActiveBinding(id, var, env), list(id = id, var = var, env = bindEnv))
                               , names(rhs[-1]), rhs[-1]))

        
        if(length(act) > 0) {
            vars = c(vars, paste0(class, names(act)))

            for( v in names(act)) {

                varName = as.name(paste0(class, v))
                tmp = substitute({
                    var = fun
                    environment(var) = enclosEnv
                    makeActiveBinding(id, var, bindEnv)
                }, list(fun = act[[v]],
                        var = varName,
                        enclosEnv = enclosEnv,
                        bindEnv = bindEnv,
                        id = v))

                code = c(code, as.list(tmp)[-1])

                j = length(rhs) + 1L
                rhs[[ j ]] = varName
                names(rhs)[j] = v
            }

        }

#browser()        
#        ids = c(ids, names(act))

        if(length(rhs) > 1) {
#            idx = seq(along.with = ids) + length(rhs) # 1L
#            rhs[idx] = lapply(vars, as.name)
#            names(rhs)[ idx ] = ids
        
            ex = substitute( enclosEnv$.__active__ <- els,
                        list(enclosEnv = enclosEnv,
                             els = rhs))
            code = c(code, ex)
        }
    }

    code
}


setFieldsCode =
function(els, env = "public_bind_env")    
{
    if(length(els) == 0)
        return(NULL)

    env = as.name(env)
    
    mapply(function(id, val)
             substitute(env$id <- val, list(env = env, id = id, val = val)),
           names(els), els)
}



# From R6 package.
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

