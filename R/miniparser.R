#' @export
generatePatterns = function(patterns, # final outputs here
                            helperPatterns = NULL, # intermediate patterns that won't be returned here
                            .open= '{', .close = '}'){

    for (i in seq_along(helperPatterns)){
        helperPatterns[[i]] = with(helperPatterns[1:i],
                                   {glue::glue(helperPatterns[[i]],
                                               .open = .open,
                                               .close = .close)})
    }

    for (i in seq_along(patterns)) {
        patterns[[i]] =
            with(c(helperPatterns, patterns[1:i]),
                 {glue::glue(patterns[[i]],
                             .open = .open,
                             .close =.close)
                 })
    }

    return(patterns)
}


#' @export
miniparser = function(text,patterns){
    patterns %>% lapply(function(x){
        matches = gregexpr(x,text,perl = TRUE)
        matches %>% lapply(function(y){
            lengths = attributes(y)$match.length
            starts = y
            seq_along(lengths) %>% lapply(function(i){
                seq(from = starts[i], to = starts[i] + lengths[i] -1)
            })

        }) %>% unlist(recursive = FALSE) -> out

        return(out)
    }) -> matches

    if(matches %>% unlist() %>% duplicated %>% any){
        badmatches = which(matches %>% unlist() %>% duplicated)
        stop(paste(badmatches,collapse = ', '),
             ' characters match multiple patterns')
    }

    missing = !(seq_len(nchar(text)) %in% (matches %>% unlist))
    if(any(missing)){
        matches$missing = as.list(which(missing))
    }

    flatMatches = matches %>% unlist(recursive=FALSE)

    names(flatMatches) = names(matches) %>%
        lapply(function(x){rep(x,length(matches[[x]]))}) %>% unlist



    flatMatches = flatMatches[order(flatMatches %>% sapply(min))]

    charSubsets = flatMatches %>% sapply(function(x){
        substr(text,x[1],x[length(x)])
    })

    return(charSubsets)
}
