#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param rawcovariateId
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' ngram()
ngram <- function(rawcovariateId){

    rawcovariateIdN2 <- rawcovariateId[-1]

    if(covariateSettings$nGram == 2){
        rawcovariateId <- paste0(rawcovariateId,rawcovariateIdN2)
    }
    if(covariateSettings$nGram == 3){
        rawcovariateIdN3 <- rawcovariateIdN2[-1]

        rawcovariateId <- c(rawcovariateId,
                            paste0(rawcovariateId,rawcovariateIdN2),
                            paste0(rawcovariateId,rawcovariateIdN2,rawcovariateIdN3))
    }

    #Unique value. (Frequency is not taken into account.)
    rawcovariateId <- unique.default(sapply(rawcovariateId, unique))

    return(rawcovariateId)
}
