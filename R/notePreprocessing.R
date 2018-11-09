#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param covariateId
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' notePreprocessing()
notePreprocessing <- function(covariateId,useDictionary =TRUE, targetLanguage = covariateSettings$targetLanguage){

    covariateId <- gsub('<[^<>]*>',' ',covariateId) #Remove Tag

    #Remove html special characters
    covariateId <- gsub('&#x0D;', " ", covariateId)
    covariateId <- gsub('&lt;', " ", covariateId)
    covariateId <- gsub('&gt;', " ", covariateId)
    covariateId <- gsub('&amp;', " ", covariateId)
    covariateId <- gsub('&quot;', " ", covariateId)

    #####At least one should be included.
    #KOR PreProcessing
    if('KOR' %in% covariateSettings$targetLanguage){
        #remove hangle typo
        covariateId <- gsub('[ㅏ-ㅣ]*','',covariateId)
        covariateId <- gsub('[ㄱ-ㅎ]*','',covariateId)

        #Only Korean and English are left. (remove special characters)
        covariateId <- gsub('[^가-힣a-zA-Z]',' ',covariateId)
    }

    #The spacing is only once                                                               ## vector
    covariateId <- stringr::str_replace_all(covariateId,"[[:space:]]{1,}"," ")

    covariateId <- sub(' ','',covariateId)

    if(covariateSettings$nGram >= 2){
        covariateId <- lapply(covariateId, function(x) unlist(lapply(NLP::ngrams(NLP::words(x), 1:covariateSettings$nGram), paste, collapse = "")))
    }
    else{
        covariateId <- strsplit(covariateId,' ')##N-gram can be developed from here!
    }


    return(covariateId)
}
