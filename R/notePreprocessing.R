#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param covariateId
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' notePreprocessing()
#'
notePreprocessing <- function(covariateId,notUseWordSequence = (covariateSettings$useTextToVec|covariateSettings$useTopicModeling), targetLanguage = covariateSettings$targetLanguage){

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
    if(notUseWordSequence == TRUE){
        if(covariateSettings$nGram >= 2){
            covariateId <- lapply(covariateId, function(x) gsub(' ','',unlist(lapply(RWeka::NGramTokenizer(x, RWeka::Weka_control(min=1, max=covariateSettings$nGram)), paste0, collapse = ""))))
            covariateId <- lapply(covariateId, unique)
        }
        else{
            covariateId <- strsplit(covariateId,' ')##N-gram can be developed from here!
            covariateId <- lapply(covariateId, unique)
        }
    }

    return(covariateId)
}
