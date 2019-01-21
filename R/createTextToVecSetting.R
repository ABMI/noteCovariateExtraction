#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param noteConceptId,topicModelExportRds,ComparisonCohortId,useDictionary,limitedMedicalTermOnlyLanguage,nGram,buildTopidModelMinFrac,buildTopidModelMaxFrac,optimalTopicValue,numberOfTopics,sampleSize
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' createTextToVecSetting()
createTextToVecSetting <- function(noteConceptId = c(44814637),
                                   useDictionary=TRUE,
                                   limitedMedicalTermOnlyLanguage = c('KOR','ENG'),
                                   nGram = c(1L),
                                   buildTopidModelMinFrac = 0.001,
                                   buildTopidModelMaxFrac = 0.5,
                                   sampleSize=-1){

    if(sum(limitedMedicalTermOnlyLanguage %in% c('KOR','ENG')==FALSE) >=1){
        stop("Implemented Language is c('KOR','ENG')")
    }
    covariateSettings <- list(noteConceptId = noteConceptId,
                              sampleSize = sampleSize,
                              ##useTextToVec
                              useTextToVec = TRUE,
                              ##TopicModeling
                              useTopicModeling = FALSE,
                              ##buildTopicModeling
                              buildTopicModeling = FALSE,
                              ##TextToVec+TopicModeling
                              useDictionary = useDictionary,
                              limitedMedicalTermOnlyLanguage = limitedMedicalTermOnlyLanguage,
                              nGram = nGram,
                              buildTopidModelMinFrac = buildTopidModelMinFrac,
                              buildTopidModelMaxFrac = buildTopidModelMaxFrac,
                              ##Glove
                              useGloVe = FALSE,
                              ##Autoencoder
                              useAutoencoder = FALSE)

    attr(covariateSettings,'fun') <- 'getTopicFromNoteSettings'

    class(covariateSettings) <- 'covariateSettings'

    return(covariateSettings)
}

