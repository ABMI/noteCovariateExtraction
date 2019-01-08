#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param useTopicFromNote,noteConceptId,useDictionary,targetLanguage,limitedMedicalTermOnlyLanguage
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' createTopicFromNoteSettings()
createTopicFromNoteSettings <- function(noteConceptId = c(44814637),
                                        existingTopicModel = NULL,
                                        buildTopicModeling= FALSE,
                                        topicModelExportRds = file.path(getwd(),"customTopicModel.rds"),
                                        useDictionary=TRUE,
                                        limitedMedicalTermOnlyLanguage = c('KOR','ENG'),
                                        nGram = c(1L,2L),
                                        buildTopidModelMinFrac = 0.001,
                                        buildTopidModelMaxFrac = 0.5,
                                        useTextToVec = FALSE,
                                        useTopicModeling=FALSE,
                                        optimalTopicValue =TRUE,
                                        numberOfTopics=10L,
                                        #useGloVe = FALSE,
                                        #latentDimensionForGlove = 100L,
                                        #useAutoencoder=FALSE,
                                        #latentDimensionForAutoEncoder = 100L,
                                        sampleSize=-1){

    #if(!useDictionary) stop('Currently you should use at least one dictionary to extract medical terms only')
    # if ( sum(limitedMedicalTermOnlyLanguage %in% targetLanguage==FALSE)>=1 ) stop (paste('Select dictionary among target Languages :',
    #                                                                                      paste(targetLanguage,collapse=" ")))
    # if (sum(targetLanguage %in% limitedMedicalTermOnlyLanguage) != length(targetLanguage)) stop("It contains an unimplemented language.
    #                                                                                             Implemented languages are c ('KOR', 'ENG').")
    # if (sum (useTextToVec,useTopicModeling,useGloVe,useAutoencoder) != 1 ) {
    #     stop("Choose only one among useTextToVec,useTopicModeling,useGloVe,useAutoencoder")
    # }
    # if(!(useTextToVec|useTopicModeling) & !(nGram > 1)){
    #     stop('Use ngram only for useTextToVec,useTopicModeling')
    # }

    covariateSettings <- list(noteConceptId = noteConceptId,
                              existingTopicModel = existingTopicModel,
                              buildTopicModeling = buildTopicModeling,
                              topicModelExportRds = topicModelExportRds,
                              useDictionary = useDictionary,
                              limitedMedicalTermOnlyLanguage = limitedMedicalTermOnlyLanguage,
                              nGram = nGram,
                              buildTopidModelMinFrac = buildTopidModelMinFrac,
                              buildTopidModelMaxFrac = buildTopidModelMaxFrac,
                              useTextToVec = useTextToVec,
                              useTopicModeling = useTopicModeling,
                              optimalTopicValue = optimalTopicValue,
                              numberOfTopics = numberOfTopics,
                              # useGloVe = useGloVe,
                              # latentDimensionForGlove = latentDimensionForGlove,
                              # useAutoencoder = useAutoencoder,
                              # latentDimensionForAutoEncoder = latentDimensionForAutoEncoder,
                              sampleSize = sampleSize)
    if(buildTopicModeling == FALSE){
        attr(covariateSettings,'fun') <- 'getTopicFromNoteSettings'
    }
    else{
        attr(covariateSettings,'fun') <- 'buildTopicModeling'
    }
    class(covariateSettings) <- 'covariateSettings'
    return(covariateSettings)
}

#' Custom createCoveriate Settings
#'
#' This function is loading existing topic modelCustom createCoveriate Settings.
#' @param existingTopicModel,useCustomTopicModel,useModel
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' loadDefaultTopicModel()
loadDefaultTopicModel<-function(noteConceptId = c(44814637),
                                targetLanguage = c('KOR'),
                                useModel = 'base'){
    if(useModel == 'base'){
        return(readRDS(system.file("BaseData", paste0("TopicModel_(",noteConceptId,")_(",paste(targetLanguage,collapse = ','),").rds"),package = 'noteCovariateExtraction')))
    }
    else if(useModel == 'custom'){
        return(readRDS(system.file("CustomData", paste0("TopicModel_(",noteConceptId,")_(",paste(targetLanguage,collapse = ','),").rds"),package = 'noteCovariateExtraction')))
    }
}
