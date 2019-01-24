#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param noteConceptId,existingTopicModel,buildTopicModeling,topicModelExportRds,useDictionary,limitedMedicalTermOnlyLanguage,nGram,buildTopidModelMinFrac,buildTopidModelMaxFrac,optimalTopicValue,numberOfTopics,sampleSize
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' createTopicModelingSetting()
createTopicModelingSetting <- function(noteConceptId = c(44814637),
                                       topicModelExportRds = file.path(getwd(),"customTopicModel.rds"),
                                       ComparisonCohortSchema = '',
                                       ComparisonCohortTable = 'cohort',
                                       ComparisonCohortId = -1,
                                       useDictionary=TRUE,
                                       limitedMedicalTermOnlyLanguage = c('KOR','ENG'),
                                       nGram = c(1L),
                                       buildTopidModelMinFrac = 0.001,
                                       buildTopidModelMaxFrac = 0.5,
                                       optimalTopicValue =FALSE,
                                       numberOfTopics=10L,
                                       sampleSize=-1){

    if(sum(limitedMedicalTermOnlyLanguage %in% c('KOR','ENG')==FALSE) >=1){
        stop("Implemented Language is c('KOR','ENG')")
    }
    covariateSettings <- list(noteConceptId = noteConceptId,
                              sampleSize = sampleSize,
                              ##useTextToVec
                              useTextToVec = FALSE,
                              ##TopicModeling
                              useTopicModeling = TRUE,
                              optimalTopicValue = optimalTopicValue,
                              numberOfTopics = numberOfTopics,
                              existingTopicModel = NULL,
                              ##buildTopicModeling
                              buildTopicModeling = TRUE,
                              topicModelExportRds = topicModelExportRds,
                              ComparisonCohortId = ComparisonCohortId,
                              ComparisonCohortSchema = ComparisonCohortSchema,
                              ComparisonCohortTable = ComparisonCohortTable,
                              ##TextToVec+TopicModeling
                              useDictionary = useDictionary,
                              limitedMedicalTermOnlyLanguage = limitedMedicalTermOnlyLanguage,
                              nGram = nGram,
                              buildTopidModelMinFrac = buildTopidModelMinFrac,
                              buildTopidModelMaxFrac = buildTopidModelMaxFrac,
                              ##Glove
                              useGloVe = FALSE,
                              latentDimensionForGlove = 0L,
                              ##Autoencoder
                              useAutoencoder = FALSE,
                              latentDimensionForAutoEncoder = 0L)

    attr(covariateSettings,'fun') <- 'getTopicFromNoteSettings'

    class(covariateSettings) <- 'covariateSettings'

    return(covariateSettings)
}

