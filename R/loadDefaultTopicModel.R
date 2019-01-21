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
                                NumberOfTopics = 100,
                                workingFile = file.path(getwd(),"customTopicModel.rds"),
                                useModel = 'base'){

    return(readRDS(system.file("BaseData", paste0("TopicModel_(",noteConceptId,")_(",paste(targetLanguage,collapse = ','),")_(",paste0('TopicNum',NumberOfTopics),").rds"),package = 'noteCovariateExtraction')))
}
