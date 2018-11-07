#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @connection connection,oracleTempSchema,cdmDatabaseSchema,cohortTable,cohortId,cdmVersion,rowIdField,covariateSettings,aggregated
#' @oracleTempSchema createCovariateSetting
#' @cdmDatabaseSchema
#' @cohortTable
#' @cohortId
#' @cdmVersion
#' @rowIdField
#' @noteConceptId
#' @covariateSettings
#' @aggregated
#' getTopicFromNoteSettings()
getTopicFromNoteSettings <- function(connection,
                                     oracleTempSchema = NULL,
                                     cdmDatabaseSchema,
                                     cohortTable = "cohort",
                                     cohortId = -1,
                                     cdmVersion = "5",
                                     rowIdField = "subject_id",
                                     covariateSettings,
                                     aggregated = FALSE){

    writeLines('Constructing TopicFromNote')
    if (covariateSettings$useTopicFromNote == FALSE) {
        return(NULL)
    }
    #if (covariateSettings$useDictionary == TRUE){
    #SQL query should be revised to extract only the latest record
    #SQL to construct the covariate:
    sql <- paste(
        'SELECT',
        '{@sampleSize != -1} ? {TOP @sampleSize}',
        " @row_id_field AS row_id,",
        'n.NOTE_TEXT AS covariate_id,',
        '1 AS covariate_value',
        'FROM @cdm_database_schema.NOTE n',
        'JOIN @cohort_table c',
        'ON n.person_id = c.subject_id',
        'AND n.NOTE_DATE = c.COHORT_START_DATE',
        'WHERE NOTE_TYPE_CONCEPT_ID = (SELECT DESCENDANT_CONCEPT_ID FROM @cdm_database_schema.CONCEPT_ANCESTOR WHERE ANCESTOR_CONCEPT_ID IN (@note_concept_id) )',
        '{@cohort_id != -1} ? {AND cohort_definition_id = @cohort_id}'
    )

    sql <- SqlRender::renderSql(sql,
                                cohort_table = cohortTable,
                                cohort_id = cohortId,
                                note_concept_id = covariateSettings$noteConceptId,
                                row_id_field = rowIdField,
                                sampleSize=covariateSettings$sampleSize,
                                cdm_database_schema = cdmDatabaseSchema)$sql
    sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql

    # Retrieve the covariate:
    rawCovariates <- DatabaseConnector::querySql.ffdf(connection, sql)
    colnames(rawCovariates)<-SqlRender::snakeCaseToCamelCase(colnames(rawCovariates))
    ########################

    #ff in list #because Characters can not be inserted into the ff package.
    rawcovariateId <- ff::ffapply(x[i1:i2],X= rawCovariates$covariateId, RETURN=TRUE, CFUN="list",
                                  AFUN = notePreprocessing)

    names(rawcovariateId) <- 'note'
    rawcovariateId <- rawcovariateId$'note'
    if(covariateSettings$nGram > 1L){
        rawcovariateId <- lapply(rawcovariateId,ngram)
    }
    else{
        rawcovariateId <- lapply(rawcovariateId, unique)
    }

    if(covariateSettings$buildTopidModelMinFrac != 0){
        MinValue <- as.integer(length(unique(unlist(rawcovariateId))) * covariateSettings$buildTopidModelMinFrac)

        MoreThanMin <- data.frame(table(unlist(rawcovariateId)),stringsAsFactors = F)
        MoreThanMinWord <- as.vector(MoreThanMin[MoreThanMin$'Freq'>=MinValue,1])

        rawcovariateId<-lapply(rawcovariateId, function(x) intersect(x, MoreThanMinWord))

    }

    if(covariateSettings$useDictionary){
        newDictionary <- lapply(rawcovariateId, function(x) intersect(x,dictionaryForLanguage('ENG')))
        newDictionary2 <- lapply(rawcovariateId, function(x) intersect(x,dictionaryForLanguage('KOR')))
        for(i in 1:length(newDictionary)){
            if(!length(newDictionary[[i]])){
                newDictionary[[i]] <- paste0(paste(newDictionary[[i]],collapse="|"),"|",paste(newDictionary2[[i]],collapse="|"))
            }
            else{
                newDictionary[[i]] <- paste(newDictionary2[[i]],collapse="|")
            }
            rawcovariateId[[i]] <- rawcovariateId[[i]][grep(newDictionary[[i]],rawcovariateId[[i]])]
        }
    }

    covariateId <- rawcovariateId

    #Configuring covariate
    rowIdMappingDf<- data.frame('num' = rep(1:length(rawCovariates$rowId)),'rowId' = rawCovariates$rowId[1:length(rawCovariates$rowId)])

    names(covariateId) <- rowIdMappingDf$'num'
    #names(covariateId) <- rawCovariates$rowId[1:length(rawCovariates$rowId)]

    covariates <- reshape2::melt(data = covariateId)
    colnames(covariates) <- c('covariateId','rowId')
    covariates$rowId <- as.numeric(covariates$rowId)
    covariateValue <- rep(1,nrow(covariates))

    covariates <- cbind(covariates,covariateValue)

    #####################################

    covariateId.factor<-as.factor(covariates$covariateId)

    #rowIds<-levels(as.factor(covariates$rowId))
    if(covariateSettings$useTextToVec == TRUE){
        ##Text2Vec
        covariates$covariateId<-as.numeric(paste0(9999,as.numeric(covariateId.factor)))
        covariates<-ff::as.ffdf(covariates)

        covariateRef  <- data.frame(covariateId = as.numeric(paste0(9999,seq(levels(covariateId.factor)) )),
                                    covariateName = paste0("NOTE-",levels(covariateId.factor)),
                                    analysisId = 0,
                                    conceptId = 0)
        covariateRef <- ff::as.ffdf(covariateRef)
    }

    if(covariateSettings$useTopicModeling == TRUE){
        covariates$covariateId<-as.numeric(as.factor(covariates$covariateId))

        data <- Matrix::sparseMatrix(i=covariates$rowId,
                                     j=covariates$covariateId,
                                     x=covariates$covariateValue, #add 0.1 to avoid to treated as binary values
                                     dims=c(max(covariates$rowId), max(covariates$covariateId))) # edit this to max(map$newIds)

        colnames(data) <- as.numeric(paste0(9999,seq(levels(covariateId.factor)) ))


        ##Topic Modeling


        # rowTotals <- apply(dtm , 1, sum)
        # dtm.new   <- dtm[rowTotals> 0, ]
        if(covariateSettings$optimalTopicValue == TRUE){
            library(topicmodels)

            best.model <- lapply(seq(2,10, by=1), function(k){LDA(data, k)})
            best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
            best.model.logLik.df <- data.frame(topics=c(2:10), LL=as.numeric(as.matrix(best.model.logLik)))

            optimal = best.model.logLik.df[which.max(best.model.logLik.df$LL),]$topics

            detach("package:topicmodels", unload=TRUE)

            lda_model = text2vec::LDA$new(n_topics = optimal, doc_topic_prior = 0.1, topic_word_prior = 0.01)
        }
        else if(covariateSettings$optimalTopicValue == FALSE){
            lda_model = text2vec::LDA$new(n_topics = covariateSettings$numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01) # covariateSettings$numberOfTopics -> optimal
        }

        doc_topic_distr = lda_model$fit_transform(x = data, n_iter = 1000,
                                                  convergence_tol = 0.001, n_check_convergence = 25,
                                                  progressbar = FALSE)

        doc_topic_distr_df <- data.frame(doc_topic_distr)

        covariateIds<-as.numeric(paste0(9999,as.numeric(1:length(doc_topic_distr_df))))
        colnames(doc_topic_distr_df)<-covariateIds
        doc_topic_distr_df$rowId<- seq(max(covariates$rowId))

        covariates<-reshape2::melt(doc_topic_distr_df,id.var = "rowId",
                                   variable.name="covariateId",
                                   value.name = "covariateValue")

        covariates$covariateId<-as.numeric(as.character(covariates$covariateId))
        covariates<-covariates[covariates$covariateValue!=0,]
        covariates<-ff::as.ffdf(covariates)

        ##need to remove 0
        covariateRef  <- data.frame(covariateId = covariateIds,
                                    covariateName = paste0("Topic",covariateIds),
                                    analysisId = 0,
                                    conceptId = 0)

        covariateRef <- ff::as.ffdf(covariateRef)
    }

    if(covariateSettings$useGloVe == TRUE){
        stop("useGlove has not not supported yet")
    }

    if(covariateSettings$useAutoencoder == TRUE){
        stop("useAutoencoder has not not supported yet")
    }

    # Construct analysis reference:
    analysisRef <- data.frame(analysisId = 0,
                              analysisName = "Features from Note",
                              domainId = "Note",
                              startDay = 0,
                              endDay = 0,
                              isBinary = "N",
                              missingMeansZero = "Y")
    analysisRef <- ff::as.ffdf(analysisRef)
    #}

    if (aggregated)
        stop("Aggregation not supported")

    # Construct analysis reference:
    metaData <- list(sql = sql, call = match.call())
    result <- list(covariates = covariates,
                   covariateRef = covariateRef,
                   analysisRef = analysisRef,
                   metaData = metaData,
                   topicModel = lda_model,
                   wordList = rowIdMappingDf,
                   SparseMatrix = data)
    class(result) <- "covariateData"
    return(result)

}
