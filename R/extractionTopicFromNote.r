#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param useTopicFromNote use = TURE, not use = FALSE
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' createTopicFromNoteSettings()
createTopicFromNoteSettings <- function(useTopicFromNote = TRUE,
                                        noteConceptId = c(44814637),
                                        useDictionary=TRUE,
                                        targetLanguage = c('KOR','ENG'),
                                        limitedMedicalTermOnlyLanguage = c('KOR','ENG'),
                                        nGram = 1L,
                                        useTextToVec = FALSE,
                                        useTopicModeling=FALSE,
                                        numberOfTopics=10L,
                                        useGloVe = FALSE,
                                        latentDimensionForGlove = 100L,
                                        useAutoencoder=FALSE,
                                        latentDimensionForAutoEncoder = 100L,
                                        sampleSize=-1){
    #if(!useDictionary) stop('Currently you should use at least one dictionary to extract medical terms only')
    if ( sum(limitedMedicalTermOnlyLanguage %in% targetLanguage==FALSE)>=1 ) stop (paste('Select dictionary among target Languages :',
                                                                                         paste(targetLanguage,collapse=" ")))
    if (sum (useTextToVec,useTopicModeling,useGloVe,useAutoencoder) != 1 ) {
        stop("Choose only one among useTextToVec,useTopicModeling,useGloVe,useAutoencoder")
    }
    if(!(useTextToVec|useTopicModeling) & !(nGram > 1)){
        stop('Use ngram only for useTextToVec,useTopicModeling')
    }
    covariateSettings <- list(useTopicFromNote = useTopicFromNote,
                              noteConceptId = noteConceptId,
                              useDictionary=useDictionary,
                              limitedMedicalTermOnlyLanguage=limitedMedicalTermOnlyLanguage,
                              nGram=nGram,
                              targetLanguage = targetLanguage,
                              useTextToVec=useTextToVec,
                              useTopicModeling=useTopicModeling,
                              numberOfTopics = numberOfTopics,
                              useGloVe=useGloVe,
                              useAutoencoder=useAutoencoder,
                              sampleSize=sampleSize)
    attr(covariateSettings,'fun') <- 'getTopicFromNoteSettings'
    class(covariateSettings) <- 'covariateSettings'
    return(covariateSettings)
}

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

    if(covariateSettings$useDictionary){
        newDictionary <- lapply(rawcovariateId, function(x) intersect(x,dictionaryForLanguage('ENG')))
        newDictionary2 <- lapply(rawcovariateId, function(x) intersect(x,dictionaryForLanguage('KOR')))
        for(i in 1:length(newDictionary)){
            newDictionary[[i]] <- paste0(paste(newDictionary[[i]],collapse="|"),paste(newDictionary2[[i]],collapse="|"))
            rawcovariateId[[i]] <- rawcovariateId[[i]][grep(newDictionary[[i]],rawcovariateId[[i]])]
        }
    }

    covariateId <- rawcovariateId

    #Configuring covariate
    names(covariateId) <- rawCovariates$rowId[1:length(rawCovariates$rowId)]

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
        lda_model = text2vec::LDA$new(n_topics = covariateSettings$numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
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
                   metaData = metaData)
    class(result) <- "covariateData"
    return(result)

}

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

    covariateId <- strsplit(covariateId,' ')##N-gram can be developed from here!

    if(covariateSettings$useTextToVec | covariateSettings$useTopicModeling){
        covariateId <- unique.default(sapply(covariateId, unique))
    }


    return(covariateId)
}

dictionaryForLanguage<-function(Language="KOR"){
    return(  switch(Language, "KOR"=kor_dictionary_db[,1], "ENG" = eng_dictionary_db[,1])  )
}


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



