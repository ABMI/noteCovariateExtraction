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
#' buildTopicModeling()
buildTopicModeling<-function(connection,
                             oracleTempSchema = NULL,
                             cdmDatabaseSchema,
                             cohortTable = "cohort",
                             cohortId,
                             cdmVersion = "5",
                             rowIdField = "subject_id",
                             covariateSettings = covariateSettings,
                             aggregated = FALSE
){
    if (!covariateSettings$buildTopicModeling) stop("buildTopicModeling is FALSE")
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
        '{@cohort_id != -1} ? {AND cohort_definition_id = @cohort_id}',
        ';'
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
                                  AFUN = notePreprocessing <- function(covariateId){

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
                                          covariateId <- gsub('[\u314f-\u3163]*','',covariateId)
                                          covariateId <- gsub('[\u3131-\u314E]*','',covariateId)

                                          #Only Korean and English are left. (remove special characters)
                                          covariateId <- gsub('[^\uac00-\ud7a3a-zA-Z]',' ',covariateId)
                                      }

                                      #The spacing is only once                                                               ## vector
                                      covariateId <- stringr::str_replace_all(covariateId,"[[:space:]]{1,}"," ")

                                      covariateId <- sub(' ','',covariateId)
                                      if(covariateSettings$useTextToVec|covariateSettings$useTopicModeling == TRUE){
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
                                  })

    names(rawcovariateId) <- 'note'
    rawcovariateId <- rawcovariateId$'note'

    if(covariateSettings$buildTopidModelMinFrac != 0){
        MinValue <- as.integer(length(unique(unlist(rawcovariateId))) * covariateSettings$buildTopidModelMinFrac)

        MoreThanMin <- data.frame(table(unlist(rawcovariateId)),stringsAsFactors = F)
        MoreThanMinWord <- as.vector(MoreThanMin[MoreThanMin$'Freq'>=MinValue,1])

        rawcovariateId<-lapply(rawcovariateId, function(x) intersect(x, MoreThanMinWord))

    }

    if(covariateSettings$useDictionary){
        dictionary <- c()
        for(language in covariateSettings$limitedMedicalTermOnlyLanguage){
            dictionary <- paste(dictionary,
                                unlist(lapply(lapply(rawcovariateId, function(x) intersect(x,dictionaryForLanguage(language))),function(x) paste(x,collapse = '|'))),sep = '|')
        }
        dictionary <- sub('\\|','',dictionary)
        dictionary <- gsub('\\|{2,}','\\|',dictionary)
        dictionary <- sub('\\|$','',dictionary)

        #lapply..
        for(i in 1:length(rawcovariateId)){
            rawcovariateId[[i]] <- rawcovariateId[[i]][grep(dictionary[i],rawcovariateId[[i]])]
        }
    }

    #Configuring covariate
    rowIdMappingDf<- data.frame('rowId' = rep(1:length(rawCovariates$rowId)),'num' = rawCovariates$rowId[1:length(rawCovariates$rowId)])

    names(rawcovariateId) <- rowIdMappingDf$'rowId'

    covariates <- reshape2::melt(data = rawcovariateId)
    colnames(covariates) <- c('covariateId','rowId')
    covariates$rowId <- as.numeric(covariates$rowId)
    covariateValue <- rep(1,nrow(covariates))

    wordlist <- covariates

    covariates <- cbind(covariates,covariateValue)
    #####################################

    covariateId.factor<-as.factor(covariates$covariateId)

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

        if(covariateSettings$optimalTopicValue == TRUE){

            topicLange <- seq(1,101, by=10)

            best.model <- lapply(topicLange, function(k){topicmodels::LDA(data, k)})
            best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, topicmodels::logLik)))
            best.model.logLik.df <- data.frame(topics=topicLange, LL=as.numeric(as.matrix(best.model.logLik)))

            optimalNumberOfTopic = best.model.logLik.df[which.max(best.model.logLik.df$LL),]$topics

            detach("package:topicmodels", unload=TRUE)

            lda_model = text2vec::LDA$new(n_topics = optimalNumberOfTopic, doc_topic_prior = 0.1, topic_word_prior = 0.01)
        }
        else if(covariateSettings$optimalTopicValue == FALSE){
            optimalNumberOfTopic = NULL
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


    result <- list(topicModel = lda_model,
                   topicDistr = doc_topic_distr,
                   wordList = data.frame(level=seq(levels(covariateId.factor)), words=as.character(levels(covariateId.factor))),
                   rowIdList = rowIdMappingDf,
                   nGramSetting = covariateSettings$nGram,
                   optimalNumberOfTopic = optimalNumberOfTopic)

    if(!dir.exists(file.path(system.file(package = 'noteCovariateExtraction'),'CustomData'))){
        dir.create(file.path(system.file(package = 'noteCovariateExtraction'),'CustomData'),recursive = T)

    }

    saveRDS(result,paste0(getwd(),'/inst/BaseData/TopicModel_',
                          paste0('(',paste0(sort(covariateSettings$noteConceptId),collapse = ','),')'),
                          '_',
                          paste0('(',paste0(sort(covariateSettings$targetLanguage),collapse = ','),')'),
                          '.rds'))

    # saveRDS(result,file.path(system.file("CustomData",package = 'noteCovariateExtraction'),
    #                          paste0('TopicModel_',paste0('(',paste0(sort(covariateSettings$noteConceptId),collapse = ','),')'),'_',
    #                                 paste0('(',paste0(sort(covariateSettings$targetLanguage),collapse = ','),')'),'.rds')))
    #
    message(paste('your CustomTopicModel name is',paste0('TopicModel_',paste0('(',paste0(sort(covariateSettings$noteConceptId),collapse = ','),')'),'_',
            paste0('(',paste0(sort(covariateSettings$targetLanguage),collapse = ','),')'),'.rds')))

    return(result)
}


