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

    #buildTopicModeling
    if(covariateSettings$buildTopicModeling == TRUE){

        ComparisonCohortId = covariateSettings$ComparisonCohortId

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
                                    cohort_id = ComparisonCohortId,
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

                                          #lower
                                          covariateId<- tolower(covariateId)

                                          #remove hangle typo
                                          covariateId <- gsub('[\u314f-\u3163]*','',covariateId)
                                          covariateId <- gsub('[\u3131-\u314E]*','',covariateId)

                                          #if add other language, add unicode
                                          covariateId <- gsub('[^\uac00-\ud7a3a-zA-Z]',' ',covariateId)


                                          #The spacing is only once                                                           ## vector
                                          covariateId <- stringr::str_replace_all(covariateId,"[[:space:]]{1,}"," ")

                                          covariateId <- sub(' ','',covariateId)

                                          return(covariateId)
                                      })
        names(rawcovariateId) <- 'note'
        rawcovariateId <- rawcovariateId$'note'

        #nGram
        if(covariateSettings$nGram > 1L){
            rawcovariateId <- lapply(rawcovariateId, function(x) RWeka::NGramTokenizer(x,RWeka::Weka_control(min=1,max=covariateSettings$nGram)))
            rawcovariateId <- lapply(rawcovariateId, function(x) sub(' ','',x))
            rawcovariateId <- lapply(rawcovariateId, unique)
        }
        else{
            rawcovariateId <- strsplit(rawcovariateId,' ')
        }

        #Frequency
        if( (covariateSettings$buildTopidModelMinFrac != 0) | (covariateSettings$buildTopidModelMaxFrac != 1)){
            #unique
            rawcovariateId <- lapply(rawcovariateId, unique)

            MinValue <- as.integer(length(unique(unlist(rawcovariateId))) * covariateSettings$buildTopidModelMinFrac)
            MaxValue <- as.integer(length(unique(unlist(rawcovariateId))) * covariateSettings$buildTopidModelMaxFrac)

            wordFreq <- data.frame(table(unlist(rawcovariateId)),stringsAsFactors = F)
            MoreThanMinWord <- wordFreq[wordFreq$Freq >= MinValue,]
            MoreThanMinLessThanMaxWord <- MoreThanMinWord[MoreThanMinWord$Freq<=MaxValue,]
            MoreThanMinLessThanMaxWordVec<-as.vector(MoreThanMinLessThanMaxWord[,1])
            #unique
            rawcovariateId<-lapply(rawcovariateId, function(x) intersect(x, MoreThanMinLessThanMaxWordVec))
        }
        else{
            #unique
            rawcovariateId <- lapply(rawcovariateId, unique)
        }

        #dictionary
        if(covariateSettings$useDictionary == TRUE){

            dictionary <- lapply(covariateSettings$limitedMedicalTermOnlyLanguage, function(x) dictionaryForLanguage(x))
            dictionary <- unlist(rapply(dictionary, as.character, classes="factor", how="replace"))

            ## Extract up to words containing dictionary words
            # dictionary <- paste(dictionary,collapse = '|')
            # rawcovariateId <-lapply(rawcovariateId, function(x) grep(dictionary,x,value = T))
            ##################################################

            ##Extraction of words after reorganization with words existing in the certificate
            SimplifyDictionary <- lapply(rawcovariateId, function(x) intersect(unique(x),dictionary))
            SimplifyDictionary <- lapply(SimplifyDictionary, function(x) paste(x,collapse = '|'))
            for(i in 1:length(rawcovariateId)){
                rawcovariateId[[i]] <- grep(SimplifyDictionary[[i]],rawcovariateId[[i]],value=T)
            }
            #################################################################################
            #PreProcessing End, word List -> docs
            rawcovariateId <- lapply(rawcovariateId, function(x) paste(x,collapse =' '))
        }
        else{
            #PreProcessing End, word List -> docs
            rawcovariateId <- lapply(rawcovariateId, function(x) paste(x,collapse =' '))
        }

        #make Corpus
        my_docs <- tm::VectorSource(rawcovariateId)
        my_corpus <- tm::VCorpus(my_docs)
        DTM <- tm::DocumentTermMatrix(my_corpus)
        Encoding(DTM$dimnames$Terms) = 'UTF-8'
        wordList <- data.frame('word' = colnames(DTM),'num' = rep(1:ncol(DTM)))

        covariates <- data.frame('rowId' = DTM$i,'covariateId' = DTM$j,'covariateValue' = DTM$v)

        data <- Matrix::sparseMatrix(i=covariates$rowId,
                                     j=covariates$covariateId,
                                     x=covariates$covariateValue, #add 0.1 to avoid to treated as binary values
                                     dims=c(max(covariates$rowId), max(covariates$covariateId))) # edit this to max(map$newIds)

        colnames(data) <- as.numeric(paste0(9999,wordList$num))

        if(covariateSettings$optimalTopicValue == TRUE){

            topicLange <- seq(2,102, by=10)

            best.model <- lapply(topicLange, function(k){topicmodels::LDA(data, k)})
            best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, topicmodels::logLik)))
            best.model.logLik.df <- data.frame(topics=topicLange, LL=as.numeric(as.matrix(best.model.logLik)))

            numberOfTopics = best.model.logLik.df[which.max(best.model.logLik.df$LL),]$topics

            detach("package:topicmodels", unload=TRUE)

            lda_model = text2vec::LDA$new(n_topics = numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
        }
        else if(covariateSettings$optimalTopicValue == FALSE){
            numberOfTopics = covariateSettings$numberOfTopics

            lda_model = text2vec::LDA$new(n_topics = numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01) # covariateSettings$numberOfTopics -> optimal
        }

        doc_topic_distr = lda_model$fit_transform(x = data, n_iter = 1000,
                                                  convergence_tol = 0.001, n_check_convergence = 25,
                                                  progressbar = FALSE)

        if (aggregated)
            stop("Aggregation not supported")

        result <- list(topicModel = lda_model,
                       topicDistr = doc_topic_distr,
                       wordList = wordList,
                       nGramSetting = covariateSettings$nGram,
                       numberOfTopics = numberOfTopics
        )

        saveRDS(result,covariateSettings$topicModelExportRds)
        message(paste('your topic model is saved at',covariateSettings$topicModelExportRds))

        covariateSettings$existingTopicModel <-  readRDS(covariateSettings$topicModelExportRds)
    }


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
                                  AFUN = notePreprocessing <- function(covariateId){

                                      covariateId <- gsub('<[^<>]*>',' ',covariateId) #Remove Tag

                                      #Remove html special characters
                                      covariateId <- gsub('&#x0D;', " ", covariateId)
                                      covariateId <- gsub('&lt;', " ", covariateId)
                                      covariateId <- gsub('&gt;', " ", covariateId)
                                      covariateId <- gsub('&amp;', " ", covariateId)
                                      covariateId <- gsub('&quot;', " ", covariateId)

                                      #lower
                                      covariateId<- tolower(covariateId)

                                      #remove hangle typo
                                      covariateId <- gsub('[\u314f-\u3163]*','',covariateId)
                                      covariateId <- gsub('[\u3131-\u314E]*','',covariateId)

                                      #if add other language, add unicode
                                      covariateId <- gsub('[^\uac00-\ud7a3a-zA-Z]',' ',covariateId)


                                      #The spacing is only once                                                           ## vector
                                      covariateId <- stringr::str_replace_all(covariateId,"[[:space:]]{1,}"," ")

                                      covariateId <- sub(' ','',covariateId)

                                      return(covariateId)
                                  })
    names(rawcovariateId) <- 'note'
    rawcovariateId <- rawcovariateId$'note'

    #Only word (not Word order) # Currently, only parts that do not care about word order are implemented.
    #Word Spacing -> nGram -> Use unique words by each diagnosis -> Frequency -> Dictionary -> List to Docs -> Corpus

    #nGram
    if(covariateSettings$nGram > 1L){
        rawcovariateId <- lapply(rawcovariateId, function(x) RWeka::NGramTokenizer(x,RWeka::Weka_control(min=1,max=covariateSettings$nGram)))
        rawcovariateId <- lapply(rawcovariateId, function(x) sub(' ','',x))
        rawcovariateId <- lapply(rawcovariateId, unique)
    }
    else{
        rawcovariateId <- strsplit(rawcovariateId,' ')
    }

    #Frequency
    if( (covariateSettings$buildTopidModelMinFrac != 0) | (covariateSettings$buildTopidModelMaxFrac != 1)){
        #unique
        rawcovariateId <- lapply(rawcovariateId, unique)

        MinValue <- as.integer(length(unique(unlist(rawcovariateId))) * covariateSettings$buildTopidModelMinFrac)
        MaxValue <- as.integer(length(unique(unlist(rawcovariateId))) * covariateSettings$buildTopidModelMaxFrac)

        wordFreq <- data.frame(table(unlist(rawcovariateId)),stringsAsFactors = F)
        MoreThanMinWord <- wordFreq[wordFreq$Freq >= MinValue,]
        MoreThanMinLessThanMaxWord <- MoreThanMinWord[MoreThanMinWord$Freq<=MaxValue,]
        MoreThanMinLessThanMaxWordVec<-as.vector(MoreThanMinLessThanMaxWord[,1])
        #unique
        rawcovariateId<-lapply(rawcovariateId, function(x) intersect(x, MoreThanMinLessThanMaxWordVec))
    }
    else{
        #unique
        rawcovariateId <- lapply(rawcovariateId, unique)
    }

    #dictionary
    if(covariateSettings$useDictionary == TRUE){

        dictionary <- lapply(covariateSettings$limitedMedicalTermOnlyLanguage, function(x) dictionaryForLanguage(x))
        dictionary <- unlist(rapply(dictionary, as.character, classes="factor", how="replace"))

        ## Extract up to words containing dictionary words
        # dictionary <- paste(dictionary,collapse = '|')
        # rawcovariateId <-lapply(rawcovariateId, function(x) grep(dictionary,x,value = T))
        ##################################################

        ##Extraction of words after reorganization with words existing in the certificate
        SimplifyDictionary <- lapply(rawcovariateId, function(x) intersect(unique(x),dictionary))
        SimplifyDictionary <- lapply(SimplifyDictionary, function(x) paste(x,collapse = '|'))
        for(i in 1:length(rawcovariateId)){
            rawcovariateId[[i]] <- grep(SimplifyDictionary[[i]],rawcovariateId[[i]],value=T)
        }
        #################################################################################
        #PreProcessing End, word List -> docs
        rawcovariateId <- lapply(rawcovariateId, function(x) paste(x,collapse =' '))
    }
    else{
        #PreProcessing End, word List -> docs
        rawcovariateId <- lapply(rawcovariateId, function(x) paste(x,collapse =' '))
    }

    #make Corpus
    my_docs <- tm::VectorSource(rawcovariateId)
    my_corpus <- tm::VCorpus(my_docs)
    DTM <- tm::DocumentTermMatrix(my_corpus)
    Encoding(DTM$dimnames$Terms) = 'UTF-8'
    wordList <- data.frame('word' = colnames(DTM),'num' = rep(1:ncol(DTM)))

    covariates <- data.frame('rowId' = DTM$i,'covariateId' = DTM$j,'covariateValue' = DTM$v)

    if(covariateSettings$useTextToVec == TRUE){
        ##Text2Vec
        covariates$covariateId <- as.numeric(paste0(9999,DTM$j))

        covariates<-ff::as.ffdf(covariates)

        covariateRef  <- data.frame(covariateId = as.numeric(paste0(9999,DTM$j)),
                                    covariateName = paste0("NOTE-",dplyr::left_join(data.frame('num'=DTM$j),wordList,by ='num')$word),
                                    analysisId = 0,
                                    conceptId = 0)
        covariateRef <- ff::as.ffdf(covariateRef)
    }

    if(covariateSettings$useTopicModeling == TRUE){

        if(is.null(covariateSettings$existingTopicModel)){

            data <- Matrix::sparseMatrix(i=covariates$rowId,
                                         j=covariates$covariateId,
                                         x=covariates$covariateValue, #add 0.1 to avoid to treated as binary values
                                         dims=c(max(covariates$rowId), max(covariates$covariateId))) # edit this to max(map$newIds)

            colnames(data) <- as.numeric(paste0(9999,wordList$num))

            if(covariateSettings$optimalTopicValue == TRUE){

                topicLange <- seq(2,102, by=10)

                best.model <- lapply(topicLange, function(k){topicmodels::LDA(data, k)})
                best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, topicmodels::logLik)))
                best.model.logLik.df <- data.frame(topics=topicLange, LL=as.numeric(as.matrix(best.model.logLik)))

                numberOfTopics = best.model.logLik.df[which.max(best.model.logLik.df$LL),]$topics

                detach("package:topicmodels", unload=TRUE)

                lda_model = text2vec::LDA$new(n_topics = numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01)
            }
            else if(covariateSettings$optimalTopicValue == FALSE){
                numberOfTopics = covariateSettings$numberOfTopics

                lda_model = text2vec::LDA$new(n_topics = numberOfTopics, doc_topic_prior = 0.1, topic_word_prior = 0.01) # covariateSettings$numberOfTopics -> optimal
            }
        }

        if (!is.null(covariateSettings$existingTopicModel)) {
            covariates$covariateId <- dplyr::left_join(data.frame('num'=DTM$j),wordList,by ='num')$word
            mergedCov<-merge(covariates,covariateSettings$existingTopicModel$wordList,by.x="covariateId",by.y = "word")
            covariateIdInt<-as.numeric(mergedCov$num)
            if(nrow(mergedCov) == 0){
                stop('buildTopicModel And your covariate Word None match')
            }

            data <- Matrix::sparseMatrix(i=mergedCov$rowId,
                                         j=covariateIdInt,
                                         x=mergedCov$covariateValue, #add 0.1 to avoid to treated as binary values
                                         dims=c(max(mergedCov$rowId), max(nrow(covariateSettings$existingTopicModel$wordList)))) # edit this to max(map$newIds)

            colnames(data) <- as.numeric(paste0(9999, rep(1:nrow(covariateSettings$existingTopicModel$wordList))))
            lda_model = covariateSettings$existingTopicModel$topicModel

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
