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

    if(covariateSettings$useTopicModeling == TRUE){

        data <- Matrix::sparseMatrix(i=covariates$rowId,
                                     j=covariates$covariateId,
                                     x=covariates$covariateValue, #add 0.1 to avoid to treated as binary values
                                     dims=c(max(covariates$rowId), max(covariates$covariateId))) # edit this to max(map$newIds)

        colnames(data) <- as.numeric(paste0(9999,wordList$num))

        if(covariateSettings$optimalTopicValue == TRUE){

            topicLange <- seq(1,101, by=10)

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


    }


    if (aggregated)
        stop("Aggregation not supported")

    result <- list(topicModel = lda_model,
                   #topicDistr = doc_topic_distr,
                   wordList = wordList,
                   #rowIdList = rowIdMappingDf,
                   nGramSetting = covariateSettings$nGram,
                   numberOfTopics = numberOfTopics,
                   RdsFilePosition = covariateSettings$topicModelExportRds
                   )

    saveRDS(result,covariateSettings$topicModelExportRds)
    message(paste('your topic model is saved at',covariateSettings$topicModelExportRds))

    return(result)
}


