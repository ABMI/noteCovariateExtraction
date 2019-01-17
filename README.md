# noteCovariateExtraction
## Getting Start
PredictionOfRehospitalization with variable Model

## Required Package
<pre><code>if(!require(ff)) {install.packages("ff")}
if(!require(stringr)) {install.packages("stringr")}
if(!require(RWeka)) {install.packages("RWeka")}
if(!require(quanteda)) {install.packages("quanteda")}
if(!require(dplyr)) {install.packages("dplyr")}
if(!require(tm)) {install.packages("tm")}
if(!require(Matrix)) {install.packages("Matrix")}
if(!require(caret)) {install.packages("caret")}
if(!require(text2vec)) {install.packages("text2vec")}
if(!require(e1071)) {install.packages("e1071")}
</code></pre>
- Install FeatureExtraction Package (https://github.com/OHDSI/FeatureExtraction)
- Install PatientLevelPrediction Package (https://github.com/OHDSI/PatientLevelPrediction)

### Instructions To Run Package
<pre><code>#install the network package
install.packages("devtools")
devtools::install_github("OHDSI/StudyProtocolSandbox/noteCovariateExtraction")
</code></pre>

### How to use the package
We create covariate in four ways with NOTE_TEXT in the NOTE table column. (Currently, only text2vec and TopicModeling are implemented.)
- step 1 : CREATE TABLE & INPUT VALUE (Cohort for hospitalized patients for more than 7 days for implementation)
- step 2 : Build TopicModel (If you do not want to use the topic model, pass it.) 
- step 3 : Create Covariates
### Library
<pre><code>library(noteCovariateExtraction)
library(DatabaseConnector)
library(SqlRender)
library(FeatureExtraction)
library(PatientLevelPrediction)
</code></pre>

## DataBase Connection & CDM Settings
<pre><code>options("fftempdir"="???") ## location with space to save big data

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                              server="???.???.???.???", ## Change to your IP
                                                              schema="???.dbo", ## Change to your DB
                                                              user="???", ## Change to your DataBase ID
                                                              password="???") ## Change to your DataBase PW
connection <- DatabaseConnector::connect(connectionDetails)

cdmDatabaseSchema<-"???.dbo" ## Change to CDM DB
targetDatabaseSchema<-"???.dbo" ## Change to target CDM DB
targetCohortTable<-"cohort"
targetCohortId <- ??? ## Change to Target Cohort Number
outcomeCohortId <- ??? ## Change to Outcome Cohort Number
trainCohortId <- ??? ## Change to trainCohort Number
cdmversion <- "???" ## your CDM Version
</code></pre>

## CREATE TABLE & INPUT VALUE
#### TargetCohort
<pre><code>
sql <- SqlRender::readSql(system.file('sql','sql_server','all_admission_a_week_or_more.sql',package = 'noteCovariateExtraction'))
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema=cdmDatabaseSchema,
                            target_database_schema=targetDatabaseSchema,
                            target_cohort_table=targetCohortTable,
                            target_cohort_id=targetCohortId
)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql
DatabaseConnector::executeSql(connection,sql)
</code></pre>

#### OutcomeCohort
<pre><code>sql <- SqlRender::readSql(system.file('sql','sql_server','ed_visit.sql',package = 'noteCovariateExtraction'))
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema=cdmDatabaseSchema,
                            target_database_schema=targetDatabaseSchema,
                            target_cohort_table=targetCohortTable,
                            target_cohort_id=outcomeCohortId
)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql
DatabaseConnector::executeSql(connection,sql)
</code></pre>

#### TRAIN COHORT For Topic Modeling
<pre><code>sql <- SqlRender::readSql(system.file('sql','sql_server','all_admission_less_than_a_week.sql',package = 'noteCovariateExtraction'))
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema=cdmDatabaseSchema,
                            target_database_schema=targetDatabaseSchema,
                            target_cohort_table=targetCohortTable,
                            target_cohort_id=trainCohortId
)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql
DatabaseConnector::executeSql(connection,sql)
</code></pre>


## Extract from the Note
#### If you want to get covariate based on topic modeling, run this code.
<pre><code>
covariateSettings <- noteCovariateExtraction::createTopicFromNoteSettings(
                     noteConceptId = c(44814637), # NOTE_TYPE_CONCEPT_ID
                     buildTopicModeling= TRUE,
                     topicModelExportRds = file.path('???.rds') # your topicModel Path
                     useDictionary=FALSE,
                     limitedMedicalTermOnlyLanguage = c('KOR','ENG'),
                     nGram = c(1L),
                     buildTopidModelMinFrac = 0.001,
                     buildTopidModelMaxFrac = 0.5,
                     useTopicModeling=TRUE,
                     optimalTopicValue =FALSE,
                     numberOfTopics=10L,
                     sampleSize=-1)
                     
topicModel<-buildTopicModeling(connection,
                               oracleTempSchema = NULL,
                               cdmDatabaseSchema,
                               cohortTable = "cohort",
                               cohortId = outcomeCohortId,
                               cdmVersion = "5",
                               rowIdField = "subject_id",
                               covariateSettings = covariateSettings
)
</code></pre>
- A TopicModel rds file is created with the topicModelExportRds you entered.
- createTopicFromNoteSettings Function Parameters
- **noteConceptId** : NOTE Column NOTE_TYPE_CONCEPT_ID in CDM
- **buildTopicModeling** : The result is an rds file containing lda_model and other information
- **topicModelExportRds** : topicModel Path ex)'/home/username/CustomTopicModel.rds'
- **useDictionary** : Extract only the words contained in the dictionary
- **limitedMedicalTermOnlyLanguage** : Korean and English dictionaries are available.
- **nGram** : Used when using techniques that do not require word order
- **buildTopidModelMinFrac** : Minimum Frequency for the entire word
- **buildTopidModelMaxFrac** : Maximum Frequency for the entire word
- **useTopicModeling** : TRUE as a function to create a Topicmodel
- **optimalTopicValue** : Find the optimal topic value. But it takes a long time.
- **NumberOfTopics** : Setting Custom Topic value.
- **sampleSize** : Enter the desired number of rows if you want to use the sample data, or -1 if you want to use the entire data.
#### Create Covariates
<pre><code>
covariateSettings <- noteCovariateExtraction::createTopicFromNoteSettings(noteConceptId = c(44814637),
                     existingTopicModel = loadDefaultTopicModel(NumberOfTopics = '???',useModel = 'base'), # Implemented 2,10,100
                    #existingTopicModel = loadDefaultTopicModel(workingFolder = '???',useModel = 'custom'),# If you want to use buildTopicModel, write your path
                    #existingTopicModel = NULL,# If you want to configure your targetCohortId.
                     useDictionary=FALSE,
                     limitedMedicalTermOnlyLanguage = c('KOR','ENG'),
                     nGram = c(1L),
                     buildTopidModelMinFrac = 0.001,
                     buildTopidModelMaxFrac = 0.5,
                     useTextToVec = FALSE,
                     useTopicModeling=TRUE,
                     optimalTopicValue =FALSE,
                     numberOfTopics=c(10L),
                     sampleSize=-1)


covariates <- FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                                    cohortDatabaseSchema = cdmDatabaseSchema,
                                                    cohortTable = "cohort",
                                                    cohortId = targetCohortId,
                                                    covariateSettings = covariateSettings)
</code></pre>
- A TopicModel rds file is created with the topicModelExportRds you entered.
createTopicFromNoteSettings Function Parameters
- **noteConceptId** : NOTE Column NOTE_TYPE_CONCEPT_ID in CDM
- **existingTopicModel** : 

|purpose|input|
|:--:|:--:|
|use a topicmodel already configured|loadDefaultTopicModel(NumberOfTopics = '???',useModel = 'base')|
|use your own topicmodel|loadDefaultTopicModel(workingFolder = '???',useModel = 'custom')|
|configure your targetCohort|NULL|
- **useDictionary** : Extract only the words contained in the dictionary
- **limitedMedicalTermOnlyLanguage** : Korean and English dictionaries are available.
- **nGram** : Used when using techniques that do not require word order
- **buildTopidModelMinFrac** : Minimum Frequency for the entire word
- **buildTopidModelMaxFrac** : Maximum Frequency for the entire word
- **useTextToVec** : TRUE to use text2vec when configuring covariates, No overlap with Topicmodel
- **useTopicModeling** : TRUE to use TopicModeling when configuring covariates, No overlap with text2vec
- **optimalTopicValue** : Find the optimal topic value. But it takes a long time. (When there is no Topicmdel)
- **NumberOfTopics** : Setting Custom Topic value.(When there is no Topicmdel)
- **sampleSize** : Enter the desired number of rows if you want to use the sample data, or -1 if you want to use the entire data.

## How to use PatientLevelPrediction Package
<pre><code>
covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                useDemographicsAgeGroup = TRUE,
                                             useDemographicsRace = TRUE,
                                             useDemographicsEthnicity = TRUE,
                                             useDemographicsIndexYear = TRUE,
                                             useDemographicsIndexMonth = TRUE)


noteCovSet <- CustomCovariateSetting::createTopicFromNoteSettings(useTopicFromNote = TRUE,
                                                                         useDictionary = TRUE,
                                                                         useTopicModeling = TRUE,
                                                                         noteConceptId = 44814637,
                                                                         numberOfTopics = 100,
                                                                         sampleSize = 1000
)


covariateSettingsList <- list(covariateSettings, noteCovSet)

covariates <-  FeatureExtraction::getDbCovariateData(connectionDetails = connectionDetails,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortDatabaseSchema = resultsDatabaseSchema,
                                 cohortTable = "cohort",
                                 cohortId = 747,
                                 covariateSettings = covariateSettingsList)



plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                      cdmDatabaseSchema = cdmDatabaseSchema,
                      oracleTempSchema = oracleTempSchema,
                      cohortDatabaseSchema = targetDatabaseSchema,
                      cohortTable = "cohort",
                      cohortId = cohortId,
                      washoutPeriod = 0,
                      covariateSettings = covariateSettingsList,
                      outcomeDatabaseSchema = resultsDatabaseSchema,
                      outcomeTable = "cohort",
                      outcomeIds = outcomeId,
                      cdmVersion = cdmVersion)

population <- PatientLevelPrediction::createStudyPopulation(plpData,
                                    outcomeId = outcomeId,
                                    includeAllOutcomes = TRUE,
                                    firstExposureOnly = FALSE,
                                    washoutPeriod = 0,
                                    removeSubjectsWithPriorOutcome = FALSE,
                                    riskWindowStart = 1,
                                    requireTimeAtRisk = FALSE,
                                    riskWindowEnd = 30)

lrModel <- setLassoLogisticRegression()
lrResults <- runPlp(population,plpData, modelSettings = lrModel, testSplit = 'person',
                    testFraction = 0.25, nfold = 2)
</code></pre>
