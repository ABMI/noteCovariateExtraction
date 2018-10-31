#' Custom createCoveriate Settings
#'
#' This function is Custom createCoveriate Settings.
#' @param Language
#' @keywordsa createCovariateSetting
#' @export
#' @examples
#' dictionaryForLanguage()
dictionaryForLanguage<-function(Language="KOR"){
    return(  switch(Language, "KOR"=kor_dictionary_db[,1], "ENG" = eng_dictionary_db[,1])  )
}
