#------------------------------------------------------------------------------#
#                               testPackage                                    #
#------------------------------------------------------------------------------#
#'  testPackage
#'
#'  \code{testPackage} Tests the analysis pipeline
#'
#' This function takes as its parameter, the meta data for the raw and clean
#' corpora and runs the analysis pipeline and logs the results.
#'
#' @param prod Logical indicating whether to use production data vs. test
#' @author John James, \email{j2sdatalab@@gmail.com}
testPackage <- function(prod = FALSE) {

  testCleanCorpus <- function(raw, clean) {
    cleanCorpus(raw, clean)
  }

  testAnalyzeCorpus <- function(clean) {
    analysis <- analyzeCorpus(clean)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'clean-corpus-analysis-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'analysis'
    logResults(analysis, fileName, objName)
    return(analysis)
  }

  testVGCFast <- function(clean) {
    vgc <- createVGC(clean)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'fast-vgc-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'vgc'
    logResults(vgc, fileName, objName)
    return(vgc)
  }

  testVGCFull <- function(clean) {
    vgc <- createVGC(clean, type = 'full')
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'full-vgc-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'vgc'
    logResults(vgc, fileName, objName)
    return(vgc)
  }

  testZipf <- function(clean, vgc) {
    zipf <- createZipf(clean, vgc)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'zipf-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'zipf'
    logResults(zipf, fileName, objName)
    return(zipf)
  }

  testEstSampleSize <- function(clean, analysis) {
    ss <- estimateSampleSize(clean, analysis)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'sample-size-estimate-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'sampleSize'
    logResults(ss, fileName, objName)
    return(ss)
  }

  testEstSamplingUnit <- function(clean, analysis) {
    su <- estimateSamplingUnit(clean)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'sampling-unit-estimate-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'samplingUnit'
    logResults(su, fileName, objName)
    return(su)
  }

  testEstCorpusSize <- function(clean) {
    sampleSize <- 2000
    numSamples <- 100
    if (grepl('test',clean$fileName)) {
      sampleSize <- 200
      numSamples <- 10
    }

    cs <- estimateCorpusSize(clean, sampleSize, numSamples)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'corpus-size-estimate-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'corpusSize'
    logResults(cs, fileName, objName)
    return(cs)
  }

  testEstRegisterSize <- function(clean, corpusSize, samplingUnit) {
    sampleSize <- 2000
    numSamples <- 100
    if (grepl('test',clean$fileName)) {
      sampleSize <- 200
      numSamples <- 10
    }

    rs <- estimateRegisterSize(clean, corpusSize, samplingUnit,
                               sampleSize, numSamples)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'register-size-estimate-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'registerSize'
    logResults(rs, fileName, objName)
    return(rs)
  }

  testDesign <- function(sampleSizeEstimate, registerSizeEstimate,
                         samplingUnit, analysis) {
    design <- designModelCorpora(sampleSizeEstimate, registerSizeEstimate,
                                 samplingUnit, analysis)
    fileName <- paste0(sub('\\..*', '', paste0('')),
                       'model-corpora-design-', clean$fileName,
                       format(Sys.time(),'_%Y%m%d_%H%M%S'), '.Rdata')
    objName <- 'design'
    logResults(design, fileName, objName)
    return(design)
  }

  testPilot <- function(clean, pilot, design) {
    buildPilot(clean, pilot, design)
  }


  # Core processing
  if (prod) {
    raw <- corpora$raw
    clean <- corpora$clean
    pilot <- corpora$pilot
  } else {
    raw <- corpora$test$raw
    clean <- corpora$test$clean
  }
  futile.logger::flog.logger("green", INFO, appender=appender.tee('./log/green.log'))
  testCleanCorpus(raw, clean)
  analysis <<- testAnalyzeCorpus(clean)
  # vgcFast <<- testVGCFast(clean)
  # zipf <<- testZipf(clean, vgcFast)
  # ss <<- testEstSampleSize(clean, analysis)
  # su <<- testEstSamplingUnit(clean, analysis)
  # cs <<- testEstCorpusSize(clean)
  # rs <<- testEstRegisterSize(clean, cs, su)
  # design <<- testDesign(ss, rs, su, analysis)
  # testPilot(clean, pilot, design)
  futile.logger::flog.logger("green", INFO, appender=appender.file('./log/green.log'))
}

testPackage()

