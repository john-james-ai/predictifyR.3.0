td <- c("This isn't the New-York James' hotel!")
td1 <- quanteda::tokenize(td, what = 'word')
td2 <- quanteda::tokenize(td, what = 'fasterword')
td3 <- quanteda::tokenize(td, what = 'fastestword')
