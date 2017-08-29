get.file2 <- function(name, link = "http://capa.ddb.umu.se/data2/"){
    filename = paste("data2/", name, ".rda", sep = "")
    if (file.exists(filename)){
        load(filename)
    }else{
        con <- url(paste(link, name, ".rda", sep = ""))
        load(con)
        close(con)
        save(list = name, file = filename)
    }
    get(name)
}