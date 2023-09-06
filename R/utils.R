cmkdir <- function(dir){
    if(!dir.exists(dir)){
        dir.create(dir)
        cli::cli_alert_success(paste(cli::col_blue(dir), "created!"))
    }
}

init_md5 <- function(ext = c("Rmd", "qmd", "R"), exclude = NULL, force = FALSE){
    cmkdir(".md5db")
    ext <- paste0(".", ext, "$") # adding regex
    files <- list.files(".", full.names = TRUE, pattern = paste0(ext, collapse = "|"), recursive = TRUE)
    files <- gsub("\\.\\/", "", files) # removing ./
    if(!is.null(exclude)){
        files <- files[!grepl(paste0(exclude, collapse = "|"), files)]
    }

    md5 <- sapply(files, tools::md5sum)
    db <- data.frame(file = files, md5 = md5)
    rownames(db) <- NULL

    if(file.exists(".md5db/md5.rds")){
        old_db <- readRDS(".md5db/md5.rds")
        new_files <- db[!db$file %in% old_db$file, ]
        db <- rbind(old_db, new_files)
    }
    saveRDS(db, ".md5db/md5.rds")
}

get_md5 <- function(file){
  db <- readRDS(".md5db/md5.rds")
  db$md5[db$file == file]
}

update_md5 <- function(file){
  db <- readRDS(".md5db/md5.rds")
  if(length(db$file) == 0){
    new_md5 <- data.frame(file = file, md5 = tools::md5sum(file))
    db <- rbind(db, new)
  }else{
    new_md5 <- tools::md5sum(file)
    db$md5[db$file == file] <- new_md5
  }
  saveRDS(db, ".md5db/md5.rds")
}