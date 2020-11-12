# Function to read and write TidyTuesday data ----

read_TTdata <- function(week) {
    # main dir
    main_dir <- paste0('2020/week ', week, '/')
    data_dir <- paste0('2020/week ', week, '/data/')
    # create directories
    # - main folder
    ifelse(!dir.exists(main_dir),
           dir.create(main_dir),
           print(paste0('`Week ',week,'` Folder Already Exists')))
    
    # - data folder
    ifelse(!dir.exists(data_dir),
           dir.create(data_dir),
           print(paste0('`Week ',week,'/data`',' Folder Exists')))
    
    ifelse(
        length(list.files(data_dir)) == 0 &
            !exists('tuesdata') |
            length(list.files(data_dir)) == 0 ,
        assign(
            'tuesdata',
            tidytuesdayR::tt_load(2020, week = week),
            envir = globalenv()
        ),
        print('Data Already Exists')
    )
    
    
    # write and read data to directory
    if (exists('tuesdata'))
        n_sets = length(tuesdata) %>% as.numeric()
    #if (exists('tuesdata')) d_name <- names(tuesdata[1]) %>% as.numeric()
    
    if (exists('tuesdata') & length(list.files(data_dir)) == 0)
        
        print(paste0('Writing Data to the `Week ',week,'/data` Folder'))
    
    if (exists('tuesdata') & length(list.files(data_dir)) == 0)
        for (i in 1:n_sets) {
            dat_name <- names(tuesdata)[i]
            #write to dir
            readr::write_csv(tuesdata[[i]], file = paste0(data_dir, dat_name, '.csv'))
        }
    
    #read to environment
    if (length(list.files(data_dir)) != 0)
        
        print('Reading Data into R Environment')
        
        for (i in 1:length(list.files(data_dir))) {
            data_name <- list.files(data_dir)[[i]]
            assign(
                stringr::str_extract(data_name, '(\\w+)'),
                readr::read_csv(paste0(data_dir, data_name)),
                envir = globalenv()
            )
        }
}
