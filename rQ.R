# function encapsulating queries to remote MySQL retrosheet data database
# based on http://stackoverflow.com/questions/14528869/how-to-disconnect-cleanly-from-mysql-in-r
rQ = function(query) {
    require("DBI")
    require("RMySQL")

    output = tryCatch({
        con = dbConnect(MySQL(), user='mlbrsheetuser', password='PeRoSEpy',
            dbname='mlbretrosheet', host='mysql.bustos.org')
        dbGetQuery(con, query)
    }, warning = function(w) {
        print("warning!")
    }, error = function(e) {
        print("error encountered!")
        print(e)
    }, finally = {
        dbDisconnect(con)
        print("goodbye")
    })  
    return(output)
}

# following function not really used - the first version
rsQ <- function(sql) {
    require("DBI")
    require("RMySQL")

    mydb = dbConnect(MySQL(), user='mlbrsheetuser', password='PeRoSEpy',
        dbname='mlbretrosheet', host='mysql.bustos.org')

    rs <- dbSendQuery(mydb, paste(sql, ";", sep=""))
    data <- fetch(rs, n=-1)
    rm(mydb)
    data
}
