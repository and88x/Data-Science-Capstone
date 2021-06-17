#
if (!dir.exists('final')){
    url = 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
    download.file(url,'./Coursera-SwiftKey.zip', mode = 'wb')
    unzip("Coursera-SwiftKey.zip", exdir = getwd())
    print("The dataset was downloaded successfully")
} else {
    print("The dataset was previously downloaded")
}
