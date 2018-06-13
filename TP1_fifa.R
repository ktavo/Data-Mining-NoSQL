require("mongolite")
conection <- mongo(collection = "FIFA-dump_tweets", db = "fifa", url = "mongodb://localhost")
fullTwwetsQuery <- conection$find('{}')
seleTwwetsQuery <- conection$find('{"screen_name" : "FCFSeleccionCol"}')
