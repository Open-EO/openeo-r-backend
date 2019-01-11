library(openEO.R.Backend)

config = ServerConfig()
config$workspaces.path = "/var/openeo/workspace"
config$rudfservice.url = "http://r-udfserver:8010/udf"

createServerInstance(configuration = config)

openeo.server$initEnvironmentDefault()
openeo.server$initializeDatabase()
openeo.server$createUser(user_name="test", password="test") #only created if not exists
openeo.server$loadDemo()
openeo.server$startup(port = 8000,host="0.0.0.0")