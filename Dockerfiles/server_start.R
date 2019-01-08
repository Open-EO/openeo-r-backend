library(openEO.R.Backend)

config = ServerConfig()
config$workspaces.path = "/var/openeo/workspace"
config$initEnvironmentDefault()
config$initializeDatabase()

createServerInstance(configuration = config)

openeo.server$createUser(user_name="test", password="test") #only created if not exists
openeo.server$loadDemo()
openeo.server$startup(port = 8000,host="0.0.0.0")