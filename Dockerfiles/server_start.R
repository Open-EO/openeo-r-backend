library(openEO.R.Backend)
rm(openeo.server)
openeo.server$workspaces.path = "/var/openeo/workspace"
openeo.server$initEnvironmentDefault()
openeo.server$initializeDatabase()

openeo.server$createUser(user_name="test", password="test") #only created if not exists
openeo.server$loadDemo()
openeo.server$startup(port = 8000,host="0.0.0.0")