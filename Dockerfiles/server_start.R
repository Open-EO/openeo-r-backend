library(openEO.R.Backend)

config = ServerConfig()

config$workspaces.path = "/var/openeo/workspace"

r_udfservice_param = "R_SERVER_R_UDF_SERVICE_URL"
r_baseserver_param = "R_SERVER_BASE_URL"
oidc_provider_param  = "OIDC_PROVIDER_URL"
mapserver_url_param = "R_SERVER_MAPSERVER_URL"
r_server_app_port_param = "R_SERVER_PORT"
r_server_user_workspace_param = "R_SERVER_WORKSPACE_PATH"
r_server_data_path_param = "R_SERVER_DATA_PATH"
r_server_sqlite_path_param = "R_SERVER_SQLITE_PATH"
r_server_udf_transaction_path_param = "R_SERVER_UDF_TRANSACTION_PATH"
r_server_secret_key_param = "R_SERVER_SECRET"
r_user_param = "R_SERVER_USER"
r_user_pwd_param = "R_SERVER_USER_PASSWORD"


env_variables = names(Sys.getenv())

if (r_udfservice_param %in% env_variables) {
  config$rudfservice.url = Sys.getenv(r_udfservice_param)
} else {
  config$rudfservice.url = "http://r-udfserver:8010/udf"
}

if (r_baseserver_param %in% env_variables) {
  config$baseserver.url = Sys.getenv(r_baseserver_param)
}

if (oidc_provider_param %in% env_variables) {
  config$oidcprovider.url = Sys.getenv(oidc_provider_param)
}

if (mapserver_url_param %in% env_variables) {
  config$mapserver.url = Sys.getenv(mapserver_url_param)
}

if (r_server_user_workspace_param %in% env_variables) {
  config$workspaces.path = Sys.getenv(r_server_user_workspace_param)
}

if (r_server_data_path_param %in% env_variables) {
  config$data.path = Sys.getenv(r_server_data_path_param)
}

if (r_server_sqlite_path_param %in% env_variables) {
  config$sqlite.path = Sys.getenv(r_server_sqlite_path_param)
}

if (r_server_udf_transaction_path_param %in% env_variables) {
  config$udf_transactions.path = Sys.getenv(r_server_udf_transaction_path_param)
}

if (r_server_secret_key_param %in% env_variables) {
  config$secret.key = Sys.getenv(r_server_secret_key_param)
}

createServerInstance(configuration = config)

openeo.server$initEnvironmentDefault()
openeo.server$initializeDatabase()

if (r_user_param %in% env_variables && r_user_pwd_param %in% env_variables) {
  openeo.server$createUser(user_name=Sys.getenv(r_user_param), 
                           password=Sys.getenv(r_user_pwd_param)) #only created if not exists
} else {
  openeo.server$createUser(user_name="test", password="test") #only created if not exists  
}

openeo.server$loadDemo()

if (r_server_app_port_param %in% env_variables) port = Sys.getenv(r_server_app_port_param) else port = 8000

openeo.server$startup(port = port,host="0.0.0.0")