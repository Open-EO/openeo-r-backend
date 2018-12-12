# authentication ----

.login_basic = function(req,res) {
  auth = req$HTTP_AUTHORIZATION
  encoded=substr(auth,7,nchar(auth))
  
  decoded = rawToChar(base64_dec(encoded))
  user_name = unlist(strsplit(decoded,":"))[1]
  user_pwd = unlist(strsplit(decoded,":"))[2]
  tryCatch(
    {  
      con = openeo.server$getConnection()
      result = dbGetQuery(con, "select * from user where user_name = :name limit 1",param=list(name=user_name))
      dbDisconnect(con)
      
      if (nrow(result) == 0) {
        throwError("CredentialsInvalid")
      }
      
      user = as.list(result)
      
      if (user$password == user_pwd) {
        encryption = data_encrypt(charToRaw(paste(user$user_id)),openeo.server$secret.key)
        
        token = bin2hex(append(encryption, attr(encryption,"nonce")))
        
        list(user_id = user$user_id, access_token=token)
      } else {
        throwError("CredentialsInvalid")
      }
    },
    error=handleError
  )
}

.login_oidc = function(req,res) {
  res$setHeader(name = "Location",
                value=openeo.server$oidcprovider.url)
  
  res$status = 303
}