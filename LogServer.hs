{-# LANGUAGE OverloadedStrings #-}
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

app :: Application
app _ respond = respond $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, World!"

main = run 3000 app
