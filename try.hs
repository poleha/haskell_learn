import Network.HTTP
import Control.Applicative

get :: String -> IO String
get url = do
    response <- simpleHTTP $ getRequest url
    getResponseBody response

main = do
    responses <- fmap get ["http://www.alfredodinapoli.com", "http://www.alfredodinapoli.com"]

    --response <- fmap (take 100) (get "http://www.alfredodinapoli.com")

    print("ddd")