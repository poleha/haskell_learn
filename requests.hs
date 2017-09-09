import Network.HTTP
import Control.Applicative

get :: String -> IO String
get url = do
    response <- simpleHTTP $ getRequest url
    getResponseBody response

main = take 204 <$> get "http://www.alfredodinapoli.com" >>= print