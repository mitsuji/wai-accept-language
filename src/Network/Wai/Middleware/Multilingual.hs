module Network.Wai.Middleware.Multilingual
       ( contentLanguage, rewriteByAcceptLanguage
       ) where



import qualified Network.Wai as Wai
import Network.HTTP.Types.Header(hAcceptLanguage)
import Network.Wai.Middleware.Rewrite(rewritePure)
import Network.Wai.Parse(parseHttpAccept)
import Data.Text.Encoding(decodeLatin1)
import Data.Maybe(fromMaybe)
import Data.List(find)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Word8(_hyphen)



-- | rewrite based on content:language list and Accept-Language header 
rewriteByAcceptLanguage :: [([T.Text],[BS.ByteString])] -> Wai.Middleware
rewriteByAcceptLanguage preparedContents = rewritePure trans
  where
    trans path headers =
      let path' = do
            preparedLanguages <- lookup path preparedContents
            acceptLanguageHeaderValue <- lookup hAcceptLanguage headers
            language <- contentLanguage preparedLanguages acceptLanguageHeaderValue
            return $ decodeLatin1 language : path
      in fromMaybe path path'



-- | determine language based on language list and Accept-Language header 
contentLanguage :: [BS.ByteString] -> BS.ByteString -> Maybe BS.ByteString
contentLanguage preparedLanguages acceptLanguageHeaderValue = 
  let
    acceptLanguages  = parseHttpAccept acceptLanguageHeaderValue
    acceptLanguages' = map takeWhileNotHyphen acceptLanguages
  in find (\al -> elem al preparedLanguages) acceptLanguages'
  where
    takeWhileNotHyphen = BS.takeWhile (_hyphen /=)


