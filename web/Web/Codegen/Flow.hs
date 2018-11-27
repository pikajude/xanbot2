{-# Language RecordWildCards #-}
{-# Language FlexibleContexts #-}
{-# Language QuasiQuotes #-}

module Web.Codegen.Flow where

import           Web.Codegen
import qualified Data.Text.IO as T
import           NeatInterpolation
import Data.Maybe
import Data.Text (Text, pack, intercalate)
import Servant.Foreign.Inflections
import Web
import Data.Proxy
import Servant.Foreign

genModule = T.writeFile "flow/codegen/API.js" [text|
    // @flow strict
    // generated code

    export default {
      $exports
    }
    |]
  where
      exports = intercalate ",\n" $ map export (content (Proxy :: Proxy API))

export r@Req{..} = [text|
    $fname: async ($args): Promise<$returnty> => {
      const opts = {
        method: $meth,
        body: $bdy,
        headers: {
          "Content-Type": "application/json", $headersjson
        },
      };

      const resp = await fetch(`/api/${path}`, opts);
      if (resp.ok) {
        return await resp.json();
      } else if (resp.status === 401) {
        return Promise.reject(new Error('Unauthorized'));
      } else {
        return Promise.reject(new Error('Unknown response'));
      }
    }|]
  where
      meth = pack $ show _reqMethod
      fname = camelCase _reqFuncName
      args = intercalate ", " $ catMaybes $ bodyArg : map hdr _reqHeaders
      bdy = maybe "null" (const "JSON.stringify(body)") _reqBody
      bodyArg = case _reqBody of
                  Just ty -> Just [text|body: $ty|]
                  Nothing -> Nothing
      returnty = fromMaybe "*" _reqReturnType
      hdr (HeaderArg (Arg (PathSegment c) ty)) = Just [text|header${c}: $ty|]
      headersjson = intercalate ", " $ map (\(HeaderArg (Arg (PathSegment c) _)) -> [text|"$c": header${c}|]) _reqHeaders
      path = intercalate "/" $ map (\(Segment (Static (PathSegment p))) -> p) (_path _reqUrl)
