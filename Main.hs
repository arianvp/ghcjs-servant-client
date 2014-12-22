import Servant.Common.Req

main = do
  a <- mkBinaryRequest' "GET" "http://localhost:8000" Nothing
  print a
