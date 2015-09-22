import Curry.Files.KiCSDebugPath

main = do
  p <- getProphecy 
  m <- getMkstrict
  o <- getOracleLibDir
  print (debugVersion,p,m,o)
