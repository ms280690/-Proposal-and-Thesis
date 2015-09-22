% DebugInfoGoodies.lhs
% Andreas Baldeau
% April 13, 2009

Overview
========

> module Curry.Debugger.DebugInfoGoodies (
>     staticInfoToSrcTuple,
>     getContext,
>     getModuleName,
>     getFuncArgs,
>     stringTermToString

     getFuncDeclName,
     getFuncDecl,

> ) where


> import Curry.Debugger.DebugInfo

General
=======

> staticInfoToSrcTuple :: StaticInfo -> (ModID, DefID)
> staticInfoToSrcTuple si = (modID si, defID si)

> getContext :: DI -> [(StaticInfo,Term)]
> getContext = context . dynamicInfo

Function handling
=================

getFuncDecl :: DI -> IO Decl
getFuncDecl i = do
    se <- getSrcAt $ staticInfoToSrcTuple $ staticInfo i
    case se of
        SrcDecl fd     -> return fd
        SrcExpr fd _ _ -> return fd
        _ -> error $ "unexpected source entity: " ++ show se

getFuncDeclName :: Decl -> String
getFuncDeclName (FunctionDecl _ i _) = name i
getFuncDeclName x = error $ show x

> getModuleName :: DI -> String
> getModuleName i = modID $ staticInfo i

> getFuncArgs :: DI -> [Term] 
> getFuncArgs i = currentValues $ dynamicInfo i

Converting Term-representations to their represented values
===========================================================

This function will only convert represented Strings to real Strings.

> stringTermToString :: Term -> String
> stringTermToString (Term _ _ []) = ""
> stringTermToString (Term _ _ [TermChar c, st]) = c : stringTermToString st

