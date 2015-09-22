-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- CurryEnv - Generates a record containing extracted and prepared data
--            from a CurrySyntax module
--
-- November 2005,
-- Martin Engelke (men@informatik.uni-kiel.de)
--
module CurryEnv (CurryEnv(..), curryEnv) where

import Data.Maybe

import Curry.Base.Position
import Curry.Base.Ident

import Curry.Syntax

import Types
import Base


------------------------------------------------------------------------------

-- A record containing the following data for a module 'm':
--
--    moduleId    - the name of 'm'
--    exports     - the export list extracted from 'm'
--    interface   - all exported declarations in 'm' (including exported 
--                  imports)
--    infixDecls  - interfaces of all infix declarations in 'm'
--    typeSynonym - interfaces of all type synonyms in 'm'
--
data CurryEnv = CurryEnv{ moduleId     :: ModuleIdent,
			  exports      :: [Export],
			  imports      :: [IDecl],
			  interface    :: [IDecl],
			  infixDecls   :: [IDecl],
			  typeSynonyms :: [IDecl]
			} deriving Show
			  

-------------------------------------------------------------------------------

-- Returns a Curry environment for the module 'mod' and its corresponding
-- environments 'mEnv' (imported modules), 'tcEnv' (table of type
-- constructors) and 'intf' (the interface of 'mod')
curryEnv :: ModuleEnv -> TCEnv -> Interface -> Module -> CurryEnv
curryEnv mEnv tcEnv (Interface iid idecls) mod@(Module mid mExp decls)
   | iid == mid
     = CurryEnv{ moduleId     = mid,
		 exports      = maybe [] (\ (Exporting _ exps) -> exps) mExp,
		 imports      = genImportIntf decls,
		 interface    = idecls,
		 infixDecls   = genInfixDecls mod,
		 typeSynonyms = genTypeSyns tcEnv mod
	       }
   | otherwise
     = internalError ("CurryEnv: interface \"" ++ show iid 
		      ++ "\" does not match module \"" ++ show mid ++ "\"")


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Generate interfaces for import declarations
genImportIntf :: [Decl] -> [IDecl]
genImportIntf decls = reverse (map snd (foldl genImpIntf [] decls))

--
genImpIntf imps (ImportDecl pos mid _ _ _)
   = maybe ((mid, IImportDecl pos mid):imps) (const imps) (lookup mid imps)
genImpIntf imps _ = imps


-------------------------------------------------------------------------------

-- Generate interface declaration for all infix declarations in the module
genInfixDecls :: Module -> [IDecl]
genInfixDecls (Module mident _ decls) = collectIInfixDecls mident decls

--
collectIInfixDecls :: ModuleIdent -> [Decl] -> [IDecl]
collectIInfixDecls mident [] = []
collectIInfixDecls mident ((InfixDecl pos infixspec prec idents):decls)
   = (map (\ident 
	   -> IInfixDecl pos infixspec prec (qualifyWith mident ident)) 
	   idents)
     ++ (collectIInfixDecls mident decls)
collectIInfixDecls mident (_:decls) = collectIInfixDecls mident decls


-------------------------------------------------------------------------------

-- Generate interface declarations for all type synonyms in the module.
genTypeSyns :: TCEnv -> Module -> [IDecl]
genTypeSyns tcEnv (Module mident _ decls)
   = concatMap (genTypeSynDecl mident tcEnv) (filter isTypeSyn decls)

--
genTypeSynDecl :: ModuleIdent -> TCEnv -> Decl -> [IDecl]
genTypeSynDecl mid tcEnv (TypeDecl pos ident params texpr)
   = [genTypeDecl pos mid ident params tcEnv texpr]
genTypeSynDecl _ _ _ 
   = []

--
genTypeDecl :: Position -> ModuleIdent -> Ident -> [Ident] -> TCEnv
	    -> TypeExpr -> IDecl
genTypeDecl pos mid ident params tcEnv texpr
   = ITypeDecl pos (qualifyWith mid ident) params
               (modifyTypeExpr tcEnv texpr)


--
modifyTypeExpr :: TCEnv -> TypeExpr -> TypeExpr
modifyTypeExpr tcEnv (ConstructorType qident typeexprs)
   = case (qualLookupTC qident tcEnv) of
       [AliasType _ arity rhstype]
          -> modifyTypeExpr tcEnv 
	                    (genTypeSynDeref (zip [0 .. (arity-1)] typeexprs)
			                     rhstype)
       _  -> ConstructorType (fromMaybe qident (lookupTCId qident tcEnv))
                             (map (modifyTypeExpr tcEnv) typeexprs)
modifyTypeExpr _ (VariableType ident)
   = VariableType ident
modifyTypeExpr tcEnv (ArrowType type1 type2)
   = ArrowType (modifyTypeExpr tcEnv type1) (modifyTypeExpr tcEnv type2)
modifyTypeExpr tcEnv (TupleType typeexprs)
   | null typeexprs 
     = ConstructorType qUnitId []
   | otherwise
     = ConstructorType (qTupleId (length typeexprs)) 
                       (map (modifyTypeExpr tcEnv) typeexprs)
modifyTypeExpr tcEnv (ListType typeexpr)
   = ConstructorType (qualify listId) [(modifyTypeExpr tcEnv typeexpr)]
modifyTypeExpr tcEnv (RecordType fields rtype)
   = RecordType (map (\ (labs, texpr) -> (labs, (modifyTypeExpr tcEnv texpr)))
		     fields)
                (maybe Nothing (Just . modifyTypeExpr tcEnv) rtype)

--
genTypeSynDeref :: [(Int,TypeExpr)] -> Type -> TypeExpr
genTypeSynDeref its (TypeConstructor qident typeexprs)
   = ConstructorType qident (map (genTypeSynDeref its) typeexprs)
genTypeSynDeref its (TypeVariable i)
   = fromMaybe (internalError ("@CurryInfo.genTypeSynDeref: " ++
			       "unkown type var index"))
               (lookup i its)
genTypeSynDeref its (TypeConstrained typeexprs i)
   = internalError ("@CurryInfo.genTypeSynDeref: " ++
		    "illegal constrained type occured")
genTypeSynDeref its (TypeArrow type1 type2)
   = ArrowType (genTypeSynDeref its type1) (genTypeSynDeref its type2)
genTypeSynDeref its (TypeSkolem i)
   = internalError ("@CurryInfo.genTypeSynDeref: " ++
		    "illegal skolem type occured")
genTypeSynDeref its (TypeRecord fields ri)
   = RecordType (map (\ (lab, texpr) -> ([lab], genTypeSynDeref its texpr))
		     fields)
                (maybe Nothing 
		       (\i -> Just (genTypeSynDeref its (TypeVariable i)))
		       ri)

--
lookupTCId :: QualIdent -> TCEnv -> Maybe QualIdent
lookupTCId qident tcEnv
   = case (qualLookupTC qident tcEnv) of
       [DataType qident' _ _]     -> Just qident'
       [RenamingType qident' _ _] -> Just qident'
       [AliasType qident' _ _]    -> Just qident'
       _                          -> Nothing



isTypeSyn :: Decl -> Bool
isTypeSyn (TypeDecl _ _ _ texpr)
   = case texpr of
       RecordType _ _ -> False
       _              -> True
isTypeSyn _ = False
