module SyntaxColoring (Program,Code(..),TypeKind(..),ConstructorKind(..),
                       IdentifierKind(..),FunctionKind(..), genProgram,
                       code2string,getQualIdent, position2code,
                       area2codes) where

import Debug.Trace
import Data.Function(on)

import Data.Maybe
import Data.Either
import Data.List
import Data.Char hiding(Space)

import Curry.Base.Position
import Curry.Base.Ident
import Curry.Base.MessageMonad
import Curry.Syntax 
import Curry.Syntax.Lexer



debug = False -- mergen von Token und Codes

trace' s x = if debug then trace s x else x


debug' = False -- messages

trace'' s x = if debug' then trace s x else x

type Program = [(Int,Int,Code)] 

data Code =  Keyword String
           | Space Int
           | NewLine
           | ConstructorName  ConstructorKind QualIdent
           | TypeConstructor  TypeKind QualIdent
           | Function FunctionKind QualIdent
           | ModuleName ModuleIdent
           | Commentary String
           | NumberCode String
           | StringCode String
           | CharCode String
           | Symbol String
           | Identifier IdentifierKind QualIdent
           | CodeWarning [WarnMsg] Code
           | NotParsed String
             deriving Show
           
data TypeKind = TypeDecla
              | TypeUse
              | TypeExport deriving Show          

data ConstructorKind = ConstrPattern
                     | ConstrCall
                     | ConstrDecla
                     | OtherConstrKind deriving Show
                     
data IdentifierKind = IdDecl
                    | IdOccur
                    | UnknownId  deriving Show         
                      
data FunctionKind = InfixFunction
                  | TypSig
                  | FunDecl
                  | FunctionCall
                  | OtherFunctionKind deriving Show      
                  
                  
     
        
--- @param plaintext
--- @param list with parse-Results with descending quality  e.g. [typingParse,fullParse,parse]                                        
--- @param lex-Result
--- @return program
genProgram :: String -> [MsgMonad Module] -> MsgMonad [(Position,Token)] -> Program       
genProgram plainText parseResults m
    = case runMsg m of
        (Left e, msgs) -> buildMessagesIntoPlainText (e : msgs) plainText
        (Right posNtokList, mess) 
            -> let messages = (prepareMessages (concatMap getMessages parseResults ++ mess))
                   mergedMessages = (mergeMessages' (trace' ("Messages: " ++ show messages) messages) posNtokList)
                   (nameList,codes) = catIdentifiers parseResults
               in tokenNcodes2codes nameList 1 1 mergedMessages codes

    
--- @param Program
--- @param line
--- @param col
--- @return Code at this Position                  
position2code :: Program -> Int -> Int -> Maybe Code                 
position2code []  _ _ = Nothing
position2code [_] _ _ = Nothing
position2code ((l,c,code):xs@((_,c2,_):_)) line col
     | line == l && col >= c && col < c2 = Just code
     | l > line = Nothing
     | otherwise = position2code xs line col
     
area2codes :: Program -> Position -> Position -> [Code]     
area2codes [] _ _ = []
area2codes xxs@((l,c,code):xs) p1@Position{file=file} p2 
     | p1 > p2 = area2codes xxs p2 p1
     | posEnd >= p1 && posBegin <= p2  = code : area2codes xs p1 p2
     | posBegin > p2 = []
     | otherwise = area2codes xs p1 p2
   where
      posBegin = Position file l c noRef
      posEnd   = Position file l (c + length (code2string code)) noRef
                  
  
--- @param code
--- @return qualIdent if available                   
getQualIdent :: Code -> Maybe QualIdent
getQualIdent (ConstructorName _ qualIdent) = Just qualIdent
getQualIdent (Function _ qualIdent) = Just qualIdent
getQualIdent (Identifier _ qualIdent) = Just qualIdent                      
getQualIdent (TypeConstructor _ qualIdent) = Just qualIdent
getQualIdent  _ = Nothing                  
                  
                    
-- DEBUGGING----------- wird bald nicht mehr gebraucht

setMessagePosition :: WarnMsg -> WarnMsg
setMessagePosition m@(WarnMsg (Just p) _) = trace'' ("pos:" ++ show p ++ ":" ++ show m) m
setMessagePosition (WarnMsg _ m) = 
        let mes@(WarnMsg pos _) =  (WarnMsg (getPositionFromString m) m) in
        trace'' ("pos:" ++ show pos ++ ":" ++ show mes) mes

getPositionFromString :: String -> Maybe Position
getPositionFromString message =
     if line > 0 && col > 0 
          then Just Position{file=file,line=line,column=col,astRef=noRef}
          else Nothing
  where
      file = takeWhile (/= '"') (tail (dropWhile (/= '"') message))
      line = readInt (takeWhile (/= '.') (drop 7 (dropWhile (/= ',') message)))
      col = readInt (takeWhile (/= ':') (tail (dropWhile (/= '.') (drop 7 (dropWhile (/= ',') message)))))
      
     
readInt :: String -> Int 
readInt s = 
      let onlyNum = filter isDigit s in
      if null onlyNum
         then 0
         else read onlyNum :: Int

-- -------------------------

  

-- -------------------------


flatCode :: Code -> Code
flatCode (CodeWarning _ code) = code
flatCode code = code
             

                 
-- ----------Message---------------------------------------                  
                  

getMessages :: MsgMonad a -> [WarnMsg]
getMessages = snd . runMsg --(Result mess _) = mess
-- getMessages (Failure mess) = mess

lessMessage :: WarnMsg -> WarnMsg -> Bool
lessMessage (WarnMsg mPos1 _) (WarnMsg mPos2 _) = mPos1 < mPos2

nubMessages :: [WarnMsg] -> [WarnMsg] 
nubMessages = nubBy eqMessage

eqMessage :: WarnMsg -> WarnMsg -> Bool
eqMessage (WarnMsg p1 s1) (WarnMsg p2 s2) = (p1 == p2) && (s1 == s2)

prepareMessages :: [WarnMsg] -> [WarnMsg]   
prepareMessages = qsort lessMessage . map setMessagePosition . nubMessages


buildMessagesIntoPlainText :: [WarnMsg] -> String -> Program
buildMessagesIntoPlainText messages text = 
    buildMessagesIntoPlainText' messages (lines text) [] 1
 where
    buildMessagesIntoPlainText' :: [WarnMsg] -> [String] -> [String] -> Int -> Program
    buildMessagesIntoPlainText' _ [] [] _ = 
          []
    buildMessagesIntoPlainText' _ [] postStrs line = 
          [(line,1,NotParsed (unlines postStrs))]    
    buildMessagesIntoPlainText' [] preStrs postStrs line = 
          [(line,1,NotParsed (unlines (preStrs ++ postStrs)))]  
            
    buildMessagesIntoPlainText' messages (str:preStrs) postStrs ln = 
          let (pre,post) = partition isLeq messages in
          if null pre 
             then buildMessagesIntoPlainText' post preStrs (postStrs ++ [str]) (ln + 1)
             else (ln,1,NotParsed (unlines postStrs)) : 
                  (ln,1,CodeWarning pre (NotParsed str)) :
                  (ln,1,NewLine) :
                  buildMessagesIntoPlainText' post preStrs [] (ln + 1)
      where 
         isLeq (WarnMsg (Just p) _) = line p <= ln 
         isLeq _ = True
                
        
        

     
--- @param parse-Modules  [typingParse,fullParse,parse] 
catIdentifiers :: [MsgMonad Module] -> ([(ModuleIdent,ModuleIdent)],[Code])
catIdentifiers = catIds . rights_sc . map (fst . runMsg)
    where 
      catIds [] = ([],[])
      catIds [m] =
          catIdentifiers' m Nothing
      catIds rs@(m:y:ys) =  
          catIdentifiers' (last rs) (Just m)
    
-- not in base befoer base4

rights_sc  xs = [ x | Right x <- xs]

--- @param parse-Module
--- @param Maybe betterParse-Module    
catIdentifiers' :: Module -> Maybe Module -> ([(ModuleIdent,ModuleIdent)],[Code])
catIdentifiers' (Module moduleIdent maybeExportSpec decls)
                Nothing =
      let codes = (concatMap decl2codes (qsort lessDecl decls)) in
      (concatMap renamedImports decls,      
      ModuleName moduleIdent :
       maybe [] exportSpec2codes maybeExportSpec ++ codes)
catIdentifiers' (Module moduleIdent maybeExportSpec1 _)
                (Just (Module _ maybeExportSpec2 decls)) =
      let codes = (concatMap decl2codes (qsort lessDecl decls)) in
      (concatMap renamedImports decls,
      replaceFunctionCalls $ 
        map (addModuleIdent moduleIdent)
          ([ModuleName moduleIdent] ++
           mergeExports2codes  
              (maybe [] (\(Exporting _ i) -> i)  maybeExportSpec1)
              (maybe [] (\(Exporting _ i) -> i)  maybeExportSpec2) ++
           codes))     
  
     
renamedImports :: Decl -> [(ModuleIdent,ModuleIdent)]
renamedImports decl =
    case decl of
        (ImportDecl _ oldName _ (Just newName) _) -> [(oldName,newName)]
        _ -> []
   
                      
replaceFunctionCalls :: [Code] -> [Code]                  
replaceFunctionCalls codes = map (idOccur2functionCall qualIdents) codes
   where
      qualIdents = findFunctionDecls codes
                                              

findFunctionDecls :: [Code] -> [QualIdent]
findFunctionDecls  =  mapMaybe getQualIdent . 
                      filter isFunctionDecl .                       
                      map flatCode                   

isFunctionDecl  :: Code -> Bool
isFunctionDecl  (Function FunDecl _)  = True
isFunctionDecl  _ = False  

idOccur2functionCall :: [QualIdent] -> Code -> Code
idOccur2functionCall qualIdents ide@(Identifier IdOccur qualIdent)  
   | isQualified qualIdent = Function FunctionCall qualIdent
   | elem qualIdent qualIdents = Function FunctionCall qualIdent
   | otherwise = ide
idOccur2functionCall qualIdents (CodeWarning mess code) =
       CodeWarning mess (idOccur2functionCall qualIdents code)
idOccur2functionCall _ code = code
  

addModuleIdent :: ModuleIdent -> Code -> Code
addModuleIdent moduleIdent c@(Function x qualIdent) 
    | uniqueId (unqualify qualIdent) == 0 =
        Function x (qualQualify moduleIdent qualIdent)
    | otherwise = c
addModuleIdent moduleIdent cn@(ConstructorName x qualIdent) 
    | not $ isQualified qualIdent =
        ConstructorName x (qualQualify moduleIdent qualIdent)
    | otherwise = cn       
addModuleIdent moduleIdent tc@(TypeConstructor TypeDecla qualIdent) 
    | not $ isQualified qualIdent =
        TypeConstructor TypeDecla (qualQualify moduleIdent qualIdent)
    | otherwise = tc         
addModuleIdent moduleIdent (CodeWarning mess code) =
    CodeWarning mess (addModuleIdent moduleIdent code)
addModuleIdent _ c = c
                        
-- ----------------------------------------

mergeMessages' :: [WarnMsg] -> [(Position,Token)] -> [([WarnMsg],Position,Token)]
mergeMessages' _ [] = []
mergeMessages' [] ((p,t):ps) = ([],p,t) : mergeMessages' [] ps
mergeMessages' mss@(m@(WarnMsg mPos x):ms) ((p,t):ps)  
    | mPos <= Just p = trace' (show mPos ++ " <= " ++ show (Just p) ++ " Message: " ++ x) ([m],p,t) : mergeMessages' ms ps 
    | otherwise = ([],p,t) : mergeMessages' mss ps


tokenNcodes2codes :: [(ModuleIdent,ModuleIdent)] -> Int -> Int -> [([WarnMsg],Position,Token)] -> [Code] -> [(Int,Int,Code)]
tokenNcodes2codes _ _ _ [] _ = []          
tokenNcodes2codes nameList currLine currCol toks@((messages,pos@Position{line=line,column=col},token):ts) codes 
    | currLine < line = 
           trace' " NewLine: "
           ((currLine,currCol,NewLine) :
           tokenNcodes2codes nameList (currLine + 1) 1 toks codes)
    | currCol < col =  
           trace' (" Space " ++ show (col - currCol))
           ((currLine,currCol,Space (col - currCol)) :         
           tokenNcodes2codes nameList currLine col toks codes)
    | isTokenIdentifier token && null codes =    
           trace' ("empty Code-List, Token: " ++ show (line,col) ++ show token)
           (addMessage [(currLine,currCol,NotParsed tokenStr)] ++ tokenNcodes2codes nameList newLine newCol ts codes)
    | not (isTokenIdentifier token) = 
           trace' (" Token ist kein Identifier: " ++ tokenStr ) 
           (addMessage [(currLine,currCol,token2code token)] ++ tokenNcodes2codes nameList newLine newCol ts codes) 
    | tokenStr == code2string (head codes) =
           trace' (" Code wird genommen: " ++ show (head codes) )
           (addMessage [(currLine,currCol,head codes)] ++ tokenNcodes2codes nameList newLine newCol ts (tail codes)) 
    | tokenStr == code2qualString (renameModuleIdents nameList (head codes)) =
           let mIdent = maybe Nothing rename (getModuleIdent (head codes)) 
               lenMod = maybe 0 (length . moduleName) mIdent
               startPos = maybe currCol (const (currCol + lenMod + 1)) mIdent
               symbol = [(currLine,currCol + lenMod,Symbol ".")]               
               prefix = maybe [] 
                              ( (: symbol) . 
                                ( \i -> (currLine,
                                         currCol,
                                         ModuleName i))) 
                              mIdent in
           trace' (" Code wird genommen: " ++ show (head codes) )
           (addMessage (prefix ++ [(currCol,startPos,head codes)]) ++ tokenNcodes2codes nameList newLine newCol ts (tail codes))           
    | elem tokenStr (codeQualifiers (head codes)) =
           trace' (" Token: "++ tokenStr ++" ist Modulname von: " ++ show (head codes) )
           (addMessage [(currLine,currCol,ModuleName (mkMIdent [tokenStr]))] ++ 
                    tokenNcodes2codes nameList newLine newCol ts codes)                  
    | otherwise = 
           trace' (" Token: "++ 
                   tokenStr ++
                   ",Code faellt weg:" ++ 
                   code2string (head codes) ++ 
                   "|" ++ 
                   code2qualString (head codes))
           (tokenNcodes2codes nameList currLine currCol toks (tail codes))
  where
      tokenStr = token2string token            
      newLine  = (currLine + length (lines tokenStr)) - 1 
      newCol   = currCol + length tokenStr   

      rename mid = Just $ fromMaybe mid (lookup mid nameList)

      addMessage [] = []
      addMessage ((l,c,code):cs)
         | null messages = (l,c,code):cs
         | otherwise = trace' ("Warning bei code: " ++ show codes ++ ":" ++ show messages) 
                              ((l,c,CodeWarning messages code): addMessage cs)
      
      
renameModuleIdents :: [(ModuleIdent,ModuleIdent)] -> Code -> Code
renameModuleIdents nameList c =
    case c of
        Function x qualIdent -> Function x (rename qualIdent (qualidMod qualIdent))
        Identifier x qualIdent -> Identifier x (rename qualIdent (qualidMod qualIdent))
        _ -> c
  where
    rename x (Nothing) = x
    rename x (Just m) = maybe x (\ m' -> qualifyWith m' (qualidId x)) (lookup m nameList)
           
{-
codeWithoutUniqueID ::  Code -> String
codeWithoutUniqueID code = maybe (code2string code) (name . unqualify) $ getQualIdent code
     

codeUnqualify :: Code -> Code
codeUnqualify code = maybe code (setQualIdent code . qualify . unqualify)  $ getQualIdent code  
-}  
          
codeQualifiers :: Code -> [String]
codeQualifiers = maybe [] moduleQualifiers . getModuleIdent

getModuleIdent :: Code -> Maybe ModuleIdent
getModuleIdent (ConstructorName _ qualIdent) = qualidMod qualIdent
getModuleIdent (Function _ qualIdent) = qualidMod qualIdent
getModuleIdent (ModuleName moduleIdent) = Just moduleIdent
getModuleIdent (Identifier _ qualIdent) = qualidMod qualIdent                     
getModuleIdent (TypeConstructor _ qualIdent) = qualidMod qualIdent
getModuleIdent _ = Nothing


  
{-
setQualIdent :: Code -> QualIdent -> Code
setQualIdent (Keyword str) _ = (Keyword str)
setQualIdent (Space i) _ = (Space i)
setQualIdent NewLine _ = NewLine
setQualIdent (ConstructorName kind _) qualIdent = (ConstructorName kind qualIdent)
setQualIdent (Function kind _) qualIdent = (Function kind qualIdent)
setQualIdent (ModuleName moduleIdent) _ = (ModuleName moduleIdent)
setQualIdent (Commentary str) _ = (Commentary str)
setQualIdent (NumberCode str) _ = (NumberCode str)
setQualIdent (Symbol str) _ = (Symbol str)
setQualIdent (Identifier kind _) qualIdent = (Identifier kind qualIdent)                      
setQualIdent (TypeConstructor kind _) qualIdent = (TypeConstructor kind qualIdent)
setQualIdent (StringCode str) _ = (StringCode str)                                 
setQualIdent (CharCode str) _ = (CharCode str)             
-}
                  
code2string (Keyword str) = str
code2string (Space i)= concat (replicate i " ")
code2string NewLine = "\n"
code2string (ConstructorName _ qualIdent) = name $ unqualify qualIdent
code2string (Function _ qualIdent) = name $ unqualify qualIdent
code2string (ModuleName moduleIdent) = moduleName moduleIdent
code2string (Commentary str) = str
code2string (NumberCode str) = str
code2string (Symbol str) = str
code2string (Identifier _ qualIdent) = name $ unqualify qualIdent                      
code2string (TypeConstructor _ qualIdent) = name $ unqualify qualIdent
code2string (StringCode str) = str                                 
code2string (CharCode str) = str
code2string (NotParsed str) = str
code2string _ = "" -- error / warning
 
code2qualString (ConstructorName _ qualIdent) = qualName qualIdent
code2qualString (Function _ qualIdent) = qualName qualIdent
code2qualString (Identifier _ qualIdent) = qualName qualIdent                      
code2qualString (TypeConstructor _ qualIdent) = qualName qualIdent
code2qualString x = code2string x



token2code :: Token -> Code
token2code tok@(Token cat _)
    | elem cat [IntTok,FloatTok,IntegerTok]
         = NumberCode (token2string tok)
    | elem cat [KW_case,KW_choice,KW_data,KW_do,KW_else,KW_eval,KW_external,
                KW_free,KW_if,KW_import,KW_in,KW_infix,KW_infixl,KW_infixr,
                KW_let,KW_module,KW_newtype,KW_of,KW_rigid,KW_then,KW_type,
                KW_where,Id_as,Id_ccall,Id_forall,Id_hiding,Id_interface,Id_primitive,
                Id_qualified]
         =  Keyword (token2string tok)
    | elem cat [LeftParen,RightParen,Semicolon,LeftBrace,RightBrace,LeftBracket,
                RightBracket,Comma,Underscore,Backquote,
                At,Colon,DotDot,DoubleColon,Equals,Backslash,Bar,LeftArrow,RightArrow,
                Tilde]
         = Symbol (token2string tok)
    | elem cat [LineComment, NestedComment]
         = Commentary (token2string tok)
    | isTokenIdentifier tok
         = Identifier UnknownId $ qualify $ mkIdent $ token2string tok
    | cat == StringTok 
         = StringCode (token2string tok)
    | cat == CharTok
         = CharCode (token2string tok)          
    | elem cat [EOF,VSemicolon,VRightBrace] = Space 0 
    
isTokenIdentifier :: Token -> Bool
isTokenIdentifier (Token cat _) = 
  elem cat [Id,QId,Sym,QSym,Sym_Dot,Sym_Minus,Sym_MinusDot]
    
-- DECL Position

getPosition :: Decl -> Position
getPosition (ImportDecl pos _ _ _ _) = pos     
getPosition (InfixDecl pos _ _ _) = pos     
getPosition (DataDecl pos _ _ _) = pos     
getPosition (NewtypeDecl pos _ _ _) = pos
getPosition (TypeDecl pos _ _ _) = pos   
getPosition (TypeSig pos _ _) = pos    
getPosition (EvalAnnot pos _ _) = pos
getPosition (FunctionDecl pos _ _) = pos    
getPosition (ExternalDecl pos _ _ _ _) = pos
getPosition (FlatExternalDecl pos _) = pos    
getPosition (PatternDecl pos _ _) = pos    
getPosition (ExtraVariables pos _) = pos
             

lessDecl :: Decl -> Decl -> Bool
lessDecl = (<) `on` getPosition

qsort _ []     = []
qsort less (x:xs) = qsort less [y | y <- xs, less y x] ++ [x] ++ qsort less [y | y <- xs, not $ less y x]



-- DECL TO CODE -------------------------------------------------------------------- 



exportSpec2codes ::  ExportSpec -> [Code]
exportSpec2codes (Exporting _ exports) = concatMap (export2codes [])  exports

--- @param parse-Exports
--- @param betterParse-Exports
mergeExports2codes :: [Export] -> [Export]  -> [Code]
mergeExports2codes [] _ = []
mergeExports2codes (e:es) xs = concatMap (export2codes xs)  (e:es)


export2codes :: [Export] -> Export -> [Code]
export2codes exports e@(Export qualIdent) 
    | length (filter checkDouble exports) /= 1 =      
       [Identifier UnknownId qualIdent]
    | otherwise =
       let [export] = (filter checkDouble exports) in
       export2c export     
  where    
    checkDouble (ExportTypeWith q _) = eqQualIdent qualIdent q
    checkDouble (Export q) = eqQualIdent qualIdent q
    checkDouble _ = False
    
    eqQualIdent q1 q2 
      | q1 == q2 = True
      | not (isQualified q1) = unqualify q1 == unqualify q2
      | otherwise = False
      
    export2c (Export qualIdent) = 
         [Function OtherFunctionKind qualIdent]
    export2c _ = 
         [TypeConstructor TypeExport qualIdent]
         
    
    
       
export2codes _ (ExportTypeWith qualIdent idents) = 
     TypeConstructor TypeExport qualIdent : map (Function OtherFunctionKind . qualify) idents
export2codes _ (ExportTypeAll  qualIdent) = 
     [TypeConstructor TypeExport qualIdent]  
export2codes _ (ExportModule moduleIdent) = 
     [ModuleName moduleIdent]

decl2codes :: Decl -> [Code]            
decl2codes (ImportDecl _ moduleIdent xQualified mModuleIdent importSpec) = 
     [ModuleName moduleIdent] ++
     maybe [] ((:[]) . ModuleName) mModuleIdent ++
     maybe [] (importSpec2codes moduleIdent)  importSpec
decl2codes (InfixDecl _ _ _ idents) =
     map (Function InfixFunction . qualify) idents
decl2codes (DataDecl _ ident idents constrDecls) =
     TypeConstructor TypeDecla (qualify ident) : 
     map (Identifier UnknownId . qualify) idents ++
     concatMap constrDecl2codes constrDecls
decl2codes (NewtypeDecl xPosition xIdent yIdents xNewConstrDecl) =
     []
decl2codes (TypeDecl _ ident idents typeExpr) =
     TypeConstructor TypeDecla (qualify ident) : 
     map (Identifier UnknownId . qualify) idents ++ 
     typeExpr2codes typeExpr
decl2codes (TypeSig _ idents typeExpr) =
     map (Function TypSig . qualify) idents ++ typeExpr2codes typeExpr   
decl2codes (EvalAnnot xPosition idents xEvalAnnotation) =
     map (Function FunDecl . qualify) idents
decl2codes (FunctionDecl _ _ equations) =
     concatMap equation2codes equations  
decl2codes (ExternalDecl xPosition xCallConv xString xIdent xTypeExpr) =
     []
decl2codes (FlatExternalDecl _ idents) =
     map (Function FunDecl . qualify) idents   
decl2codes (PatternDecl xPosition constrTerm rhs) =
     constrTerm2codes constrTerm ++ rhs2codes rhs
decl2codes (ExtraVariables _ idents) =
     map (Identifier IdDecl . qualify) idents
  
equation2codes :: Equation -> [Code]
equation2codes (Equation _ lhs rhs) =
     lhs2codes lhs ++ rhs2codes rhs
     
lhs2codes :: Lhs -> [Code]
lhs2codes (FunLhs ident constrTerms) =
    Function FunDecl (qualify ident) : concatMap constrTerm2codes constrTerms
lhs2codes (OpLhs constrTerm1 ident constrTerm2) =
    constrTerm2codes constrTerm1 ++ [Function FunDecl $ qualify ident] ++ constrTerm2codes constrTerm2
lhs2codes (ApLhs lhs constrTerms) =
    lhs2codes lhs ++ concatMap constrTerm2codes constrTerms     

rhs2codes :: Rhs -> [Code]
rhs2codes (SimpleRhs _ expression decls) =
    expression2codes expression ++ concatMap decl2codes decls
rhs2codes (GuardedRhs condExprs decls) =
    concatMap condExpr2codes condExprs ++ concatMap decl2codes decls
    
condExpr2codes :: CondExpr -> [Code]
condExpr2codes (CondExpr _ expression1 expression2) =   
   expression2codes expression1 ++ expression2codes expression2    
    
constrTerm2codes :: ConstrTerm -> [Code]
constrTerm2codes (LiteralPattern literal) = []
constrTerm2codes (NegativePattern ident literal) = []
constrTerm2codes (VariablePattern ident) = [Identifier IdDecl (qualify ident)]
constrTerm2codes (ConstructorPattern qualIdent constrTerms) =
    ConstructorName ConstrPattern qualIdent : concatMap constrTerm2codes constrTerms
constrTerm2codes (InfixPattern constrTerm1 qualIdent constrTerm2) =
    constrTerm2codes constrTerm1 ++ [ConstructorName ConstrPattern qualIdent] ++ constrTerm2codes constrTerm2
constrTerm2codes (ParenPattern constrTerm) = constrTerm2codes constrTerm
constrTerm2codes (TuplePattern _ constrTerms) = concatMap constrTerm2codes constrTerms
constrTerm2codes (ListPattern _ constrTerms) = concatMap constrTerm2codes constrTerms
constrTerm2codes (AsPattern ident constrTerm) =
    Function OtherFunctionKind (qualify ident) : constrTerm2codes constrTerm
constrTerm2codes (LazyPattern _ constrTerm) = constrTerm2codes constrTerm
constrTerm2codes (FunctionPattern qualIdent constrTerms) = 
    Function OtherFunctionKind qualIdent : concatMap constrTerm2codes constrTerms
constrTerm2codes (InfixFuncPattern constrTerm1 qualIdent constrTerm2) =
    constrTerm2codes constrTerm1 ++ [Function InfixFunction qualIdent] ++ constrTerm2codes constrTerm2
   
expression2codes :: Expression -> [Code]
expression2codes (Literal literal) = []
expression2codes (Variable qualIdent) = 
    [Identifier IdOccur qualIdent]
expression2codes (Constructor qualIdent) = 
    [ConstructorName ConstrCall qualIdent]
expression2codes (Paren expression) = 
    expression2codes expression
expression2codes (Typed expression typeExpr) = 
    expression2codes expression ++ typeExpr2codes typeExpr
expression2codes (Tuple _ expressions) = 
    concatMap expression2codes expressions
expression2codes (List _ expressions) = 
    concatMap expression2codes expressions
expression2codes (ListCompr _ expression statements) = 
    expression2codes expression ++ concatMap statement2codes statements
expression2codes (EnumFrom expression) = 
    expression2codes expression
expression2codes (EnumFromThen expression1 expression2) = 
    expression2codes expression1 ++ expression2codes expression2
expression2codes (EnumFromTo expression1 expression2) = 
    expression2codes expression1 ++ expression2codes expression2
expression2codes (EnumFromThenTo expression1 expression2 expression3) = 
    expression2codes expression1 ++ 
    expression2codes expression2 ++ 
    expression2codes expression3
expression2codes (UnaryMinus ident expression) = 
    Symbol (name ident) : expression2codes expression 
expression2codes (Apply expression1 expression2) = 
    expression2codes expression1 ++ expression2codes expression2
expression2codes (InfixApply expression1 infixOp expression2) = 
    expression2codes expression1 ++ infixOp2codes infixOp ++ expression2codes expression2
expression2codes (LeftSection expression infixOp) = 
    expression2codes expression ++ infixOp2codes infixOp
expression2codes (RightSection infixOp expression) = 
    infixOp2codes infixOp ++ expression2codes expression
expression2codes (Lambda _ constrTerms expression) = 
    concatMap constrTerm2codes constrTerms ++ expression2codes expression
expression2codes (Let decls expression) = 
    concatMap decl2codes decls ++ expression2codes expression
expression2codes (Do statements expression) = 
    concatMap statement2codes statements ++ expression2codes expression
expression2codes (IfThenElse _ expression1 expression2 expression3) = 
    expression2codes expression1 ++ expression2codes expression2 ++ expression2codes expression3
expression2codes (Case _ expression alts) = 
    expression2codes expression ++ concatMap alt2codes alts
    
infixOp2codes :: InfixOp -> [Code]
infixOp2codes (InfixOp qualIdent) = [Function InfixFunction qualIdent]
infixOp2codes (InfixConstr qualIdent) = [ConstructorName OtherConstrKind qualIdent]


statement2codes :: Statement -> [Code] 
statement2codes (StmtExpr _ expression) =
    expression2codes expression
statement2codes (StmtDecl decls) =
    concatMap decl2codes decls
statement2codes (StmtBind _ constrTerm expression) =
     constrTerm2codes constrTerm ++ expression2codes expression


alt2codes :: Alt -> [Code]
alt2codes (Alt _ constrTerm rhs) =
    constrTerm2codes constrTerm ++ rhs2codes rhs
         
constrDecl2codes :: ConstrDecl -> [Code]
constrDecl2codes (ConstrDecl _ idents ident typeExprs) =
    ConstructorName ConstrDecla (qualify ident) : concatMap typeExpr2codes typeExprs
constrDecl2codes (ConOpDecl _ idents typeExpr1 ident typeExpr2) =   
    typeExpr2codes typeExpr1 ++ [ConstructorName ConstrDecla $ qualify ident] ++ typeExpr2codes typeExpr2

         
importSpec2codes :: ModuleIdent -> ImportSpec -> [Code]
importSpec2codes moduleIdent (Importing _ imports) = concatMap (import2codes moduleIdent) imports
importSpec2codes moduleIdent (Hiding _ imports) = concatMap (import2codes moduleIdent) imports

import2codes :: ModuleIdent -> Import -> [Code]
import2codes moduleIdent (Import ident) =
     [Function OtherFunctionKind $ qualifyWith moduleIdent ident]  
import2codes moduleIdent (ImportTypeWith ident idents) = 
     ConstructorName OtherConstrKind (qualifyWith moduleIdent ident) :
     map (Function OtherFunctionKind . qualifyWith moduleIdent) idents
import2codes moduleIdent (ImportTypeAll  ident) = 
     [ConstructorName OtherConstrKind $ qualifyWith moduleIdent ident]  
     
typeExpr2codes :: TypeExpr -> [Code]     
typeExpr2codes (ConstructorType qualIdent typeExprs) = 
    TypeConstructor TypeUse qualIdent : concatMap typeExpr2codes typeExprs
typeExpr2codes (VariableType ident) = 
    [Identifier IdOccur (qualify ident)]
typeExpr2codes (TupleType typeExprs) = 
    concatMap typeExpr2codes typeExprs
typeExpr2codes (ListType typeExpr) = 
    typeExpr2codes typeExpr
typeExpr2codes (ArrowType typeExpr1 typeExpr2) = 
    typeExpr2codes typeExpr1 ++ typeExpr2codes typeExpr2

-- TOKEN TO STRING ------------------------------------------------------------

token2string (Token Id a) = attributes2string a
token2string (Token QId a) = attributes2string a
token2string (Token Sym a) = attributes2string a
token2string (Token QSym a) = attributes2string a
token2string (Token IntTok a) = attributes2string a
token2string (Token FloatTok a) = attributes2string a
token2string (Token CharTok a) = attributes2string a
token2string (Token IntegerTok a) = attributes2string a
token2string (Token StringTok a) = attributes2string a
token2string (Token LeftParen _) = "("
token2string (Token RightParen _) = ")"
token2string (Token Semicolon _) = ";"
token2string (Token LeftBrace _) = "{"
token2string (Token RightBrace _) = "}"
token2string (Token LeftBracket _) = "["
token2string (Token RightBracket _) = "]"
token2string (Token Comma _) = ","
token2string (Token Underscore _) = "_"
token2string (Token Backquote _) = "`"
token2string (Token VSemicolon _) = ""
token2string (Token VRightBrace _) = ""
token2string (Token At _) = "@"
token2string (Token Colon _) = ":"
token2string (Token DotDot _) = ".."
token2string (Token DoubleColon _) = "::"
token2string (Token Equals _) = "="
token2string (Token Backslash _) = "\\"
token2string (Token Bar _) = "|"
token2string (Token LeftArrow _) = "<-"
token2string (Token RightArrow _) = "->"
token2string (Token Tilde _) = "~"
token2string (Token Sym_Dot _) = "."
token2string (Token Sym_Minus _) = "-"
token2string (Token Sym_MinusDot _) = "-."
token2string (Token KW_case _) = "case"
token2string (Token KW_choice _) = "choice"
token2string (Token KW_data _) = "data"
token2string (Token KW_do _) = "do"
token2string (Token KW_else _) = "else"
token2string (Token KW_eval _) = "eval"
token2string (Token KW_external _) = "external"
token2string (Token KW_free _) = "free"
token2string (Token KW_if _) = "if"
token2string (Token KW_import _) = "import"
token2string (Token KW_in _) = "in"
token2string (Token KW_infix _) = "infix"
token2string (Token KW_infixl _) = "infixl"
token2string (Token KW_infixr _) = "infixr"
token2string (Token KW_let _) = "let"
token2string (Token KW_module _) = "module"
token2string (Token KW_newtype _) = "newtype"
token2string (Token KW_of _) = "of"
token2string (Token KW_rigid _) = "rigid"
token2string (Token KW_then _) = "then"
token2string (Token KW_type _) = "type"
token2string (Token KW_where _) = "where"
token2string (Token Id_as _) = "as"
token2string (Token Id_ccall _) = "ccall"
token2string (Token Id_forall _) = "forall"
token2string (Token Id_hiding _) = "hiding"
token2string (Token Id_interface _) = "interface"
token2string (Token Id_primitive _) = "primitive"
token2string (Token Id_qualified _) = "qualified"
token2string (Token EOF _) = ""
token2string (Token LineComment (StringAttributes sval _)) = sval
token2string (Token NestedComment (StringAttributes sval _)) = sval

attributes2string NoAttributes = ""
attributes2string (CharAttributes cval _) = showCh cval 
attributes2string (IntAttributes ival _) = show ival
attributes2string (FloatAttributes fval _) = show fval
attributes2string (IntegerAttributes intval _) = show intval
attributes2string (StringAttributes sval _) = showSt sval 
attributes2string (IdentAttributes mIdent ident) =concat (intersperse "." (mIdent ++ [ident])) 


showCh c    
   | c == '\\' = "'\\\\'"
   | elem c ('\127' : ['\001' .. '\031']) = show c
   | otherwise = toString c
  where
    toString c = '\'' : c : "'"

showSt = addQuotes . concatMap toGoodChar 
   where
      addQuotes x = "\"" ++ x ++ "\""

toGoodChar c     
   | c == '\\' = "\\\\"
   | elem c ('\127' : ['\001' .. '\031']) = justShow c
   | c == '"' = "\\\""
   | otherwise = c : "" 
 where
     justShow = init . tail . show
