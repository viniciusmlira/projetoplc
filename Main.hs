import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (StringLit string) = return $ String string -- our code avaliar string
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do -- x=0;
    varScope <- stateLookup env var -- stores wether the variable will be global ou not
    e <- evalExpr env expr
    case varScope of
        GlobalVar -> createGlobalVar var e
        _ -> setVar var e -- sets it in the first scope it finds the variable

evalExpr env (DotRef expr id) = do -- bar.foo
    var <- evalExpr env expr
    case id of -- checks which is the called property
        (Id "head") -> do
            case var of -- checks the list format
                (List []) -> return (List [])
                (List (l:ls)) -> return l
                _ -> return $ Error "Não é um tipo válido"
        (Id "tail") -> do
            case var of
                (List []) -> return (List [])
                (List (l:ls)) -> return $ List ls
                _ -> return $ Error "Não é um tipo válido"
        (Id "len") -> do
            case var of
                (List _) -> do
                    return $ Int $ count var
                    where 
                        count (List []) = 0 
                        count (List (l:ls)) = 1 + (count (List ls))
                _ -> return $ Error "Não é um tipo válido"
        _ -> return $ Error "Não é uma função válida"

-- Listas
evalExpr env (ArrayLit []) = return $ List []
evalExpr env (ArrayLit [expr]) = do
    val <- evalExpr env expr
    return $ List [val]
evalExpr env (ArrayLit (l:ls)) = do
    cabeca <- evalExpr env l
    (List cauda) <- evalExpr env (ArrayLit ls)
    return $ List (cabeca:cauda)

-- Obter elementos de dentro de listas
evalExpr env (BracketRef expr indexExpr) = do
    list <- evalExpr env expr
    case list of
        (List _) -> do
            index <- evalExpr env indexExpr
            getElementFromList env list index

-- Function calls
evalExpr env (CallExpr name argsExpr) = do
    case name of
        (DotRef list (Id "concat")) -> do
            (List list1) <- evalExpr env list
            res <- evalExpr env (head argsExpr)
            case res of
                (List list2) -> return $ List $ list1 ++ list2
                val -> return $ List $ list1 ++ [val]
        _ -> do -- General function case  
            func <- evalExpr env name
            case func of
                (Function idd args stmts) -> do
                    addState env
                    result <- mapM (evalExpr env) argsExpr
                    let vars = (zip (map (\(Id a) -> a) args) result)
                    createLocalVars vars
                    res <- evalStmt env (BlockStmt stmts)
                    removeState env
                    case res of
                        Return val -> return val
                        Nil -> return Nil
                        _ -> return res
                res -> error $ "Deu isso aqui: " ++ (show res)

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr

-- blocos de statements
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt [stmt]) = evalStmt env stmt
evalStmt env (BlockStmt (stmt:stmts)) = do
    cabeca <- evalStmt env stmt
    case cabeca of
        Break -> return Break
        Return val -> return $ Return val
        _ -> evalStmt env (BlockStmt stmts)

-- if com um único stmt --
evalStmt env (IfSingleStmt expr stmt) = do
    v <- evalExpr env expr
    case v of
        (Bool b) -> if (b) then do
            addState env
            res <- evalStmt env stmt 
            removeState env
            return res
        else return Nil
        _ -> error $ "Not a valid expression"

-- if com 2 stmts --   if x then stmt1 else stmt2
evalStmt env (IfStmt expr stmt1 stmt2) = do
    v <- evalExpr env expr
    case v of
        (Bool b) -> if (b) then do
            addState env
            res <- evalStmt env stmt1
            removeState env
            return res
        else evalStmt env stmt2
        _ -> error $ "Not a valid expression"

-- While 
evalStmt env (WhileStmt expr stmt) = do
    Bool b <- evalExpr env expr
    if b then do 
        addState env
        res <- evalStmt env stmt
        removeState env
        case res of
            Break -> return Nil
            Continue -> evalStmt env (WhileStmt expr stmt)
            _ -> evalStmt env (WhileStmt expr stmt)
        
    else return Nil

--DoWhile
evalStmt env (DoWhileStmt stmt expr) = do
    addState env
    res <- evalStmt env stmt
    removeState env
    Bool b <- evalExpr env expr
    case res of 
        Break -> return Nil
        Continue -> if b then evalStmt env (DoWhileStmt stmt expr) else return Nil
        _ -> if b then evalStmt env (DoWhileStmt stmt expr) else return Nil        

-- BreakStmt
evalStmt env (BreakStmt Nothing) = return Break;

-- ContinueStmt
evalStmt env (ContinueStmt Nothing) = return Continue;

-- ReturnStmt
evalStmt env (ReturnStmt maybeExpr) = do
    case maybeExpr of
        (Just expr) -> do
            val <- evalExpr env expr
            return $ Return val
        Nothing -> return Nil

-- ForStmt for normal (MODIFICADO?)
evalStmt env (ForStmt initial test inc stmt) = do
    v <- evalForInit env initial
    testRes <- evalForTest env test -- Asks if the loop should continue
    case testRes of
        (Bool True) -> do
            addState env
            d <- evalStmt env stmt
            removeState env
            evalForInc env inc
            case d of
                Break -> return Nil
                Continue -> evalStmt env (ForStmt NoInit test inc stmt)
                _ -> evalStmt env (ForStmt NoInit test inc stmt)
        (Bool False) -> return Nil
        -- TODO Error
        _ ->  error "Not a valid expression"

-- ForInStmt
evalStmt env (ForInStmt initial expr stmt) = do
    list <- evalExpr env expr
    case initial of
        (ForInVar (Id id)) -> forLoop env id list stmt
        (ForInLVal (LVar id)) -> forLoop env id list stmt

evalStmt env (FunctionStmt id@(Id funcId) args stmts) = createGlobalVar funcId (Function id args stmts) --Talvez nao precise ser global

 
-- Searches for an element from a list using an index
getElementFromList :: StateT -> Value -> Value -> StateTransformer Value
getElementFromList _ (List []) _ = error $ "Index out of bounds exception" -- ver msg melhor?
getElementFromList env (List (l:ls)) (Int index) = if index == 0 then return l else getElementFromList env (List ls) (Int (index-1))


-- Evaluates For increment expression
evalForInc :: StateT -> (Maybe Expression) -> StateTransformer Value
evalForInc _ Nothing = return Nil
evalForInc env (Just inc) = evalExpr env inc

-- Evaluates For test expression
evalForTest :: StateT -> (Maybe Expression) -> StateTransformer Value
evalForTest _ Nothing = return $ Bool True
evalForTest env (Just test) = evalExpr env test

-- Evaluates For initialization
evalForInit :: StateT -> ForInit -> StateTransformer Value
evalForInit env NoInit = return Nil
evalForInit env (VarInit list) = evalStmt env (VarDeclStmt list)
evalForInit env (ExprInit expr) = evalExpr env expr

-- Loop forIn
forLoop :: StateT -> String -> Value -> Statement -> StateTransformer Value
forLoop env idd (List []) stmt = return Break
forLoop env idd (List (l:ls)) stmt = do
    addState env
    setVar idd l
    res <- evalStmt env stmt
    removeState env
    case res of
       Break -> return Break
       Continue -> forLoop env idd (List ls) stmt
       _ -> forLoop env idd (List ls) stmt

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Int  v1) (Int  v2) = return $ Bool $ v1 /= v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

-- Comparação de listas
-- Igualdade
infixOp env OpEq   (List  []) (List  []) = return $ Bool True
infixOp env OpEq   (List  []) (List  _) = return $ Bool False
infixOp env OpEq   (List  _) (List  []) = return $ Bool False
infixOp env OpEq   (List  (l:ls)) (List  (x:xs)) = do
    result <- infixOp env OpEq l x
    resto <- infixOp env OpEq (List ls) (List xs)
    infixOp env OpLAnd result resto


-- Diferença
infixOp env OpNEq   (List  []) (List  []) = return $ Bool False
infixOp env OpNEq   (List  []) (List  _) = return $ Bool True
infixOp env OpNEq   (List  _) (List  []) = return $ Bool True
infixOp env OpNEq   (List  (l:ls)) (List  (x:xs)) = do
    result <- infixOp env OpNEq l x
    resto <- infixOp env OpNEq (List ls) (List xs)
    infixOp env OpLOr result resto

infixOp _ _ val1 val2 = error $ "infixOp error with " ++ (show val1) ++ " and " ++ (show val2)

--
-- Environment and auxiliary functions
--

environment :: StateT
environment = [Map.empty]


-- look for state
stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    let pops [] _ = Nothing
        pops (s:states) var = 
            case Map.lookup var s of
                Nothing -> pops states var
                Just v -> Just v
    in case pops s var of
        Nothing -> (GlobalVar, s) -- nao achou em nenhuma memoria
        Just v -> (v, s) -- achou em alguma memoria


-- Declara uma variavel no escopo local, podendo ou não guardar um valor
varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> createLocalVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            createLocalVar id val


-- Procura, a partir de um estado local até o global, uma variável chamada var para
-- setar seu valor como val.

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, setVarIntoState s)
    where
        setVarIntoState [] = error $ "Global is dead"
        setVarIntoState (state:states) = case (Map.lookup var state) of
            Nothing -> state:(setVarIntoState states)
            Just v -> (insert var val state):states

setVars :: [(String, Value)] -> StateTransformer Value
setVars [] = return Nil
setVars ((id, val):vars) = do
    setVar id val
    setVars vars

-- Cria uma variavel APENAS no escopo local
createLocalVar :: String -> Value -> StateTransformer Value
createLocalVar var val = ST $ \(s:states) -> (val, (insert var val s):states)

createLocalVars :: [(String, Value)] -> StateTransformer Value
createLocalVars [] = return Nil -- var x;
createLocalVars ((id, val):vars) = do -- var y = 0;
    createLocalVar id val
    createLocalVars vars


-- Percorre os estados de memória até o global e cria uma variável APENAS no global
createGlobalVar :: String -> Value -> StateTransformer Value
createGlobalVar var val = ST $ \s -> (val, global var val s)
    where
        global var val (state:[]) = (insert var val state):[]
        global var val (state:states) = state:(global var val states)

-- Adiciona uma camada (escopo) de memória mais local
addState :: StateT -> StateTransformer Value
addState env = ST $ \s -> (Nil, Map.empty:s)


-- Remove a camada (escopo) de memória mais local
removeState :: StateT -> StateTransformer Value
removeState env = ST $ \(s:states) -> (Nil, states)

--
-- Types and boilerplate
--

type StateT = [Map String Value]
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

-- Imprime apenas a memória global
showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union (last defs) (last environment)) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f [Map.empty]

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements