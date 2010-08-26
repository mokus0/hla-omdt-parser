{-# LANGUAGE TypeOperators, NoMonomorphismRestriction #-}
module Text.OMDT.Parser.Prim where

import Text.OMDT.Lexer (Token(..), tokenAlexPosn, AlexPosn(..))
import Text.OMDT.Syntax (FootNoted(..), SExpr(..), Atom(..))

import Control.Monad (liftM)
import Data.Char (toLower)
import qualified Data.Map as M
import Data.Record.Label
import Data.Time (Day, fromGregorian)
import Text.Parsec hiding (string)

-- * General purpose utility functions used by the parser

-- |Run a 'ParsecT' parser with a local state, potentially of a different 
-- type than the state of the surrounding context; sets the state to the 
-- provided value, executes the action, restores the old state and returns
-- the updated local state.
localState :: Monad m => u -> ParsecT s u m a -> ParsecT s u' m (u,a)
localState u action = mkPT $ \origState -> do
    let u' = stateUser origState
    result <- runParsecT action (origState {stateUser = u})
    let alterReply (Ok a newState err) = Ok (stateUser newState, a) (newState {stateUser = u'}) err
        alterReply (Error err)   = Error err
    return (fmap (liftM alterReply) result)

-- |Apply a 'lens' to focus the user state.  Essentially, hoists a part of the 
-- state (or some transformation of the state, changes to which can be pulled
-- back to the original state) to be the new top-level state.
focus :: Monad m => (u :-> u') -> ParsecT s u' m a -> ParsecT s u m a
focus lens action = do
    orig <- getState
    (new, result) <- localState (getL lens orig) action
    putState (setL lens new orig)
    return result


element tag lens parseIt = (setP lens . Just =<< tagged tag parseIt) <?> (tag ++ " element")

-- | Set the value of a 'lens' in the parsec user state
setP :: Monad m => (u :-> t) -> t -> ParsecT s u m ()
setP lens val = modifyState (setL lens val)

-- | Modify the value of a 'lens' in the parsec user state
modifyP :: Monad m => (u :-> t) -> (t -> t) -> ParsecT s u m ()
modifyP lens f = modifyState (modL lens f)

-- |Update a Parsec 'SourcePos' with the 'AlexPosn' in a 'Token'
updatePosToken srcPos token = case tokenAlexPosn token of
    Nothing                     -> srcPos
    Just (AlexPn byte row col)  -> flip setSourceLine row . flip setSourceColumn col $ srcPos

-- |Parse a single token if the supplied function yields 'Just' something.
tokenMaybe f = tokenPrim showTok (\pos tok _ -> updatePosToken pos tok) f
    where
        showTok (OpenParen     posn)     = "'('"
        showTok (CloseParen    posn)     = "')'"
        showTok (OpenBracket   posn)     = "'['"
        showTok (CloseBracket  posn)     = "']'"
        showTok (Slash         posn)     = "'/'"
        showTok (Version       posn v)   = show v
        showTok (String        posn s)   = s
        showTok (Int           posn s _) = s
        showTok (Frac          posn s _) = s

-- * Unparsed S-Expressions

sexpr = list <|> atom

list = fmap List $ parens (many sexpr)
atom = fmap Atom $ choice
    [ fmap S_ anyString, fmap V_ version, fmap D_ date, fmap I_ int, fmap F_ frac, fmap N_ footnote]

unparsed lens = do
    openParen
    tag <- anyString
    things <- many sexpr
    closeParen
    
    modifyP lens (M.insertWith (++) tag [things])

-- * Primitive tokens from the lexer

openParen = tokenMaybe f <?> "("
    where f OpenParen{} = Just '('; f _ = Nothing
closeParen = tokenMaybe f <?> ")"
    where f CloseParen{} = Just ')'; f _ = Nothing
openBracket = tokenMaybe f <?> "["
    where f OpenBracket{} = Just '['; f _ = Nothing
closeBracket = tokenMaybe f <?> "]"
    where f CloseBracket{} = Just ']'; f _ = Nothing
slash = tokenMaybe f <?> "/"
    where f Slash{} = Just '/'; f _ = Nothing

stringWhere p = tokenMaybe f
    where f (String _ s) | p s = Just s ; f _ = Nothing
string s = stringWhere (s==) <?> show s
anyString = stringWhere (const True) <?> "string"

version = tokenMaybe f
    where f (Version _ v) = Just v; f _ = Nothing

int = tokenMaybe f
    where f (Int _ _ i) = Just (fromInteger i); f _ = Nothing
frac = int <|> tokenMaybe f
    where f (Frac _ _ f) = Just (fromRational f); f _ = Nothing

date = do
    m <- try $ do
        m <- int
        slash
        return m
    d <- int
    slash
    y <- int
    return (fromGregorian y (fromInteger m) (fromInteger d))

boolean = choice
    [ stringWhere ((=="yes"  ) . map toLower) >> return True
    , stringWhere ((=="true" ) . map toLower) >> return True
    , stringWhere ((=="no"   ) . map toLower) >> return False
    , stringWhere ((=="false") . map toLower) >> return False
    ]

parens thing = do
    try openParen
    x <- thing
    closeParen
    return x

brackets thing = do
    try openBracket
    x <- thing
    closeBracket
    return x

footnote = brackets int

tagged tag content = do
    try $ do
        openParen
        string tag <?> (tag ++ " tag")
    result <- content
    closeParen
    
    return result

footNoted content = do
    thing <- content
    mbNote <- optionMaybe footnote
    return (FootNoted mbNote thing)