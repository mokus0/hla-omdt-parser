{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, TypeOperators #-}
module Text.OMDT.Parser where

import Prelude hiding (id, (.))
import Control.Category

import Text.OMDT.Lexer (Token(..), tokenAlexPosn, AlexPosn(..))
import Text.OMDT.Syntax as Syn

import Text.Parsec hiding (string)
import Text.Parsec.Prim (ParsecT(..))

import Control.Monad (liftM)
import Data.Time (Day)
import Data.Record.Label
import qualified Data.IntMap as I
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Version (Version)

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

setP :: Monad m => (u :-> t) -> t -> ParsecT s u m ()
setP lens val = modifyState (setL lens val)

updatePosToken srcPos token = case tokenAlexPosn token of
    Nothing                     -> srcPos
    Just (AlexPn byte row col)  -> flip setSourceLine row . flip setSourceColumn col $ srcPos

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

openParen = tokenMaybe f
    where f OpenParen{} = Just '('; p _ = Nothing
closeParen = tokenMaybe f
    where f CloseParen{} = Just ')'; p _ = Nothing
openBracket = tokenMaybe f
    where f OpenBracket{} = Just '['; p _ = Nothing
closeBracket = tokenMaybe f
    where f CloseBracket{} = Just ']'; p _ = Nothing
slash = tokenMaybe f
    where f Slash{} = Just '/'; p _ = Nothing

stringWhere p = tokenMaybe f
    where f (String _ s) | p s = Just s | otherwise = Nothing
string s = stringWhere (s==)
anyString = stringWhere (const True)

version = tokenMaybe f
    where f (Version _ v) = Just v; f _ = Nothing

int = tokenMaybe f
    where f (Int _ _ i) = Just (fromInteger i); f _ = Nothing
frac = int <|> tokenMaybe f
    where f (Frac _ _ f) = Just (fromRational f); f _ = Nothing

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

tagged tag content = parens $ do
    string tag
    content

header = do
    (hdr, _) <- localState emptyHeader (many headerElement)
    return hdr

headerElement = choice [pocElement]

pocElement = focus poc $ choice [pocHonorificName, pocFirstName, pocLastName, pocOrgName, pocPhone, pocEmail]

pocHonorificName = setP honorificName . Just =<< tagged "POCHonorificName" anyString
pocFirstName     = setP firstName     . Just =<< tagged "POCFirstName"     anyString
pocLastName      = setP lastName      . Just =<< tagged "POCLastName"      anyString
pocOrgName       = setP orgName       . Just =<< tagged "POCOrgName"       anyString
pocPhone         = setP phone         . Just =<< tagged "POCPhone"         anyString
pocEmail         = setP email         . Just =<< tagged "POCEmail"         anyString

