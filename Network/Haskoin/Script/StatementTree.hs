{-# LANGUAGE FlexibleContexts #-}
{-|
  This module contains parsing functionality for Bitcoin scripts.  The
  main function parses the script and builds a statement tree from the
  if statements.
-}
module Network.Haskoin.Script.StatementTree
(
  -- * Parsing Function
  parseScript
  -- * Statement Tree node type
, Statement(..)
)
where

import Network.Haskoin.Script ( Script(..), ScriptOp(..) )

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Data.Maybe ( fromMaybe )
import Control.Monad ( void )

-- | A simple data type defining the statement tree.
data Statement t 
  -- | A simple statement ( not an if statement ) containing a single ScriptOp
  = SingletonStatement t
  -- | An if statement with two branches, both of which can be empty,
  -- defining action to perform under true and false conditions
  -- respectively.
  | IfStatement [ Statement t ] [ Statement t ]
  deriving ( Show )

-- | Parser based on Text.Parec.Char.satisfy
opSatisfy :: ( Stream s m ScriptOp ) => ( ScriptOp -> Bool ) -> ParsecT s u m ScriptOp
opSatisfy f = tokenPrim show
                        ( \pos _ _ -> incSourceLine pos 1 )
                        ( \c -> if f c then Just c else Nothing )

-- | Defines when a script op is a Singleton
isSimple :: ScriptOp -> Bool
isSimple c = case c of
  OP_IF -> False
  OP_NOTIF -> False
  OP_ELSE -> False
  OP_ENDIF -> False
  _ -> True

-- | Convenience OP_IF parser
opIf :: ( Stream s m ScriptOp ) => ParsecT s u m ()
opIf = void $ opSatisfy ( == OP_IF )

-- | Convenience OP_NOTIF parser
opNotIf :: ( Stream s m ScriptOp ) => ParsecT s u m ()
opNotIf = void $ opSatisfy ( == OP_NOTIF )

-- | Convenience OP_ElSE parser
opElse :: ( Stream s m ScriptOp ) => ParsecT s u m ()
opElse = void $ opSatisfy ( == OP_ELSE )

-- | Convenience OP_ENDIF parser
opEndIf :: ( Stream s m ScriptOp ) => ParsecT s u m ()
opEndIf = void $ opSatisfy ( == OP_ENDIF )

elseStatement :: GenParser ScriptOp st [ Statement ScriptOp ]
elseStatement = do
  opElse
  statements

ifStatement :: GenParser ScriptOp st ( Statement ScriptOp )
ifStatement = do 
  opIf
  trueStatements <- statements
  maybeFalseStatements <- optionMaybe elseStatement
  opEndIf
  return $ IfStatement trueStatements ( fromMaybe [] maybeFalseStatements )

notIfStatement :: GenParser ScriptOp st ( Statement ScriptOp )
notIfStatement = do 
  opNotIf
  falseStatements <- statements
  maybeTrueStatements <- optionMaybe elseStatement
  opEndIf
  return $ IfStatement ( fromMaybe [] maybeTrueStatements ) falseStatements

simpleStatement :: GenParser ScriptOp st ( Statement ScriptOp )
simpleStatement = 
   do c <- opSatisfy isSimple
      return ( SingletonStatement c )

statements :: GenParser ScriptOp st [ Statement ScriptOp ]
statements = many ( simpleStatement <|> ifStatement <|> notIfStatement )

-- | Parses a script into a statement tree.  With no if statements, a
-- list results.
parseScript :: Script
            -> [ Statement ScriptOp ]
parseScript s = case parse statements "script parser" (scriptOps s) of
                  Left _ -> []
                  Right x -> x