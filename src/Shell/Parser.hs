module Shell.Parser 
    ( module Shell.Parser.Types
    , blockP
    ) where

import Control.Monad ( foldM )
import Control.Monad.Combinators.Expr
import Data.Char ( isAlphaNum, isSpace )
import Data.Functor ( void, ($>) )
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import System.Posix.IO ( stdInput, stdOutput )
import System.Posix.Types ( Fd(..) )

import Shell.Parser.Scan
import Shell.Parser.Types

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

--

bare :: Parser Atom
bare = Bare <$> scan1P (Just "bare word") False impl
    where
        invalidCharacters :: String
        invalidCharacters = "|&;<>()$\"'"

        isValid x = not (x `elem` invalidCharacters || isSpace x)

        impl True  (Just _)    = Continue False
        impl True  Nothing     = Done
        impl False (Just '\\') = Continue True
        impl False (Just c)
                               | isValid c = Continue False
                               | otherwise = Done
        impl False Nothing     = Done

charLiteral :: Parser Char
charLiteral = try L.charLiteral <|> (char '\\' *> satisfy (`elem` extraEscapes))
    where
        extraEscapes :: String
        extraEscapes = "`$"

singlyQuoted :: Parser Atom
singlyQuoted = SinglyQuoted . T.pack <$> (sq *> manyTill charLiteral sq)
    where
        sq = char '\''

doublyQuoted :: Parser Atom
doublyQuoted = DoublyQuoted <$> (dq *> manyTill dqAtom dq)
    where
        dq = char '"'
        vs = char '$'

        dqAtom = dqBare <|> variable
        dqBare = Bare . T.pack <$> (some (notFollowedBy (dq <|> vs) *> charLiteral))

variable :: Parser Atom
variable = Variable <$> ("$" *> takeWhile1P (Just "variable name") p)
    where
        p x = isAlphaNum x || x == '_'

atom :: Parser Atom
atom = lexeme $ singlyQuoted <|> doublyQuoted <|> variable <|> bare

--

data CommandPart = Redirect Redirection | Background | Atom Atom
                 deriving (Show, Eq)

redirect :: Parser CommandPart
redirect = lexeme $ do
    fdIn <- optional L.decimal
    mode <- "<" $> Read
        <|> try ">>" $> Append
        <|> ">" $> Overwrite
    spaceConsumer
    target <- ("&" *> (ToFd . Fd <$> L.decimal) <|> CloseFd <$ "-") <|> (ToFile <$> atom)
    return $ Redirect (Redirection (maybe stdOutput Fd fdIn) mode target)

background :: Parser CommandPart
background = lexeme $ Background <$ symbol "&"

cmdAtom :: Parser CommandPart
cmdAtom = Atom <$> atom

cmdParts :: Parser [CommandPart]
cmdParts = many (redirect <|> background <|> cmdAtom)

command :: Parser Syntax
command = do
    initialAtom <- atom
    let initial = Command initialAtom [] [] False
    parts <- cmdParts
    return $ foldr go initial parts
    where
        go (Redirect r) cmd@Command{ cmdRedirections } = cmd { cmdRedirections = r : cmdRedirections}
        go Background   cmd                            = cmd { cmdIsBackground = True }
        go (Atom a)     cmd@Command{ cmdArgs }         = cmd { cmdArgs = a : cmdArgs }

term :: Parser Syntax
term = command <|> between (symbol "(") (symbol ")") expr

expr :: Parser Syntax
expr = makeExprParser term opTable

pipe, andOp, orOp :: Parser (Syntax -> Syntax -> Syntax)
pipe  = symbol "|"  $> Pipe
andOp = symbol "&&" $> AndOp
orOp  = symbol "||" $> OrOp

opTable :: [[Operator Parser Syntax]]
opTable = 
    [ [ InfixL pipe ]
    , [ InfixL andOp, InfixL orOp ]
    ]

blockP :: Parser [Syntax]
blockP = sepEndBy expr (void (symbol ";") <|> eof)
