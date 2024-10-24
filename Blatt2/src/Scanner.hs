#module Scanner where
-- A Scanner for a subset of Java
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace) -- util for the lexer

-- Tokens are smaller bites of a written Code
data Token = ClassToken  -- Keyword for "Class"
     	   | ModifierToken String -- Keyword for modifier like "public", "private", "protected"
 	   | StaticToken -- Token for Keyword for "static"
	   | TypeToken -- Token for the Type like "int", "boolean", "String"
	   | IntToken Int -- Token for an Integer
	   | BoolToken Bool -- Token for a boolean literal
      	   | IfToken -- Token for the Keyword "if"
	   | ElseToken -- Token for the Else Keyword
	   | SymbolToken Char -- Token for a Symbol "+","-","/","*","%" ...
	   | IdentifierToken String -- token for an identifier like public *Main*
	   | WhileToken -- Token for While loops
	   | BraceToken -- Tokens for Braces like "(" ")" "{" "}"
	   | ForToken -- For loop token
	deriving (Eq,Show)

-- A lexer takes a string (code) and creates a list of tokens from it
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = AlphaLexer (c:cs)
      | isDigit c = DigitLexer (c:cs)
lexer ('=':cs) = SymToken '=' : lexer cs
lexer ('+':cs) = SymToken '+' : lexer cs
lexer ('-':cs) = SymToken '-' : lexer cs
lexer ('*':cs) = SymToken '*' : lexer cs
lexer ('/':cs) = SymToken '/' : lexer cs
lexer ('(':cs) = BraceToken '(' : lexer cs
lexer (')':cs) = BraceToken ')' : lexer cs
lexer ('{':cs) = BraceToken '{' : lexer cs
lexer ('}':cs) = BraceToken '}' : lexer cs

-- A Helper function to lex digits (int)
DigitLexer cs = IntToken (read num) : lexer rest
  	where (num,rest) = span isDigit cs

AlphaLexer cs =
  case span isAlpha cs of
	("class",rest) -> ClassToken : lexer rest
	("public",rest) -> ModifierToken "public" : lexer rest
	("private",rest) -> ModifierToken "private" : lexer rest
	("protected",rest) -> ModifierToken "protected" : lexer rest
	("return",rest) -> ReturnToken : lexer rest
	
(var,rest) -> IdentifierToken var : lexer rest
