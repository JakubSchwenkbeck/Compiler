
module Scanner where
-- A Scanner for a subset of Java
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace) -- util for the lexer

-- Tokens are smaller bites of a written Code
data Token = ClassToken  -- Keyword for "Class"
     	   | ModifierToken String -- Keyword for modifier like "public", "private", "protected"
 	   | StaticToken -- Token for Keyword for "static"
	   | TypeToken String-- Token for the Type like "int", "boolean", "String"
	   | ReturnToken -- Return keyword for functions 
	   | IntToken Int -- Token for an Integer
	   | BoolToken Bool -- Token for a boolean literal
      	   | IfToken -- Token for the Keyword "if"
	   | ElseToken -- Token for the Else Keyword
	   | SymToken Char -- Token for a Symbol "+","-","/","*","%" ...
	   | IdentifierToken String -- token for an identifier like public *Main*
	   | WhileToken -- Token for While loops
	   | BraceToken Char -- Tokens for Braces like "(" ")" "{" "}"
	   | ForToken -- For loop token
	   | OtherToken String --Error / unexpected Token
	deriving (Eq,Show)

-- A lexer takes a string (code) and creates a list of tokens from it
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = alphaLexer (c:cs)
      | isDigit c = digitLexer (c:cs)
lexer ('=':cs) = SymToken '=' : lexer cs
lexer ('+':cs) = SymToken '+' : lexer cs
lexer ('-':cs) = SymToken '-' : lexer cs
lexer ('*':cs) = SymToken '*' : lexer cs
lexer ('/':cs) = SymToken '/' : lexer cs
lexer ('%':cs) = SymToken '%' : lexer cs
lexer ('(':cs) = BraceToken '(' : lexer cs
lexer (')':cs) = BraceToken ')' : lexer cs
lexer ('{':cs) = BraceToken '{' : lexer cs
lexer ('}':cs) = BraceToken '}' : lexer cs
lexer (c:cs)   = OtherToken [c] : lexer cs  -- Catch-all for unexpected ones



-- A Helper function to lex digits (int)
digitLexer :: String -> [Token]
digitLexer cs = IntToken (read num) : lexer rest
  	where (num,rest) = span isDigit cs



-- a big helper function to lex alphabetic input
alphaLexer :: String -> [Token]
alphaLexer cs =
  case span isAlpha cs of
	("class",rest) -> ClassToken : lexer rest
	("public",rest) -> ModifierToken "public" : lexer rest
	("private",rest) -> ModifierToken "private" : lexer rest
	("protected",rest) -> ModifierToken "protected" : lexer rest
	("return",rest) -> ReturnToken : lexer rest
	("static",rest) -> StaticToken : lexer rest
 	("int",rest) -> TypeToken "Integer" : lexer rest
 	("boolean",rest) -> TypeToken "Boolean" : lexer rest
	("String",rest) -> TypeToken "String" : lexer rest
	("true", rest) -> BoolToken True : lexer rest
	("false", rest) -> BoolToken False : lexer rest
	("if", rest) -> IfToken : lexer rest
	("else", rest) -> ElseToken : lexer rest
   	("while",rest) -> WhileToken : lexer rest
	("for" , rest) -> ForToken : lexer rest
        (var,rest) -> IdentifierToken var : lexer rest

