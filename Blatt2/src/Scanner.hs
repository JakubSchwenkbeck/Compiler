module Scanner where
-- A Scanner for a subset of Java
import Data.Char

data Token = ClassToken  -- Keyword for "Class"
     	   | ModifierToken -- Keyword for modifier like "public", "private", "protected"
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
