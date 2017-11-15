module Types where

data Token = TokenNewline      -- '\n'
           | TokenH Int        -- ein Header mit der Anzahl der Hashes
           | TokenText String  -- Text
           | TokenEmptyline Int-- '\n' oder Leerzeichen + '\n'
           | TokenBlanks Int   -- Blanks mit Anzahl
           | TokenStars Int    -- Sterne mit Anzahl
           | TokenCodeline String
           | TokenCodeBlock String
    deriving (Show, Eq)

-- Abstract Syntax Tree für HTML-Generierung. Daher schon nahe an HTML angelehnt.
data AST = Sequence [AST]   -- eine Sequenz von HTML-Elementen
         | H Int AST        -- eine Überschrift, der Int ist das Level (6 für H6)
                            -- und der AST repräsentiert den Inhalt
         | P [AST]          -- ein Absatz mit dem Inhalt
         | Text String      -- einfach nur Text
         | Emptyline        -- eine leere Zeile
         | Emphasis AST     -- kursiver Text
         | Strong AST       -- fetter Text
    deriving (Eq, Show)
