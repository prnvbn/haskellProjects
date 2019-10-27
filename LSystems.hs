module LSystems where

import IC.Graphics
import Data.Fixed

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)


----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (angle',_,_)
  = angle'

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (_,axiom',_)
  = axiom'

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (_,_,rules')
  = rules'

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a  binding in the Rules list
lookupChar searchChar rules
  = head [string | (ruleChar,string) <-rules,searchChar == ruleChar]

-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne command rules
  = concat (map (flip lookupChar rules) command)

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand commands noOfExpansions rules
  = head (drop noOfExpansions (iterate (flip expandOne rules) commands))

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
-- Pre: the command is 'F','L' or 'R'
move 'F' _ ((x,y),orientation)
  = ((x+cos (degToRad orientation),y+sin (degToRad orientation)),orientation)
move 'R' angle (vertex,orientation)
  = (vertex,mod' (orientation - angle) 360.0)
move 'L' angle (vertex,orientation)
  = (vertex,mod' (orientation + angle) 360.0)
  -- mod 360 is present to ensure that the orientation doesn't go above 360
degToRad :: Angle -> Angle
degToRad
  = (*(pi/180))
--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
-- the original turtle state is at origin facing the +ve y-axis
trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 cs angle colour
  = trace1' cs ((0.0,0.0),90)

trace1' :: Commands -> TurtleState -> [ColouredLine]
trace1' [] _
  = []
trace1' (']':cs) tState
  = [] -- ++ trace1' cs angle colour tState
trace1' ('[':cs) tState
  = trace1' cs tState
trace1' (c:cs) tState@(oldVertex,_)
  | c == 'F'  = (oldVertex,newVertex,colour) : trace1' cs newTState
  | otherwise = trace1' cs newTState
  where
    newTState@(newVertex,_) = move c angle tState

-- Uses an explicit stack to deal with bracketed commands
trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 commands angle colour
  = trace2' commands angle colour [] ((0.0,0.0),90.0)
  where
    trace2' :: Commands -> Stack -> TurtleState -> [ColouredLine]
    trace2' "" _ _
      = []
    trace2' commands@(c:cs)  stack tState@(oldVertex,_)
      |c == '['  = trace2' cs (tState:stack) tState
      |c == ']'  = trace2' cs rest top
      |c == 'F'  = (oldVertex,newVertex,colour) : trace2' cs stack newTState
      |otherwise = trace2' cs stack newTState
      where
        (top:rest) = stack
        newTState@(newVertex,_) = move c angle tState





----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
