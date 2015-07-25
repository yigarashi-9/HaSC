module Optimize where

import ObjInfo
import IntermedSyntax

type FlowGraph = [(Label, Node)]
data Node = Node { icode :: [ICode],
                   next  :: Label    } deriving(Eq, Show)

makeFlowGraph :: IProgram -> IProgram

analyzeBody :: [ICode] -> FlowGraph
