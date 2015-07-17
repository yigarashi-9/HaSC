module HaSC.Prim.ErrorMsg where

import HaSC.Prim.ObjInfo
import HaSC.Prim.AnalyzedAST
import Text.Parsec.Pos

errorHeader :: SourcePos -> String
errorHeader pos = "\n*** Compilation Error ***\n" ++ showSourcePos pos ++ "\n"

showSourcePos :: SourcePos -> String
showSourcePos pos = concat ["src  : ", sourceName pos , "\n",
                            "line : ", show $ sourceLine pos, "\n",
                            "col  : ", show $ sourceColumn pos, "\n"]

enclose :: String -> String -> String
enclose s body = s ++ body ++ s

makeError :: SourcePos -> [String] -> String
makeError pos msgs = errorHeader pos ++ concat msgs ++ "\n"

funcReferError :: SourcePos -> String -> String
funcReferError pos name
    = makeError pos ["Not function: ", "'" `enclose` name, "\n"]

invalidRetTypeError :: SourcePos -> String -> String
invalidRetTypeError pos name
    = makeError pos ["Invalid function type: ", "'" `enclose` name]

retTypeError :: SourcePos -> String -> CType -> CType -> String
retTypeError pos name expTy orgTy
    = makeError pos ["Invalid return type:\n",
                     "  Expected : ", show expTy, "\n",
                     "  Actual   : ", show orgTy]

condError :: SourcePos -> CType -> String
condError pos orgTy = makeError pos ["Invalid expression:\n",
                                     "  Expected : ", show CInt, "\n",
                                     "  Actual   : ", show orgTy]

binaryTypeError :: SourcePos -> String -> CType -> CType -> String
binaryTypeError pos op ty1 ty2
    = makeError pos ["Operands of ", "'" `enclose` op, " must be 'int':\n",
                     "  Left  : ", show ty1, "\n",
                     "  Right : ", show ty2]

typeDiffError :: SourcePos -> String -> CType -> CType -> String
typeDiffError pos op ty1 ty2
    = makeError pos ["Operands of ", "'" `enclose` op, " must be SAME:\n",
                     "  Left  : ", show ty1, "\n",
                     "  Right : ", show ty2]

invalidCalcError :: SourcePos -> String -> CType -> CType -> String
invalidCalcError pos op ty1 ty2
    = makeError pos ["Operands of ", "'" `enclose` op, " is INVALID:\n",
                     "  Left  : ", show ty1, "\n",
                     "  Right : ", show ty2]

argumentError :: SourcePos -> String -> String
argumentError pos name
    = makeError pos ["Invalid argument: ", "'" `enclose` name]

unaryError :: SourcePos -> String -> CType -> CType -> String
unaryError pos op expTy orgTy
    = makeError pos ["Invalid argument of ", "'" `enclose` op,
                     "  Expected : ", show expTy, "\n",
                     "  Actual   : ", show orgTy]

addrFormError :: SourcePos -> String
addrFormError pos = makeError pos ["'&' takes only variable"]

assignError :: SourcePos -> String
assignError pos = makeError pos ["Invalid assign"]

protoTypeError :: SourcePos -> String -> String
protoTypeError pos name = makeError pos ["Invalid prototype declralation: ",
                                         "'" `enclose` name]

funcDeclError :: SourcePos -> String -> String
funcDeclError pos name = makeError pos ["Invalid function declralation: ",
                                         "'" `enclose` name]

voidError :: SourcePos -> String
voidError pos = makeError pos ["Invalid void"]

duplicateError :: SourcePos -> String -> String
duplicateError pos name
    = makeError pos ["Duplicate decralation: ", "'" `enclose` name]

undefinedError :: SourcePos -> String -> String
undefinedError pos name
    = makeError pos ["Undefined variable: ", "'" `enclose` name]

warningMsg :: SourcePos -> String -> String
warningMsg pos name
    = concat  [showSourcePos pos,
              "Shadow variable: ", "'" `enclose` name, "\n\n"]
