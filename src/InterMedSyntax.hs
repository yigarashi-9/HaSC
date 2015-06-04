module InterMedSyntax where

data Type = CInt
          | CPointer Type
          | CArray Type Int
          | CFun Type [Type]
