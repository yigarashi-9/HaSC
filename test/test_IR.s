IFunDecl ("initialize",0) [("array",1)] (ICompound [IVarDecl 0($fp),IVarDecl -4($fp),IVarDecl -8($fp),IVarDecl -12($fp),IVarDecl -16($fp)] [ILet 0($fp) (IInt 3),ILet -8($fp) (IInt 4),ILet -12($fp) (IAop "*" -8($fp) 0($fp)),ILet -4($fp) (IAop "+" 4($fp) -12($fp)),ILet -16($fp) (IInt 3),IWrite -4($fp) -16($fp)])
IFunDecl ("main",0) [] (ICompound [IVarDecl 0($fp),IVarDecl -4($fp),IVarDecl -8($fp),IVarDecl -12($fp),IVarDecl -16($fp),IVarDecl -20($fp),IVarDecl -24($fp),IVarDecl -28($fp),IVarDecl -32($fp),IVarDecl -64($fp)] [ILet 0($fp) (IAddr -64($fp)),ICall -4($fp) ("initialize",0) [0($fp)],ILet -12($fp) (IAddr -64($fp)),ILet -16($fp) (IInt 3),ILet -24($fp) (IInt 4),ILet -28($fp) (IAop "*" -24($fp) -16($fp)),ILet -20($fp) (IAop "+" -12($fp) -28($fp)),IRead -8($fp) -20($fp),IPrint -8($fp),ILet -32($fp) (IInt 0),IReturn -32($fp)])
