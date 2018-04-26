(* ::Package:: *)

(*

SternBrocot: Wolfram Mathematica package for the generation of Stern-Brocot rational sequences approximating real numbers.

For more information, see: 
[1] https://www.ams.org/publicoutreach/feature-column/fcarc-stern-brocot
[2] https://www.cut-the-knot.org/blue/Stern.shtml
[3] M. Ortolano, M. Abrate, L. Callegaro, "On the synthesis of quantum Hall array resistance standards", Metrologia, 52, 31-39,2015. This shows an application.

Copyright 2018 Massimo Ortolano <massimo.ortolano@polito.it>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the 
Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF 
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR 
ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH 
THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

BeginPackage["SternBrocot`"]

rationalApproximant::usage = "r = rationalApproximant[val,order]"

Begin["`Private`"]
mediant[a_,b_]:={a[[1]]+b[[1]],a[[2]]+b[[2]]}
leftAncestor[val_/;val>=0,0]:=leftAncestor[val,0]={0,1}
rightAncestor[val_/;val>=0,0]:=rightAncestor[val,0]={1,0}
leftAncestor[val_,n_Integer]:=leftAncestor[val,n]=If[val<rationalApproximant[val,n][[1]]/rationalApproximant[val,n][[2]],leftAncestor[val,n-1],rationalApproximant[val,n]]
rightAncestor[val_,n_Integer]:=rightAncestor[val,n]=If[val<rationalApproximant[val,n][[1]]/rationalApproximant[val,n][[2]],rationalApproximant[val,n],rightAncestor[val,n-1]]
rationalApproximant[val_/;val>=0,1]:=rationalApproximant[val,1]={1,1}
rationalApproximant[val_/;val>=0,n_Integer/;n>0]:=rationalApproximant[val,n]=If[val==rationalApproximant[val,n-1][[1]]/rationalApproximant[val,n-1][[2]],{rationalApproximant[val,n-1][[1]],rationalApproximant[val,n-1][[2]]},mediant[leftAncestor[val,n-1],rightAncestor[val,n-1]]]
SetAttributes[rationalApproximant,Listable];
End[]
EndPackage[]









