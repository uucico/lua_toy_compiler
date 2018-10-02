-module(oputil).

-export([opname/1, optype/1, opnum/1]).

opname(0) -> "move";
opname(1) -> "loadk";
opname(2) -> "loadbool";
opname(3) -> "loadnil";
opname(4) -> "getupval";
opname(5) -> "getglobal";
opname(6) -> "gettable";
opname(7) -> "setglobal";
opname(8) -> "setupval";
opname(9) -> "settable";
opname(10) -> "newtable";
opname(11) -> "self";
opname(12) -> "add";
opname(13) -> "sub";
opname(14) -> "mul";
opname(15) -> "dive";
opname(16) -> "mod";
opname(17) -> "pow";
opname(18) -> "unm";
opname(19) -> "not";
opname(20) -> "len";
opname(21) -> "concat";
opname(22) -> "jmp";
opname(23) -> "eq";
opname(24) -> "lt";
opname(25) -> "le";
opname(26) -> "test";
opname(27) -> "testset";
opname(28) -> "call";
opname(29) -> "tailcall";
opname(30) -> "return";
opname(31) -> "forloop";
opname(32) -> "forprep";
opname(33) -> "tforloop";
opname(34) -> "setlist";
opname(35) -> "close";
opname(36) -> "closure";
opname(37) -> "vararg".

optype(1) -> sABx;
optype(5) -> sABx; 
optype(7) -> sABx; 
optype(22) -> sABx; 
optype(31) -> sAsBx; 
optype(32) -> sAsBx; 
optype(36) -> sABx; 
optype(_) -> sABC.

opnum("move") -> 0;
opnum("loadk") -> 1;
opnum("loadbool") -> 2;
opnum("loadnil") -> 3;
opnum("getupval") -> 4;
opnum("getglobal") -> 5;
opnum("gettable") -> 6;
opnum("setglobal") -> 7;
opnum("setupval") -> 8;
opnum("settable") -> 9;
opnum("newtable") -> 10;
opnum("self") -> 11;
opnum("add") -> 12;
opnum("sub") -> 13;
opnum("mul") -> 14;
opnum("dive") -> 15;
opnum("mod") -> 16;
opnum("pow") -> 17;
opnum("unm") -> 18;
opnum("not") -> 19;
opnum("len") -> 20;
opnum("concat") -> 21;
opnum("jmp") -> 22;
opnum("eq") -> 23;
opnum("lt") -> 24;
opnum("le") -> 25;
opnum("test") -> 26;
opnum("testset") -> 27;
opnum("call") -> 28;
opnum("tailcall") -> 29;
opnum("return") -> 30;
opnum("forloop") -> 31;
opnum("forprep") -> 32;
opnum("tforloop") -> 33;
opnum("setlist") -> 34;
opnum("close") -> 35;
opnum("closure") -> 36;
opnum("vararg") -> 37.
