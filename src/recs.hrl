-record(conf, {	version=16#51,
				format,
				endianess,
				sint,
				ssize_t,
				sinstr,
				slua_number,
				integral } ).

-record(str, {	size,
				text } ).

-record(op, {	opcode,
				name,
				type,
				a,
				b,
				c,
				bx,
				sbx } ).


-record(sym, {	n,
				t = number,
				r = 0,
				s = local 
				} ).

-record(gcp, {	k = [],
				sk = 0,
				c = [],
				sc = 0,
				l = [],
				sl = 0,
				r = 0,
				dr = 0,
				ctx = any,
				srcname
				} ).
