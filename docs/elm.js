(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aX.ax === region.aJ.ax)
	{
		return 'on line ' + region.aX.ax;
	}
	return 'on lines ' + region.aX.ax + ' through ' + region.aJ.ax;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cr,
		impl.cU,
		impl.cQ,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		aj: func(record.aj),
		ba: record.ba,
		a6: record.a6
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.aj;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.ba;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.a6) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cr,
		impl.cU,
		impl.cQ,
		function(sendToApp, initialModel) {
			var view = impl.cV;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.cr,
		impl.cU,
		impl.cQ,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.a8 && impl.a8(sendToApp)
			var view = impl.cV;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.ag);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.an) && (_VirtualDom_doc.title = title = doc.an);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.aN;
	var onUrlRequest = impl.aO;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		a8: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bJ === next.bJ
							&& curr.bs === next.bs
							&& curr.bG.a === next.bG.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		cr: function(flags)
		{
			return A3(impl.cr, flags, _Browser_getUrl(), key);
		},
		cV: impl.cV,
		cU: impl.cU,
		cQ: impl.cQ
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cp: 'hidden', cb: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cp: 'mozHidden', cb: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cp: 'msHidden', cb: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cp: 'webkitHidden', cb: 'webkitvisibilitychange' }
		: { cp: 'hidden', cb: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		bR: _Browser_getScene(),
		b1: {
			t: _Browser_window.pageXOffset,
			u: _Browser_window.pageYOffset,
			cW: _Browser_doc.documentElement.clientWidth,
			co: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		cW: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		co: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			bR: {
				cW: node.scrollWidth,
				co: node.scrollHeight
			},
			b1: {
				t: node.scrollLeft,
				u: node.scrollTop,
				cW: node.clientWidth,
				co: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			bR: _Browser_getScene(),
			b1: {
				t: x,
				u: y,
				cW: _Browser_doc.documentElement.clientWidth,
				co: _Browser_doc.documentElement.clientHeight
			},
			cg: {
				t: x + rect.left,
				u: y + rect.top,
				cW: rect.width,
				co: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.h) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.i),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.i);
		} else {
			var treeLen = builder.h * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.j) : builder.j;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.h);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.i) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.i);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{j: nodeList, h: (len / $elm$core$Array$branchFactor) | 0, i: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Main$audioPortFromJS = _Platform_incomingPort('audioPortFromJS', $elm$json$Json$Decode$value);
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $author$project$Main$audioPortToJS = _Platform_outgoingPort('audioPortToJS', $elm$core$Basics$identity);
var $MartinSStewart$elm_audio$Audio$UserMsg = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {bp: fragment, bs: host, bD: path, bG: port_, bJ: protocol, bK: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $MartinSStewart$elm_audio$Audio$getUserModel = function (_v0) {
	var model = _v0;
	return model.G;
};
var $MartinSStewart$elm_audio$Audio$Model = $elm$core$Basics$identity;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $MartinSStewart$elm_audio$Audio$audioSourceBufferId = function (_v0) {
	var audioSource = _v0;
	return audioSource.as;
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $MartinSStewart$elm_audio$Audio$encodeBufferId = function (_v0) {
	var bufferId = _v0;
	return $elm$json$Json$Encode$int(bufferId);
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $ianmackenzie$elm_units$Duration$inSeconds = function (_v0) {
	var numSeconds = _v0;
	return numSeconds;
};
var $ianmackenzie$elm_units$Duration$inMilliseconds = function (duration) {
	return $ianmackenzie$elm_units$Duration$inSeconds(duration) * 1000;
};
var $MartinSStewart$elm_audio$Audio$encodeDuration = A2($elm$core$Basics$composeR, $ianmackenzie$elm_units$Duration$inMilliseconds, $elm$json$Json$Encode$float);
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(0),
			pairs));
};
var $MartinSStewart$elm_audio$Audio$encodeLoopConfig = function (maybeLoop) {
	if (!maybeLoop.$) {
		var loop = maybeLoop.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'loopStart',
					$MartinSStewart$elm_audio$Audio$encodeDuration(loop.by)),
					_Utils_Tuple2(
					'loopEnd',
					$MartinSStewart$elm_audio$Audio$encodeDuration(loop.bx))
				]));
	} else {
		return $elm$json$Json$Encode$null;
	}
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $MartinSStewart$elm_audio$Audio$encodeTime = A2($elm$core$Basics$composeR, $elm$time$Time$posixToMillis, $elm$json$Json$Encode$int);
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(0),
				entries));
	});
var $mgold$elm_nonempty_list$List$Nonempty$toList = function (_v0) {
	var x = _v0.a;
	var xs = _v0.b;
	return A2($elm$core$List$cons, x, xs);
};
var $MartinSStewart$elm_audio$Audio$encodeVolumeTimeline = function (volumeTimeline) {
	return A2(
		$elm$json$Json$Encode$list,
		function (_v0) {
			var time = _v0.a;
			var volume = _v0.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'time',
						$MartinSStewart$elm_audio$Audio$encodeTime(time)),
						_Utils_Tuple2(
						'volume',
						$elm$json$Json$Encode$float(volume))
					]));
		},
		$mgold$elm_nonempty_list$List$Nonempty$toList(volumeTimeline));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $MartinSStewart$elm_audio$Audio$encodeStartSound = F2(
	function (nodeGroupId, audio_) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('startSound')),
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'bufferId',
					$MartinSStewart$elm_audio$Audio$encodeBufferId(
						$MartinSStewart$elm_audio$Audio$audioSourceBufferId(audio_.ad))),
					_Utils_Tuple2(
					'startTime',
					$MartinSStewart$elm_audio$Audio$encodeTime(audio_.ae)),
					_Utils_Tuple2(
					'startAt',
					$MartinSStewart$elm_audio$Audio$encodeDuration(audio_.V)),
					_Utils_Tuple2(
					'volume',
					$elm$json$Json$Encode$float(audio_.ap)),
					_Utils_Tuple2(
					'volumeTimelines',
					A2($elm$json$Json$Encode$list, $MartinSStewart$elm_audio$Audio$encodeVolumeTimeline, audio_.aq)),
					_Utils_Tuple2(
					'loop',
					$MartinSStewart$elm_audio$Audio$encodeLoopConfig(audio_.Z)),
					_Utils_Tuple2(
					'playbackRate',
					$elm$json$Json$Encode$float(audio_.ab))
				]));
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $MartinSStewart$elm_audio$Audio$flattenAudio = function (audio_) {
	switch (audio_.$) {
		case 0:
			var group_ = audio_.a;
			return $elm$core$List$concat(
				A2($elm$core$List$map, $MartinSStewart$elm_audio$Audio$flattenAudio, group_));
		case 1:
			var source = audio_.a.ad;
			var startTime = audio_.a.ae;
			var settings = audio_.a.bT;
			return _List_fromArray(
				[
					{Z: settings.Z, ab: settings.ab, ad: source, V: settings.V, ae: startTime, ap: 1, aq: _List_Nil}
				]);
		default:
			var effect = audio_.a;
			var _v1 = effect.a$;
			if (!_v1.$) {
				var scaleVolume_ = _v1.a;
				return A2(
					$elm$core$List$map,
					function (a) {
						return _Utils_update(
							a,
							{ap: scaleVolume_.bQ * a.ap});
					},
					$MartinSStewart$elm_audio$Audio$flattenAudio(effect.b8));
			} else {
				var volumeAt = _v1.a.b2;
				return A2(
					$elm$core$List$map,
					function (a) {
						return _Utils_update(
							a,
							{
								aq: A2($elm$core$List$cons, volumeAt, a.aq)
							});
					},
					$MartinSStewart$elm_audio$Audio$flattenAudio(effect.b8));
			}
	}
};
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $MartinSStewart$elm_audio$Audio$encodeSetLoopConfig = F2(
	function (nodeGroupId, loop) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setLoopConfig')),
					_Utils_Tuple2(
					'loop',
					$MartinSStewart$elm_audio$Audio$encodeLoopConfig(loop))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetPlaybackRate = F2(
	function (nodeGroupId, playbackRate) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setPlaybackRate')),
					_Utils_Tuple2(
					'playbackRate',
					$elm$json$Json$Encode$float(playbackRate))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetVolume = F2(
	function (nodeGroupId, volume) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setVolume')),
					_Utils_Tuple2(
					'volume',
					$elm$json$Json$Encode$float(volume))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeSetVolumeAt = F2(
	function (nodeGroupId, volumeTimelines) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'nodeGroupId',
					$elm$json$Json$Encode$int(nodeGroupId)),
					_Utils_Tuple2(
					'action',
					$elm$json$Json$Encode$string('setVolumeAt')),
					_Utils_Tuple2(
					'volumeAt',
					A2($elm$json$Json$Encode$list, $MartinSStewart$elm_audio$Audio$encodeVolumeTimeline, volumeTimelines))
				]));
	});
var $MartinSStewart$elm_audio$Audio$encodeStopSound = function (nodeGroupId) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'action',
				$elm$json$Json$Encode$string('stopSound')),
				_Utils_Tuple2(
				'nodeGroupId',
				$elm$json$Json$Encode$int(nodeGroupId))
			]));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $MartinSStewart$elm_audio$Audio$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $MartinSStewart$elm_audio$Audio$removeAt = F2(
	function (index, l) {
		if (index < 0) {
			return l;
		} else {
			var tail = $elm$core$List$tail(
				A2($elm$core$List$drop, index, l));
			var head = A2($elm$core$List$take, index, l);
			if (tail.$ === 1) {
				return l;
			} else {
				var t = tail.a;
				return A2($elm$core$List$append, head, t);
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$updateAudioState = F2(
	function (_v0, _v1) {
		var nodeGroupId = _v0.a;
		var audioGroup = _v0.b;
		var flattenedAudio = _v1.a;
		var audioState = _v1.b;
		var json = _v1.c;
		var validAudio = A2(
			$elm$core$List$filter,
			function (_v7) {
				var a = _v7.b;
				return _Utils_eq(a.ad, audioGroup.ad) && (_Utils_eq(a.ae, audioGroup.ae) && _Utils_eq(a.V, audioGroup.V));
			},
			A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, flattenedAudio));
		var _v2 = A2(
			$MartinSStewart$elm_audio$Audio$find,
			function (_v3) {
				var a = _v3.b;
				return _Utils_eq(a, audioGroup);
			},
			validAudio);
		if (!_v2.$) {
			var _v4 = _v2.a;
			var index = _v4.a;
			return _Utils_Tuple3(
				A2($MartinSStewart$elm_audio$Audio$removeAt, index, flattenedAudio),
				audioState,
				json);
		} else {
			if (validAudio.b) {
				var _v6 = validAudio.a;
				var index = _v6.a;
				var a = _v6.b;
				var encodeValue = F2(
					function (getter, encoder) {
						return _Utils_eq(
							getter(audioGroup),
							getter(a)) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
							A2(
								encoder,
								nodeGroupId,
								getter(a)));
					});
				var effects = A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							A2(
							encodeValue,
							function ($) {
								return $.ap;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetVolume),
							A2(
							encodeValue,
							function ($) {
								return $.Z;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetLoopConfig),
							A2(
							encodeValue,
							function ($) {
								return $.ab;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetPlaybackRate),
							A2(
							encodeValue,
							function ($) {
								return $.aq;
							},
							$MartinSStewart$elm_audio$Audio$encodeSetVolumeAt)
						]));
				return _Utils_Tuple3(
					A2($MartinSStewart$elm_audio$Audio$removeAt, index, flattenedAudio),
					A3($elm$core$Dict$insert, nodeGroupId, a, audioState),
					_Utils_ap(effects, json));
			} else {
				return _Utils_Tuple3(
					flattenedAudio,
					A2($elm$core$Dict$remove, nodeGroupId, audioState),
					A2(
						$elm$core$List$cons,
						$MartinSStewart$elm_audio$Audio$encodeStopSound(nodeGroupId),
						json));
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$diffAudioState = F3(
	function (nodeGroupIdCounter, audioState, newAudio) {
		var _v0 = A3(
			$elm$core$List$foldl,
			$MartinSStewart$elm_audio$Audio$updateAudioState,
			_Utils_Tuple3(
				$MartinSStewart$elm_audio$Audio$flattenAudio(newAudio),
				audioState,
				_List_Nil),
			$elm$core$Dict$toList(audioState));
		var newAudioLeft = _v0.a;
		var newAudioState = _v0.b;
		var json2 = _v0.c;
		var _v1 = A3(
			$elm$core$List$foldl,
			F2(
				function (audioLeft, _v2) {
					var counter = _v2.a;
					var audioState_ = _v2.b;
					var json_ = _v2.c;
					return _Utils_Tuple3(
						counter + 1,
						A3($elm$core$Dict$insert, counter, audioLeft, audioState_),
						A2(
							$elm$core$List$cons,
							A2($MartinSStewart$elm_audio$Audio$encodeStartSound, counter, audioLeft),
							json_));
				}),
			_Utils_Tuple3(nodeGroupIdCounter, newAudioState, json2),
			newAudioLeft);
		var newNodeGroupIdCounter = _v1.a;
		var newAudioState2 = _v1.b;
		var json3 = _v1.c;
		return _Utils_Tuple3(newAudioState2, newNodeGroupIdCounter, json3);
	});
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $MartinSStewart$elm_audio$Audio$encodeAudioLoadRequest = F2(
	function (index, audioLoad) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audioUrl',
					$elm$json$Json$Encode$string(audioLoad.X)),
					_Utils_Tuple2(
					'requestId',
					$elm$json$Json$Encode$int(index))
				]));
	});
var $MartinSStewart$elm_audio$Audio$flattenAudioCmd = function (audioCmd) {
	if (!audioCmd.$) {
		var data = audioCmd.a;
		return _List_fromArray(
			[data]);
	} else {
		var list = audioCmd.a;
		return $elm$core$List$concat(
			A2($elm$core$List$map, $MartinSStewart$elm_audio$Audio$flattenAudioCmd, list));
	}
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $MartinSStewart$elm_audio$Audio$encodeAudioCmd = F2(
	function (_v0, audioCmd) {
		var model = _v0;
		var flattenedAudioCmd = $MartinSStewart$elm_audio$Audio$flattenAudioCmd(audioCmd);
		var newPendingRequests = A2($elm$core$List$indexedMap, $elm$core$Tuple$pair, flattenedAudioCmd);
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					g: A2(
						$elm$core$Dict$union,
						model.g,
						$elm$core$Dict$fromList(newPendingRequests)),
					E: model.E + $elm$core$List$length(flattenedAudioCmd)
				}),
			A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				A2(
					$elm$core$List$map,
					function (_v1) {
						var index = _v1.a;
						var value = _v1.b;
						return A2($MartinSStewart$elm_audio$Audio$encodeAudioLoadRequest, model.E + index, value);
					},
					newPendingRequests)));
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $MartinSStewart$elm_audio$Audio$initHelper = F3(
	function (audioPort, audioFunc, _v0) {
		var model = _v0.a;
		var cmds = _v0.b;
		var audioCmds = _v0.c;
		var _v1 = A3(
			$MartinSStewart$elm_audio$Audio$diffAudioState,
			0,
			$elm$core$Dict$empty,
			audioFunc(model));
		var audioState = _v1.a;
		var newNodeGroupIdCounter = _v1.b;
		var json = _v1.c;
		var initialModel = {H: audioState, I: newNodeGroupIdCounter, g: $elm$core$Dict$empty, E: 0, F: $elm$core$Maybe$Nothing, G: model};
		var _v2 = A2($MartinSStewart$elm_audio$Audio$encodeAudioCmd, initialModel, audioCmds);
		var initialModel2 = _v2.a;
		var audioRequests = _v2.b;
		var portMessage = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audio',
					A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, json)),
					_Utils_Tuple2('audioCmds', audioRequests)
				]));
		return _Utils_Tuple2(
			initialModel2,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $MartinSStewart$elm_audio$Audio$UserMsg, cmds),
						audioPort(portMessage)
					])));
	});
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $MartinSStewart$elm_audio$Audio$FromJSMsg = function (a) {
	return {$: 0, a: a};
};
var $MartinSStewart$elm_audio$Audio$JsonParseError = function (a) {
	return {$: 3, a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioLoadFailed = function (a) {
	return {$: 1, a: a};
};
var $MartinSStewart$elm_audio$Audio$AudioLoadSuccess = function (a) {
	return {$: 0, a: a};
};
var $MartinSStewart$elm_audio$Audio$InitAudioContext = function (a) {
	return {$: 2, a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $MartinSStewart$elm_audio$Audio$BufferId = $elm$core$Basics$identity;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $MartinSStewart$elm_audio$Audio$decodeBufferId = A2($elm$json$Json$Decode$map, $elm$core$Basics$identity, $elm$json$Json$Decode$int);
var $MartinSStewart$elm_audio$Audio$MediaDecodeAudioDataUnknownContentType = 0;
var $MartinSStewart$elm_audio$Audio$NetworkError = 1;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $MartinSStewart$elm_audio$Audio$decodeLoadError = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		switch (value) {
			case 'NetworkError':
				return $elm$json$Json$Decode$succeed(1);
			case 'MediaDecodeAudioDataUnknownContentType':
				return $elm$json$Json$Decode$succeed(0);
			default:
				return $elm$json$Json$Decode$fail('Unknown load error');
		}
	},
	$elm$json$Json$Decode$string);
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $ianmackenzie$elm_units$Quantity$Quantity = $elm$core$Basics$identity;
var $ianmackenzie$elm_units$Duration$seconds = function (numSeconds) {
	return numSeconds;
};
var $MartinSStewart$elm_audio$Audio$decodeFromJSMsg = A2(
	$elm$json$Json$Decode$andThen,
	function (value) {
		switch (value) {
			case 0:
				return A3(
					$elm$json$Json$Decode$map2,
					F2(
						function (requestId, error) {
							return $MartinSStewart$elm_audio$Audio$AudioLoadFailed(
								{at: error, aU: requestId});
						}),
					A2($elm$json$Json$Decode$field, 'requestId', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'error', $MartinSStewart$elm_audio$Audio$decodeLoadError));
			case 1:
				return A4(
					$elm$json$Json$Decode$map3,
					F3(
						function (requestId, bufferId, duration) {
							return $MartinSStewart$elm_audio$Audio$AudioLoadSuccess(
								{
									as: bufferId,
									bm: $ianmackenzie$elm_units$Duration$seconds(duration),
									aU: requestId
								});
						}),
					A2($elm$json$Json$Decode$field, 'requestId', $elm$json$Json$Decode$int),
					A2($elm$json$Json$Decode$field, 'bufferId', $MartinSStewart$elm_audio$Audio$decodeBufferId),
					A2($elm$json$Json$Decode$field, 'durationInSeconds', $elm$json$Json$Decode$float));
			case 2:
				return A2(
					$elm$json$Json$Decode$map,
					function (samplesPerSecond) {
						return $MartinSStewart$elm_audio$Audio$InitAudioContext(
							{F: samplesPerSecond});
					},
					A2($elm$json$Json$Decode$field, 'samplesPerSecond', $elm$json$Json$Decode$int));
			default:
				return $elm$json$Json$Decode$succeed(
					$MartinSStewart$elm_audio$Audio$JsonParseError(
						{
							at: 'Type ' + ($elm$core$String$fromInt(value) + ' not handled.')
						}));
		}
	},
	A2($elm$json$Json$Decode$field, 'type', $elm$json$Json$Decode$int));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $MartinSStewart$elm_audio$Audio$fromJSPortSub = function (json) {
	var _v0 = A2($elm$json$Json$Decode$decodeValue, $MartinSStewart$elm_audio$Audio$decodeFromJSMsg, json);
	if (!_v0.$) {
		var value = _v0.a;
		return $MartinSStewart$elm_audio$Audio$FromJSMsg(value);
	} else {
		var error = _v0.a;
		return $MartinSStewart$elm_audio$Audio$FromJSMsg(
			$MartinSStewart$elm_audio$Audio$JsonParseError(
				{
					at: $elm$json$Json$Decode$errorToString(error)
				}));
	}
};
var $elm$core$Platform$Sub$map = _Platform_map;
var $MartinSStewart$elm_audio$Audio$subscriptions = F2(
	function (app, _v0) {
		var model = _v0;
		return $elm$core$Platform$Sub$batch(
			_List_fromArray(
				[
					A2(
					$elm$core$Platform$Sub$map,
					$MartinSStewart$elm_audio$Audio$UserMsg,
					app.cQ(model.G)),
					app.b9.cm($MartinSStewart$elm_audio$Audio$fromJSPortSub)
				]));
	});
var $MartinSStewart$elm_audio$Audio$File = $elm$core$Basics$identity;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $mgold$elm_nonempty_list$List$Nonempty$head = function (_v0) {
	var x = _v0.a;
	var xs = _v0.b;
	return x;
};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $MartinSStewart$elm_audio$Audio$updateHelper = F4(
	function (audioPort, audioFunc, userUpdate, _v0) {
		var model = _v0;
		var _v1 = userUpdate(model.G);
		var newUserModel = _v1.a;
		var userCmd = _v1.b;
		var audioCmds = _v1.c;
		var _v2 = A3(
			$MartinSStewart$elm_audio$Audio$diffAudioState,
			model.I,
			model.H,
			audioFunc(newUserModel));
		var audioState = _v2.a;
		var newNodeGroupIdCounter = _v2.b;
		var json = _v2.c;
		var newModel = _Utils_update(
			model,
			{H: audioState, I: newNodeGroupIdCounter, G: newUserModel});
		var _v3 = A2($MartinSStewart$elm_audio$Audio$encodeAudioCmd, newModel, audioCmds);
		var newModel2 = _v3.a;
		var audioRequests = _v3.b;
		var portMessage = $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'audio',
					A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, json)),
					_Utils_Tuple2('audioCmds', audioRequests)
				]));
		return _Utils_Tuple2(
			newModel2,
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						A2($elm$core$Platform$Cmd$map, $MartinSStewart$elm_audio$Audio$UserMsg, userCmd),
						audioPort(portMessage)
					])));
	});
var $MartinSStewart$elm_audio$Audio$update = F3(
	function (app, msg, _v0) {
		var model = _v0;
		if (msg.$ === 1) {
			var userMsg = msg.a;
			return A4(
				$MartinSStewart$elm_audio$Audio$updateHelper,
				app.b9.cT,
				app.b8,
				app.cU(userMsg),
				model);
		} else {
			var response = msg.a;
			switch (response.$) {
				case 0:
					var requestId = response.a.aU;
					var bufferId = response.a.as;
					var duration = response.a.bm;
					var _v3 = A2($elm$core$Dict$get, requestId, model.g);
					if (!_v3.$) {
						var pendingRequest = _v3.a;
						var a = $elm$core$Result$Ok(
							{as: bufferId});
						var b = A2(
							$MartinSStewart$elm_audio$Audio$find,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Tuple$first,
								$elm$core$Basics$eq(a)),
							$mgold$elm_nonempty_list$List$Nonempty$toList(pendingRequest.O));
						if (!b.$) {
							var _v5 = b.a;
							var userMsg = _v5.b;
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.b9.cT,
								app.b8,
								app.cU(userMsg),
								_Utils_update(
									model,
									{
										g: A2($elm$core$Dict$remove, requestId, model.g)
									}));
						} else {
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.b9.cT,
								app.b8,
								app.cU(
									$mgold$elm_nonempty_list$List$Nonempty$head(pendingRequest.O).b),
								_Utils_update(
									model,
									{
										g: A2($elm$core$Dict$remove, requestId, model.g)
									}));
						}
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				case 1:
					var requestId = response.a.aU;
					var error = response.a.at;
					var _v6 = A2($elm$core$Dict$get, requestId, model.g);
					if (!_v6.$) {
						var pendingRequest = _v6.a;
						var a = $elm$core$Result$Err(error);
						var b = A2(
							$MartinSStewart$elm_audio$Audio$find,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$Tuple$first,
								$elm$core$Basics$eq(a)),
							$mgold$elm_nonempty_list$List$Nonempty$toList(pendingRequest.O));
						if (!b.$) {
							var _v8 = b.a;
							var userMsg = _v8.b;
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.b9.cT,
								app.b8,
								app.cU(userMsg),
								_Utils_update(
									model,
									{
										g: A2($elm$core$Dict$remove, requestId, model.g)
									}));
						} else {
							return A4(
								$MartinSStewart$elm_audio$Audio$updateHelper,
								app.b9.cT,
								app.b8,
								app.cU(
									$mgold$elm_nonempty_list$List$Nonempty$head(pendingRequest.O).b),
								_Utils_update(
									model,
									{
										g: A2($elm$core$Dict$remove, requestId, model.g)
									}));
						}
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				case 2:
					var samplesPerSecond = response.a.F;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								F: $elm$core$Maybe$Just(samplesPerSecond)
							}),
						$elm$core$Platform$Cmd$none);
				default:
					var error = response.a.at;
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
		}
	});
var $MartinSStewart$elm_audio$Audio$elementWithAudio = function (app) {
	return $elm$browser$Browser$element(
		{
			cr: A2(
				$elm$core$Basics$composeR,
				app.cr,
				A2($MartinSStewart$elm_audio$Audio$initHelper, app.b9.cT, app.b8)),
			cQ: $MartinSStewart$elm_audio$Audio$subscriptions(app),
			cU: $MartinSStewart$elm_audio$Audio$update(app),
			cV: A2(
				$elm$core$Basics$composeR,
				$MartinSStewart$elm_audio$Audio$getUserModel,
				A2(
					$elm$core$Basics$composeR,
					app.cV,
					$elm$html$Html$map($MartinSStewart$elm_audio$Audio$UserMsg)))
		});
};
var $ianmackenzie$elm_units$Quantity$zero = 0;
var $MartinSStewart$elm_audio$Audio$audioDefaultConfig = {Z: $elm$core$Maybe$Nothing, ab: 1, V: $ianmackenzie$elm_units$Quantity$zero};
var $MartinSStewart$elm_audio$Audio$BasicAudio = function (a) {
	return {$: 1, a: a};
};
var $MartinSStewart$elm_audio$Audio$audioWithConfig = F3(
	function (audioSettings, source, startTime) {
		return $MartinSStewart$elm_audio$Audio$BasicAudio(
			{bT: audioSettings, ad: source, ae: startTime});
	});
var $MartinSStewart$elm_audio$Audio$audio = F2(
	function (source, startTime) {
		return A3($MartinSStewart$elm_audio$Audio$audioWithConfig, $MartinSStewart$elm_audio$Audio$audioDefaultConfig, source, startTime);
	});
var $MartinSStewart$elm_audio$Audio$Group = function (a) {
	return {$: 0, a: a};
};
var $MartinSStewart$elm_audio$Audio$group = function (audios) {
	return $MartinSStewart$elm_audio$Audio$Group(audios);
};
var $MartinSStewart$elm_audio$Audio$silence = $MartinSStewart$elm_audio$Audio$group(_List_Nil);
var $author$project$Main$gameToAudio = function (game) {
	var _v0 = game.am;
	if (_v0.$ === 1) {
		var x = _v0.a;
		var _v1 = x.bW;
		switch (_v1.$) {
			case 0:
				return $MartinSStewart$elm_audio$Audio$silence;
			case 1:
				var time = _v1.a;
				return A2($MartinSStewart$elm_audio$Audio$audio, x.am, time);
			default:
				return $MartinSStewart$elm_audio$Audio$silence;
		}
	} else {
		return $MartinSStewart$elm_audio$Audio$silence;
	}
};
var $author$project$Types$GameModels$SoundLoaded = function (a) {
	return {$: 11, a: a};
};
var $author$project$Types$GameModels$Init = 2;
var $author$project$Types$GameModels$Blinky = 0;
var $author$project$Types$GameModels$Blue = 2;
var $author$project$Types$GameModels$Clyde = 3;
var $author$project$Types$GameModels$Inky = 2;
var $author$project$Types$GameModels$Left = 2;
var $author$project$Types$GameModels$LoadingModel = {$: 0};
var $author$project$Types$GameModels$None = 4;
var $author$project$Types$GameModels$Pink = 1;
var $author$project$Types$GameModels$Pinky = 1;
var $author$project$Types$GameModels$Red = 0;
var $author$project$Types$GameModels$Right = 3;
var $author$project$Types$GameModels$Stopped = function (a) {
	return {$: 1, a: a};
};
var $author$project$Types$GameModels$Up = 0;
var $author$project$Types$GameModels$Waiting = {$: 2};
var $author$project$Types$GameModels$Yellow = 3;
var $author$project$Settings$gameMessages = {cn: 'GAME OVER!', bB: '', cG: 'PAUSE', cL: 'READY!'};
var $author$project$Types$Line$Both = 3;
var $author$project$Types$Line$Line = F3(
	function (start, end, linetype) {
		return {aJ: end, bw: linetype, aX: start};
	});
var $author$project$Settings$itemSettings = {bo: '#FFAAA5', a3: 4000, aW: 5, cO: 15, aF: 10};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $author$project$Eatable$moveToWards = F3(
	function (from, to, lenght) {
		return {
			t: from.t + A2($elm$core$Basics$min, lenght, to.t - from.t),
			u: from.u + A2($elm$core$Basics$min, lenght, to.u - from.u)
		};
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Eatable$createPoints = F2(
	function (line, pointList) {
		var startPoint = {
			t: A2($elm$core$Basics$min, line.aX.t, line.aJ.t),
			u: A2($elm$core$Basics$min, line.aX.u, line.aJ.u)
		};
		var endPoint = {
			t: A2($elm$core$Basics$max, line.aX.t, line.aJ.t),
			u: A2($elm$core$Basics$max, line.aX.u, line.aJ.u)
		};
		var currentPoint = A3($author$project$Eatable$moveToWards, startPoint, endPoint, $author$project$Settings$itemSettings.cO);
		return (line.bw === 3) ? ((!_Utils_eq(startPoint, endPoint)) ? A2(
			$elm$core$List$cons,
			startPoint,
			A2(
				$author$project$Eatable$createPoints,
				A3($author$project$Types$Line$Line, currentPoint, endPoint, line.bw),
				pointList)) : A2($elm$core$List$cons, currentPoint, pointList)) : pointList;
	});
var $Chadtech$unique_list$List$Unique$UniqueList = $elm$core$Basics$identity;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $Chadtech$unique_list$List$Unique$consIfNotMember = F2(
	function (el, list) {
		return A2($elm$core$List$member, el, list) ? list : A2($elm$core$List$cons, el, list);
	});
var $Chadtech$unique_list$List$Unique$fromList = function (list) {
	return A3($elm$core$List$foldr, $Chadtech$unique_list$List$Unique$consIfNotMember, _List_Nil, list);
};
var $Chadtech$unique_list$List$Unique$toList = function (_v0) {
	var list = _v0;
	return list;
};
var $Chadtech$unique_list$List$Unique$filterDuplicates = A2($elm$core$Basics$composeR, $Chadtech$unique_list$List$Unique$fromList, $Chadtech$unique_list$List$Unique$toList);
var $author$project$Settings$pillsList = _List_fromArray(
	[
		{t: 25, u: 115},
		{t: 55, u: 400},
		{t: 385, u: 55},
		{t: 325, u: 430}
	]);
var $author$project$Types$Line$GhostStartLine = 2;
var $author$project$Types$Line$Pacman = 0;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $author$project$Settings$pointMesh = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			1,
			{t: 25, u: 25}),
			_Utils_Tuple2(
			2,
			{t: 115, u: 25}),
			_Utils_Tuple2(
			3,
			{t: 220, u: 25}),
			_Utils_Tuple2(
			4,
			{t: 280, u: 25}),
			_Utils_Tuple2(
			5,
			{t: 385, u: 25}),
			_Utils_Tuple2(
			6,
			{t: 475, u: 25}),
			_Utils_Tuple2(
			7,
			{t: 25, u: 85}),
			_Utils_Tuple2(
			8,
			{t: 175, u: 85}),
			_Utils_Tuple2(
			9,
			{t: 220, u: 85}),
			_Utils_Tuple2(
			10,
			{t: 280, u: 85}),
			_Utils_Tuple2(
			11,
			{t: 325, u: 85}),
			_Utils_Tuple2(
			12,
			{t: 475, u: 85}),
			_Utils_Tuple2(
			13,
			{t: 25, u: 145}),
			_Utils_Tuple2(
			14,
			{t: 115, u: 145}),
			_Utils_Tuple2(
			15,
			{t: 175, u: 145}),
			_Utils_Tuple2(
			16,
			{t: 220, u: 145}),
			_Utils_Tuple2(
			17,
			{t: 280, u: 145}),
			_Utils_Tuple2(
			18,
			{t: 325, u: 145}),
			_Utils_Tuple2(
			19,
			{t: 385, u: 145}),
			_Utils_Tuple2(
			20,
			{t: 475, u: 145}),
			_Utils_Tuple2(
			21,
			{t: 175, u: 190}),
			_Utils_Tuple2(
			22,
			{t: 220, u: 190}),
			_Utils_Tuple2(
			23,
			{t: 280, u: 190}),
			_Utils_Tuple2(
			24,
			{t: 325, u: 190}),
			_Utils_Tuple2(
			25,
			{t: -5, u: 235}),
			_Utils_Tuple2(
			26,
			{t: 175, u: 235}),
			_Utils_Tuple2(
			27,
			{t: 325, u: 235}),
			_Utils_Tuple2(
			28,
			{t: 505, u: 235}),
			_Utils_Tuple2(
			29,
			{t: 175, u: 280}),
			_Utils_Tuple2(
			30,
			{t: 325, u: 280}),
			_Utils_Tuple2(
			31,
			{t: 25, u: 325}),
			_Utils_Tuple2(
			32,
			{t: 175, u: 325}),
			_Utils_Tuple2(
			33,
			{t: 220, u: 325}),
			_Utils_Tuple2(
			34,
			{t: 280, u: 325}),
			_Utils_Tuple2(
			35,
			{t: 325, u: 325}),
			_Utils_Tuple2(
			36,
			{t: 475, u: 325}),
			_Utils_Tuple2(
			37,
			{t: 25, u: 370}),
			_Utils_Tuple2(
			38,
			{t: 55, u: 370}),
			_Utils_Tuple2(
			39,
			{t: 115, u: 370}),
			_Utils_Tuple2(
			40,
			{t: 175, u: 370}),
			_Utils_Tuple2(
			41,
			{t: 220, u: 370}),
			_Utils_Tuple2(
			42,
			{t: 280, u: 370}),
			_Utils_Tuple2(
			43,
			{t: 325, u: 370}),
			_Utils_Tuple2(
			44,
			{t: 385, u: 370}),
			_Utils_Tuple2(
			45,
			{t: 445, u: 370}),
			_Utils_Tuple2(
			46,
			{t: 475, u: 370}),
			_Utils_Tuple2(
			47,
			{t: 25, u: 430}),
			_Utils_Tuple2(
			48,
			{t: 55, u: 430}),
			_Utils_Tuple2(
			49,
			{t: 115, u: 430}),
			_Utils_Tuple2(
			50,
			{t: 175, u: 430}),
			_Utils_Tuple2(
			51,
			{t: 220, u: 430}),
			_Utils_Tuple2(
			52,
			{t: 280, u: 430}),
			_Utils_Tuple2(
			53,
			{t: 325, u: 430}),
			_Utils_Tuple2(
			54,
			{t: 385, u: 430}),
			_Utils_Tuple2(
			55,
			{t: 445, u: 430}),
			_Utils_Tuple2(
			56,
			{t: 475, u: 430}),
			_Utils_Tuple2(
			57,
			{t: 25, u: 475}),
			_Utils_Tuple2(
			58,
			{t: 220, u: 475}),
			_Utils_Tuple2(
			59,
			{t: 280, u: 475}),
			_Utils_Tuple2(
			60,
			{t: 475, u: 475}),
			_Utils_Tuple2(
			61,
			{t: 385, u: 235}),
			_Utils_Tuple2(
			62,
			{t: 115, u: 235}),
			_Utils_Tuple2(
			63,
			{t: 220, u: 235}),
			_Utils_Tuple2(
			64,
			{t: 280, u: 235}),
			_Utils_Tuple2(
			65,
			{t: 250, u: 235}),
			_Utils_Tuple2(
			66,
			{t: 250, u: 190})
		]));
var $author$project$Settings$getPoint = function (i) {
	var _v0 = A2($elm$core$Dict$get, i, $author$project$Settings$pointMesh);
	if (!_v0.$) {
		var point = _v0.a;
		return point;
	} else {
		return {t: 0, u: 0};
	}
};
var $author$project$Settings$runMesh = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			1,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(29),
				$author$project$Settings$getPoint(30),
				3)),
			_Utils_Tuple2(
			2,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(21),
				$author$project$Settings$getPoint(32),
				3)),
			_Utils_Tuple2(
			3,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(21),
				$author$project$Settings$getPoint(24),
				3)),
			_Utils_Tuple2(
			4,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(24),
				$author$project$Settings$getPoint(35),
				3)),
			_Utils_Tuple2(
			5,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(1),
				$author$project$Settings$getPoint(3),
				3)),
			_Utils_Tuple2(
			6,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(1),
				$author$project$Settings$getPoint(13),
				3)),
			_Utils_Tuple2(
			7,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(7),
				$author$project$Settings$getPoint(12),
				3)),
			_Utils_Tuple2(
			8,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(2),
				$author$project$Settings$getPoint(49),
				3)),
			_Utils_Tuple2(
			9,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(3),
				$author$project$Settings$getPoint(9),
				3)),
			_Utils_Tuple2(
			10,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(4),
				$author$project$Settings$getPoint(10),
				3)),
			_Utils_Tuple2(
			11,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(4),
				$author$project$Settings$getPoint(6),
				3)),
			_Utils_Tuple2(
			12,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(5),
				$author$project$Settings$getPoint(54),
				3)),
			_Utils_Tuple2(
			13,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(6),
				$author$project$Settings$getPoint(20),
				3)),
			_Utils_Tuple2(
			14,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(19),
				$author$project$Settings$getPoint(20),
				3)),
			_Utils_Tuple2(
			15,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(11),
				$author$project$Settings$getPoint(18),
				3)),
			_Utils_Tuple2(
			16,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(17),
				$author$project$Settings$getPoint(18),
				3)),
			_Utils_Tuple2(
			17,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(17),
				$author$project$Settings$getPoint(23),
				3)),
			_Utils_Tuple2(
			18,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(8),
				$author$project$Settings$getPoint(15),
				3)),
			_Utils_Tuple2(
			19,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(15),
				$author$project$Settings$getPoint(16),
				3)),
			_Utils_Tuple2(
			20,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(16),
				$author$project$Settings$getPoint(22),
				3)),
			_Utils_Tuple2(
			21,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(25),
				$author$project$Settings$getPoint(62),
				0)),
			_Utils_Tuple2(
			22,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(61),
				$author$project$Settings$getPoint(28),
				0)),
			_Utils_Tuple2(
			23,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(31),
				$author$project$Settings$getPoint(33),
				3)),
			_Utils_Tuple2(
			24,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(34),
				$author$project$Settings$getPoint(36),
				3)),
			_Utils_Tuple2(
			25,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(39),
				$author$project$Settings$getPoint(44),
				3)),
			_Utils_Tuple2(
			26,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(33),
				$author$project$Settings$getPoint(41),
				3)),
			_Utils_Tuple2(
			27,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(34),
				$author$project$Settings$getPoint(42),
				3)),
			_Utils_Tuple2(
			28,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(31),
				$author$project$Settings$getPoint(37),
				3)),
			_Utils_Tuple2(
			29,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(37),
				$author$project$Settings$getPoint(38),
				3)),
			_Utils_Tuple2(
			30,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(38),
				$author$project$Settings$getPoint(48),
				3)),
			_Utils_Tuple2(
			31,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(47),
				$author$project$Settings$getPoint(49),
				3)),
			_Utils_Tuple2(
			32,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(57),
				$author$project$Settings$getPoint(60),
				3)),
			_Utils_Tuple2(
			33,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(40),
				$author$project$Settings$getPoint(50),
				3)),
			_Utils_Tuple2(
			34,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(50),
				$author$project$Settings$getPoint(51),
				3)),
			_Utils_Tuple2(
			35,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(51),
				$author$project$Settings$getPoint(58),
				3)),
			_Utils_Tuple2(
			36,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(57),
				$author$project$Settings$getPoint(47),
				3)),
			_Utils_Tuple2(
			37,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(43),
				$author$project$Settings$getPoint(53),
				3)),
			_Utils_Tuple2(
			38,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(52),
				$author$project$Settings$getPoint(53),
				3)),
			_Utils_Tuple2(
			39,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(52),
				$author$project$Settings$getPoint(59),
				3)),
			_Utils_Tuple2(
			40,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(36),
				$author$project$Settings$getPoint(46),
				3)),
			_Utils_Tuple2(
			41,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(45),
				$author$project$Settings$getPoint(46),
				3)),
			_Utils_Tuple2(
			42,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(45),
				$author$project$Settings$getPoint(55),
				3)),
			_Utils_Tuple2(
			43,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(54),
				$author$project$Settings$getPoint(56),
				3)),
			_Utils_Tuple2(
			44,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(56),
				$author$project$Settings$getPoint(60),
				3)),
			_Utils_Tuple2(
			45,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(13),
				$author$project$Settings$getPoint(14),
				3)),
			_Utils_Tuple2(
			46,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(27),
				$author$project$Settings$getPoint(61),
				3)),
			_Utils_Tuple2(
			47,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(26),
				$author$project$Settings$getPoint(62),
				3)),
			_Utils_Tuple2(
			48,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(63),
				$author$project$Settings$getPoint(64),
				2)),
			_Utils_Tuple2(
			49,
			A3(
				$author$project$Types$Line$Line,
				$author$project$Settings$getPoint(65),
				$author$project$Settings$getPoint(66),
				2))
		]));
var $elm$core$Basics$not = _Basics_not;
var $author$project$Main$substractList = F2(
	function (a, b) {
		return A2(
			$elm$core$List$filter,
			function (x) {
				return !A2($elm$core$List$member, x, a);
			},
			b);
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Main$getFullItemList = A2(
	$author$project$Main$substractList,
	$author$project$Settings$pillsList,
	$Chadtech$unique_list$List$Unique$filterDuplicates(
		A3(
			$elm$core$List$foldl,
			$author$project$Eatable$createPoints,
			_List_Nil,
			$elm$core$Dict$values($author$project$Settings$runMesh))));
var $author$project$Types$Ghost$getGhostSrc = F2(
	function (color, dir) {
		var ghostSrcDir = function () {
			switch (dir) {
				case 2:
					return '_left';
				case 3:
					return '_right';
				case 0:
					return '_up';
				case 1:
					return '_down';
				default:
					return '';
			}
		}();
		switch (color) {
			case 0:
				return 'blinky/blinky' + ghostSrcDir;
			case 1:
				return 'pinky/pinky' + ghostSrcDir;
			case 3:
				return 'clyde/clyde' + ghostSrcDir;
			case 2:
				return 'inky/inky' + ghostSrcDir;
			case 4:
				return 'hunted';
			case 5:
				return 'hunted_white';
			default:
				return 'eyes/eyes' + ghostSrcDir;
		}
	});
var $author$project$Settings$pacSettings = {
	cd: 'Assets/img/pacman/pacman_closed_mouth.svg',
	aP: 'Assets/img/pacman/pacman_opened_mouth.svg',
	aT: 22,
	N: {t: 247, u: 370}
};
var $author$project$Settings$ghostSettings = {
	bg: {t: 220, u: 235},
	a5: {t: 250, u: 235},
	aT: $author$project$Settings$pacSettings.aT,
	N: {t: 250, u: 190},
	b5: {t: 280, u: 235}
};
var $author$project$Main$resetGame = F7(
	function (newLife, newScore, prevItemList, prevPillsList, prevItemCounter, prevLevel, mode) {
		var newState = function () {
			if (mode === 2) {
				return $author$project$Types$GameModels$Waiting;
			} else {
				return $author$project$Types$GameModels$Stopped(4);
			}
		}();
		var newPillsList = function () {
			if (mode === 1) {
				return prevPillsList;
			} else {
				return $author$project$Settings$pillsList;
			}
		}();
		var newLevel = function () {
			if (!mode) {
				return prevLevel + 1;
			} else {
				return prevLevel;
			}
		}();
		var newItemList = function () {
			if (mode === 1) {
				return prevItemList;
			} else {
				return $author$project$Main$getFullItemList;
			}
		}();
		var newItemCounter = function () {
			if (mode === 1) {
				return prevItemCounter;
			} else {
				return 0;
			}
		}();
		return {
			bf: {
				aG: false,
				aH: 2,
				aI: 3,
				f: false,
				aL: 2,
				aM: 2,
				ak: $author$project$Settings$ghostSettings.bg,
				b: false,
				U: A2($author$project$Types$Ghost$getGhostSrc, 2, 0)
			},
			aZ: true,
			a_: 4,
			C: 0,
			au: false,
			av: 0,
			ai: newItemCounter,
			aw: newItemList,
			y: newLevel,
			z: newLife,
			aj: $author$project$Settings$gameMessages.cL,
			ay: true,
			e: 3,
			S: false,
			aR: $author$project$Settings$pacSettings.N,
			L: 0,
			_: $author$project$Settings$pacSettings.aP,
			cH: false,
			cI: 0,
			aS: newPillsList,
			bF: {
				aG: false,
				aH: 1,
				aI: 0,
				f: false,
				aL: 1,
				aM: 4,
				ak: $author$project$Settings$ghostSettings.a5,
				b: false,
				U: A2($author$project$Types$Ghost$getGhostSrc, 1, 0)
			},
			bL: {
				aG: true,
				aH: 0,
				aI: 4,
				f: false,
				aL: 0,
				aM: 0,
				ak: $author$project$Settings$ghostSettings.N,
				b: true,
				U: A2($author$project$Types$Ghost$getGhostSrc, 0, 3)
			},
			M: newScore,
			a7: {
				cv: '',
				cJ: {t: 0, u: 0}
			},
			a9: false,
			am: $author$project$Types$GameModels$LoadingModel,
			a: newState,
			b$: $elm$core$List$length($author$project$Main$getFullItemList),
			b4: {
				aG: false,
				aH: 3,
				aI: 2,
				f: false,
				aL: 3,
				aM: 0,
				ak: $author$project$Settings$ghostSettings.b5,
				b: false,
				U: A2($author$project$Types$Ghost$getGhostSrc, 3, 0)
			}
		};
	});
var $author$project$Main$initialModel = A7($author$project$Main$resetGame, 3, 0, _List_Nil, _List_Nil, 0, 1, 2);
var $MartinSStewart$elm_audio$Audio$AudioLoadRequest = function (a) {
	return {$: 0, a: a};
};
var $MartinSStewart$elm_audio$Audio$ErrorThatHappensWhenYouLoadMoreThan1000SoundsDueToHackyWorkAroundToMakeThisPackageBehaveMoreLikeAnEffectPackage = 2;
var $mgold$elm_nonempty_list$List$Nonempty$Nonempty = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $MartinSStewart$elm_audio$Audio$enumeratedResults = A2(
	$mgold$elm_nonempty_list$List$Nonempty$Nonempty,
	$elm$core$Result$Err(2),
	_Utils_ap(
		_List_fromArray(
			[
				$elm$core$Result$Err(0),
				$elm$core$Result$Err(1)
			]),
		A2(
			$elm$core$List$map,
			function (bufferId) {
				return $elm$core$Result$Ok(
					{as: bufferId});
			},
			A2($elm$core$List$range, 0, 1000))));
var $mgold$elm_nonempty_list$List$Nonempty$map = F2(
	function (f, _v0) {
		var x = _v0.a;
		var xs = _v0.b;
		return A2(
			$mgold$elm_nonempty_list$List$Nonempty$Nonempty,
			f(x),
			A2($elm$core$List$map, f, xs));
	});
var $MartinSStewart$elm_audio$Audio$loadAudio = F2(
	function (userMsg, url) {
		return $MartinSStewart$elm_audio$Audio$AudioLoadRequest(
			{
				X: url,
				O: A2(
					$mgold$elm_nonempty_list$List$Nonempty$map,
					function (results) {
						return _Utils_Tuple2(
							results,
							userMsg(results));
					},
					$MartinSStewart$elm_audio$Audio$enumeratedResults)
			});
	});
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple3(
		$author$project$Main$initialModel,
		$elm$core$Platform$Cmd$none,
		A2($MartinSStewart$elm_audio$Audio$loadAudio, $author$project$Types$GameModels$SoundLoaded, 'Assets/sounds/start_music.wav'));
};
var $author$project$Types$GameModels$ChangeColor = {$: 7};
var $author$project$Types$GameModels$ChangePacmanSrc = {$: 8};
var $author$project$Types$GameModels$ClearScoreMsg = {$: 14};
var $author$project$Types$GameModels$EatWaiter = {$: 13};
var $author$project$Types$GameModels$Fruit = {$: 4};
var $author$project$Types$GameModels$GhostMove = function (a) {
	return {$: 6, a: a};
};
var $author$project$Types$GameModels$MoveDirection = function (a) {
	return {$: 0, a: a};
};
var $author$project$Types$GameModels$NewLevel = 0;
var $author$project$Types$GameModels$NormalReset = 1;
var $author$project$Types$GameModels$Pill = {$: 5};
var $author$project$Types$GameModels$ResetGame = function (a) {
	return {$: 9, a: a};
};
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {bI: processes, b_: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 1) {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 0, a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 1, a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.bI;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.b_);
		if (_v0.$ === 1) {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $author$project$Types$GameModels$ChangeDirection = function (a) {
	return {$: 3, a: a};
};
var $author$project$Types$GameModels$ChangeState = {$: 1};
var $author$project$Types$GameModels$DoNothing = {$: 2};
var $author$project$Types$GameModels$Down = 1;
var $author$project$Main$toKey = function (string) {
	switch (string) {
		case 'ArrowUp':
			return $author$project$Types$GameModels$ChangeDirection(0);
		case 'ArrowDown':
			return $author$project$Types$GameModels$ChangeDirection(1);
		case 'ArrowLeft':
			return $author$project$Types$GameModels$ChangeDirection(2);
		case 'ArrowRight':
			return $author$project$Types$GameModels$ChangeDirection(3);
		case 'Escape':
			return $author$project$Types$GameModels$ChangeState;
		default:
			return $author$project$Types$GameModels$DoNothing;
	}
};
var $author$project$Main$keyDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Main$toKey,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$Events$Document = 0;
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {bE: pids, bZ: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (!node) {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {bn: event, bt: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (!node) {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.bE,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.bt;
		var event = _v0.bn;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.bZ);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyDown = A2($elm$browser$Browser$Events$on, 0, 'keydown');
var $author$project$Main$subscriptions = function (game) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$elm$browser$Browser$Events$onKeyDown($author$project$Main$keyDecoder),
				function () {
				var _v0 = game.a;
				switch (_v0.$) {
					case 0:
						var d = _v0.a;
						return A2(
							$elm$time$Time$every,
							20,
							function (_v1) {
								return $author$project$Types$GameModels$MoveDirection(d);
							});
					case 2:
						return A2(
							$elm$time$Time$every,
							20,
							function (_v2) {
								return $author$project$Types$GameModels$ResetGame(1);
							});
					default:
						return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v3 = game.a;
				if (!_v3.$) {
					return game.au ? A2(
						$elm$time$Time$every,
						1000,
						function (_v4) {
							return $author$project$Types$GameModels$Fruit;
						}) : $elm$core$Platform$Sub$none;
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v5 = game.a;
				if (!_v5.$) {
					return game.cH ? A2(
						$elm$time$Time$every,
						1000,
						function (_v6) {
							return $author$project$Types$GameModels$Pill;
						}) : $elm$core$Platform$Sub$none;
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v7 = game.a;
				if (!_v7.$) {
					return (game.cH && (game.cI > 7)) ? A2(
						$elm$time$Time$every,
						250,
						function (_v8) {
							return $author$project$Types$GameModels$ChangeColor;
						}) : $elm$core$Platform$Sub$none;
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				_Utils_eq(game.b$, game.ai) ? A2(
				$elm$time$Time$every,
				20,
				function (_v9) {
					return $author$project$Types$GameModels$ResetGame(0);
				}) : $elm$core$Platform$Sub$none,
				function () {
				var _v10 = game.a;
				if (!_v10.$) {
					return game.aZ ? A2(
						$elm$time$Time$every,
						1000,
						function (_v11) {
							return $author$project$Types$GameModels$EatWaiter;
						}) : $elm$core$Platform$Sub$none;
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v12 = game.a;
				if (!_v12.$) {
					return game.ay ? A2(
						$elm$time$Time$every,
						200,
						function (_v13) {
							return $author$project$Types$GameModels$ChangePacmanSrc;
						}) : $elm$core$Platform$Sub$none;
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v14 = game.a;
				if (!_v14.$) {
					return game.bL.f ? A2(
						$elm$time$Time$every,
						10,
						function (_v15) {
							return $author$project$Types$GameModels$GhostMove(0);
						}) : (game.cH ? A2(
						$elm$time$Time$every,
						30,
						function (_v16) {
							return $author$project$Types$GameModels$GhostMove(0);
						}) : A2(
						$elm$time$Time$every,
						20,
						function (_v17) {
							return $author$project$Types$GameModels$GhostMove(0);
						}));
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v18 = game.a;
				if (!_v18.$) {
					return game.bF.f ? A2(
						$elm$time$Time$every,
						10,
						function (_v19) {
							return $author$project$Types$GameModels$GhostMove(1);
						}) : (game.cH ? A2(
						$elm$time$Time$every,
						30,
						function (_v20) {
							return $author$project$Types$GameModels$GhostMove(1);
						}) : A2(
						$elm$time$Time$every,
						20,
						function (_v21) {
							return $author$project$Types$GameModels$GhostMove(1);
						}));
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v22 = game.a;
				if (!_v22.$) {
					return game.bf.f ? A2(
						$elm$time$Time$every,
						10,
						function (_v23) {
							return $author$project$Types$GameModels$GhostMove(2);
						}) : (game.cH ? A2(
						$elm$time$Time$every,
						30,
						function (_v24) {
							return $author$project$Types$GameModels$GhostMove(2);
						}) : A2(
						$elm$time$Time$every,
						20,
						function (_v25) {
							return $author$project$Types$GameModels$GhostMove(2);
						}));
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v26 = game.a;
				if (!_v26.$) {
					return game.b4.f ? A2(
						$elm$time$Time$every,
						10,
						function (_v27) {
							return $author$project$Types$GameModels$GhostMove(3);
						}) : (game.cH ? A2(
						$elm$time$Time$every,
						30,
						function (_v28) {
							return $author$project$Types$GameModels$GhostMove(3);
						}) : A2(
						$elm$time$Time$every,
						20,
						function (_v29) {
							return $author$project$Types$GameModels$GhostMove(3);
						}));
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}(),
				function () {
				var _v30 = game.a;
				if (!_v30.$) {
					return game.a9 ? A2(
						$elm$time$Time$every,
						1500,
						function (_v31) {
							return $author$project$Types$GameModels$ClearScoreMsg;
						}) : $elm$core$Platform$Sub$none;
				} else {
					return $elm$core$Platform$Sub$none;
				}
			}()
			]));
};
var $author$project$Types$GameModels$GetCurrentTime = F2(
	function (a, b) {
		return {$: 12, a: a, b: b};
	});
var $author$project$Types$GameModels$GoBackInPrison = 6;
var $author$project$Types$GameModels$LoadFailedModel = {$: 2};
var $author$project$Types$GameModels$LoadedModel = function (a) {
	return {$: 1, a: a};
};
var $andrewMacmurray$elm_delay$Delay$Millisecond = 0;
var $author$project$Types$GameModels$Playing = function (a) {
	return {$: 1, a: a};
};
var $author$project$Types$GameModels$Running = function (a) {
	return {$: 0, a: a};
};
var $author$project$Types$GameModels$StartGame = {$: 10};
var $andrewMacmurray$elm_delay$Delay$Duration = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Process$sleep = _Process_sleep;
var $andrewMacmurray$elm_delay$Delay$after_ = F2(
	function (time, msg) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$always(msg),
			$elm$core$Process$sleep(time));
	});
var $andrewMacmurray$elm_delay$Delay$Minute = 2;
var $andrewMacmurray$elm_delay$Delay$Second = 1;
var $andrewMacmurray$elm_delay$Delay$toMillis = function (_v0) {
	var t = _v0.a;
	var u = _v0.b;
	switch (u) {
		case 0:
			return t;
		case 1:
			return 1000 * t;
		case 2:
			return $andrewMacmurray$elm_delay$Delay$toMillis(
				A2($andrewMacmurray$elm_delay$Delay$Duration, 60 * t, 1));
		default:
			return $andrewMacmurray$elm_delay$Delay$toMillis(
				A2($andrewMacmurray$elm_delay$Delay$Duration, 60 * t, 2));
	}
};
var $andrewMacmurray$elm_delay$Delay$after = F3(
	function (time, unit, msg) {
		return A2(
			$andrewMacmurray$elm_delay$Delay$after_,
			$andrewMacmurray$elm_delay$Delay$toMillis(
				A2($andrewMacmurray$elm_delay$Delay$Duration, time, unit)),
			msg);
	});
var $author$project$Types$Ghost$changeGhostSrc = F2(
	function (ghost, color) {
		return {
			aG: ghost.aG,
			aH: color,
			aI: ghost.aI,
			f: ghost.f,
			aL: ghost.aL,
			aM: ghost.aM,
			ak: ghost.ak,
			b: ghost.b,
			U: A2($author$project$Types$Ghost$getGhostSrc, color, ghost.aI)
		};
	});
var $author$project$Types$Ghost$changeGoBackInPrison = F2(
	function (ghost, value) {
		return {aG: ghost.aG, aH: ghost.aH, aI: ghost.aI, f: value, aL: ghost.aL, aM: ghost.aM, ak: ghost.ak, b: ghost.b, U: ghost.U};
	});
var $author$project$Main$changeXPosition = F2(
	function (value, game) {
		var oldPosition = game.aR;
		return _Utils_update(
			oldPosition,
			{t: value});
	});
var $author$project$Main$changeYPosition = F2(
	function (value, game) {
		var oldPosition = game.aR;
		return _Utils_update(
			oldPosition,
			{u: value});
	});
var $elm$core$Basics$ge = _Utils_ge;
var $author$project$Movement$checkPath = F4(
	function (pos, lType, line, e) {
		return ((_Utils_cmp(
			pos.t,
			A2($elm$core$Basics$min, line.aX.t, line.aJ.t)) > -1) && ((_Utils_cmp(
			pos.t,
			A2($elm$core$Basics$max, line.aX.t, line.aJ.t)) < 1) && ((_Utils_cmp(
			pos.u,
			A2($elm$core$Basics$min, line.aX.u, line.aJ.u)) > -1) && ((_Utils_cmp(
			pos.u,
			A2($elm$core$Basics$max, line.aX.u, line.aJ.u)) < 1) && (_Utils_eq(line.bw, lType) || (line.bw === 3)))))) || e;
	});
var $author$project$Settings$movement = 1;
var $author$project$Movement$checkDir = F3(
	function (point, dir, ghost) {
		switch (dir) {
			case 2:
				return A3(
					$elm$core$List$foldl,
					A2(
						F2(
							function (x, y) {
								return A2($author$project$Movement$checkPath, x, y);
							}),
						{t: point.t - $author$project$Settings$movement, u: point.u},
						ghost),
					false,
					$elm$core$Dict$values($author$project$Settings$runMesh));
			case 3:
				return A3(
					$elm$core$List$foldl,
					A2(
						F2(
							function (x, y) {
								return A2($author$project$Movement$checkPath, x, y);
							}),
						{t: point.t + $author$project$Settings$movement, u: point.u},
						ghost),
					false,
					$elm$core$Dict$values($author$project$Settings$runMesh));
			case 0:
				return A3(
					$elm$core$List$foldl,
					A2(
						F2(
							function (x, y) {
								return A2($author$project$Movement$checkPath, x, y);
							}),
						{t: point.t, u: point.u - $author$project$Settings$movement},
						ghost),
					false,
					$elm$core$Dict$values($author$project$Settings$runMesh));
			case 1:
				return A3(
					$elm$core$List$foldl,
					A2(
						F2(
							function (x, y) {
								return A2($author$project$Movement$checkPath, x, y);
							}),
						{t: point.t, u: point.u + $author$project$Settings$movement},
						ghost),
					false,
					$elm$core$Dict$values($author$project$Settings$runMesh));
			default:
				return false;
		}
	});
var $author$project$Types$GameModels$Hunted = 4;
var $author$project$Settings$fruitSettings = {
	cs: 70,
	ct: 170,
	ak: {t: 250, u: 280},
	aT: 20,
	cY: 700,
	cZ: 3000,
	c_: 100,
	c$: 1000,
	c0: 5000,
	c1: 500,
	c2: 2000,
	c3: 300
};
var $author$project$Settings$pillSettings = {bo: '#FFAAA5', cK: 6, aF: 50};
var $author$project$Eatable$setScoreMsg = F2(
	function (msgPoint, msgText) {
		return {cv: msgText, cJ: msgPoint};
	});
var $author$project$Eatable$checkEatable = function (game) {
	var fruitXp = (game.y === 1) ? $author$project$Settings$fruitSettings.c_ : ((game.y === 2) ? $author$project$Settings$fruitSettings.c3 : (((game.y === 3) || (game.y === 4)) ? $author$project$Settings$fruitSettings.c1 : (((game.y === 5) || (game.y === 6)) ? $author$project$Settings$fruitSettings.cY : (((game.y === 7) || (game.y === 8)) ? $author$project$Settings$fruitSettings.c$ : (((game.y === 9) || (game.y === 10)) ? $author$project$Settings$fruitSettings.c2 : (((game.y === 11) || (game.y === 1)) ? $author$project$Settings$fruitSettings.cZ : $author$project$Settings$fruitSettings.c0))))));
	var checkCurrentPoint = function (lp) {
		checkCurrentPoint:
		while (true) {
			if (!lp.b) {
				return _List_Nil;
			} else {
				var x = lp.a;
				var xs = lp.b;
				if (_Utils_eq(x, game.aR)) {
					var $temp$lp = xs;
					lp = $temp$lp;
					continue checkCurrentPoint;
				} else {
					return A2(
						$elm$core$List$cons,
						x,
						checkCurrentPoint(xs));
				}
			}
		}
	};
	var localListItems = checkCurrentPoint(game.aw);
	var localListPills = checkCurrentPoint(game.aS);
	return (_Utils_eq(
		$elm$core$List$length(game.aS),
		$elm$core$List$length(localListPills)) && _Utils_eq(
		$elm$core$List$length(game.aw),
		$elm$core$List$length(localListItems))) ? ((_Utils_eq(game.aR, $author$project$Settings$fruitSettings.ak) && game.au) ? _Utils_update(
		game,
		{
			au: false,
			ay: true,
			M: game.M + fruitXp,
			a7: A2(
				$author$project$Eatable$setScoreMsg,
				$author$project$Settings$fruitSettings.ak,
				$elm$core$String$fromInt(fruitXp)),
			a9: true
		}) : game) : ((!_Utils_eq(
		$elm$core$List$length(game.aS),
		$elm$core$List$length(localListPills))) ? _Utils_update(
		game,
		{
			bf: A2($author$project$Types$Ghost$changeGhostSrc, game.bf, 4),
			ay: true,
			cH: true,
			cI: 0,
			aS: localListPills,
			bF: A2($author$project$Types$Ghost$changeGhostSrc, game.bF, 4),
			bL: A2($author$project$Types$Ghost$changeGhostSrc, game.bL, 4),
			M: game.M + $author$project$Settings$pillSettings.aF,
			a7: A2(
				$author$project$Eatable$setScoreMsg,
				game.aR,
				$elm$core$String$fromInt($author$project$Settings$pillSettings.aF)),
			a9: true,
			b4: A2($author$project$Types$Ghost$changeGhostSrc, game.b4, 4)
		}) : ((_Utils_eq(game.aR, $author$project$Settings$fruitSettings.ak) && game.au) ? _Utils_update(
		game,
		{
			aZ: true,
			a_: $author$project$Settings$itemSettings.a3,
			au: false,
			aw: localListItems,
			ay: true,
			M: (game.M + fruitXp) + $author$project$Settings$itemSettings.aF,
			a7: A2(
				$author$project$Eatable$setScoreMsg,
				$author$project$Settings$fruitSettings.ak,
				$elm$core$String$fromInt(fruitXp)),
			a9: true
		}) : ((_Utils_eq(game.ai, $author$project$Settings$fruitSettings.cs) || _Utils_eq(game.ai, $author$project$Settings$fruitSettings.ct)) ? _Utils_update(
		game,
		{aZ: true, a_: $author$project$Settings$itemSettings.a3, au: true, ai: game.ai + 1, aw: localListItems, ay: true, M: game.M + $author$project$Settings$itemSettings.aF}) : _Utils_update(
		game,
		{aZ: true, a_: $author$project$Settings$itemSettings.a3, ai: game.ai + 1, aw: localListItems, ay: true, M: game.M + $author$project$Settings$itemSettings.aF}))));
};
var $author$project$Types$Ghost$checkGhoastEatingPacMan = F2(
	function (pacPos, ghostPos) {
		return (!_Utils_eq(pacPos, ghostPos)) && ((!_Utils_eq(
			{t: pacPos.t + 1, u: pacPos.u},
			ghostPos)) && ((!_Utils_eq(
			{t: pacPos.t - 1, u: pacPos.u},
			ghostPos)) && ((!_Utils_eq(
			{t: pacPos.t, u: pacPos.u + 1},
			ghostPos)) && (!_Utils_eq(
			{t: pacPos.t, u: pacPos.u - 1},
			ghostPos)))));
	});
var $MartinSStewart$elm_audio$Audio$AudioCmdGroup = function (a) {
	return {$: 1, a: a};
};
var $MartinSStewart$elm_audio$Audio$cmdNone = $MartinSStewart$elm_audio$Audio$AudioCmdGroup(_List_Nil);
var $author$project$Settings$fieldSettings = {d: '#3498DB', co: 500, cW: 500};
var $elm$core$Basics$round = _Basics_round;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $lynn$elm_arithmetic$Arithmetic$intSquareRoot = A2(
	$elm$core$Basics$composeR,
	$elm$core$Basics$toFloat,
	A2($elm$core$Basics$composeR, $elm$core$Basics$sqrt, $elm$core$Basics$round));
var $author$project$Types$Ghost$getVectorLength = F2(
	function (p1, p2) {
		var dv = {t: p1.t - p2.t, u: p1.u - p2.u};
		return $lynn$elm_arithmetic$Arithmetic$intSquareRoot((dv.t * dv.t) + (dv.u * dv.u));
	});
var $author$project$Types$Ghost$getBlueGhoastOffset = F2(
	function (redGhostPos, pacPos) {
		return A2($author$project$Types$Ghost$getVectorLength, redGhostPos, pacPos) * 2;
	});
var $author$project$Types$Line$Ghost = 1;
var $author$project$Types$Ghost$getNextCross = F2(
	function (pos, dir) {
		getNextCross:
		while (true) {
			if ((A3($author$project$Movement$checkDir, pos, dir, 1) && ((!A3($author$project$Movement$checkDir, pos, 3, 1)) && ((!A3($author$project$Movement$checkDir, pos, 0, 1)) && (!A3($author$project$Movement$checkDir, pos, 1, 1))))) || (((!A3($author$project$Movement$checkDir, pos, 2, 1)) && ((!A3($author$project$Movement$checkDir, pos, 3, 1)) && (!A3($author$project$Movement$checkDir, pos, 1, 1)))) || (((!A3($author$project$Movement$checkDir, pos, 2, 1)) && ((!A3($author$project$Movement$checkDir, pos, 0, 1)) && (!A3($author$project$Movement$checkDir, pos, 1, 1)))) || ((!A3($author$project$Movement$checkDir, pos, 2, 1)) && ((!A3($author$project$Movement$checkDir, pos, 3, 1)) && (!A3($author$project$Movement$checkDir, pos, 0, 1))))))) {
				switch (dir) {
					case 2:
						var $temp$pos = {t: pos.t + $author$project$Settings$movement, u: pos.u},
							$temp$dir = dir;
						pos = $temp$pos;
						dir = $temp$dir;
						continue getNextCross;
					case 3:
						var $temp$pos = {t: pos.t - $author$project$Settings$movement, u: pos.u},
							$temp$dir = dir;
						pos = $temp$pos;
						dir = $temp$dir;
						continue getNextCross;
					case 1:
						var $temp$pos = {t: pos.t, u: pos.u + $author$project$Settings$movement},
							$temp$dir = dir;
						pos = $temp$pos;
						dir = $temp$dir;
						continue getNextCross;
					case 0:
						var $temp$pos = {t: pos.t, u: pos.u - $author$project$Settings$movement},
							$temp$dir = dir;
						pos = $temp$pos;
						dir = $temp$dir;
						continue getNextCross;
					default:
						return pos;
				}
			} else {
				return pos;
			}
		}
	});
var $author$project$Types$Ghost$getGhostNextDir = F2(
	function (game, ghost) {
		var currentType = (!ghost.aG) ? 2 : 3;
		var targetPos = function () {
			if (ghost.f) {
				return $author$project$Settings$ghostSettings.N;
			} else {
				if ((ghost.aL === 3) && ((_Utils_cmp(
					A2($author$project$Types$Ghost$getVectorLength, ghost.ak, game.aR),
					8 * $author$project$Settings$pacSettings.aT) > 0) && (currentType !== 2))) {
					return $author$project$Settings$getPoint(44);
				} else {
					if (!ghost.aG) {
						return $author$project$Settings$ghostSettings.N;
					} else {
						var _v0 = game.a;
						if (!_v0.$) {
							var dir = _v0.a;
							switch (dir) {
								case 2:
									var pos = {t: game.aR.t - (ghost.aM * $author$project$Settings$pacSettings.aT), u: game.aR.u};
									return (ghost.aL === 2) ? {
										t: game.aR.t - A2($author$project$Types$Ghost$getBlueGhoastOffset, game.bL.ak, pos),
										u: game.aR.u
									} : pos;
								case 3:
									var pos = {t: game.aR.t + (ghost.aM * $author$project$Settings$pacSettings.aT), u: game.aR.u};
									return (ghost.aL === 2) ? {
										t: game.aR.t + A2($author$project$Types$Ghost$getBlueGhoastOffset, game.bL.ak, pos),
										u: game.aR.u
									} : pos;
								case 1:
									var pos = {t: game.aR.t, u: game.aR.u + (ghost.aM * $author$project$Settings$pacSettings.aT)};
									return (ghost.aL === 2) ? {
										t: game.aR.t,
										u: game.aR.u + A2($author$project$Types$Ghost$getBlueGhoastOffset, game.bL.ak, pos)
									} : pos;
								case 0:
									var pos = {t: game.aR.t, u: game.aR.u - (ghost.aM * $author$project$Settings$pacSettings.aT)};
									return (ghost.aL === 2) ? {
										t: game.aR.t,
										u: game.aR.u - A2($author$project$Types$Ghost$getBlueGhoastOffset, game.bL.ak, pos)
									} : pos;
								default:
									return game.aR;
							}
						} else {
							return game.aR;
						}
					}
				}
			}
		}();
		if ((ghost.aI === 4) || ((((ghost.aI === 2) || (ghost.aI === 3)) && (A3($author$project$Movement$checkDir, ghost.ak, 0, currentType) || A3($author$project$Movement$checkDir, ghost.ak, 1, currentType))) || (((!ghost.aI) || (ghost.aI === 1)) && (A3($author$project$Movement$checkDir, ghost.ak, 2, currentType) || A3($author$project$Movement$checkDir, ghost.ak, 3, currentType))))) {
			var yDif = A2($elm$core$Basics$max, targetPos.u, ghost.ak.u) - A2($elm$core$Basics$min, targetPos.u, ghost.ak.u);
			var xDif = A2($elm$core$Basics$max, targetPos.t, ghost.ak.t) - A2($elm$core$Basics$min, targetPos.t, ghost.ak.t);
			var nextVerticalCross = (_Utils_cmp(
				A2(
					$author$project$Types$Ghost$getVectorLength,
					A2($author$project$Types$Ghost$getNextCross, ghost.ak, 0),
					targetPos),
				A2(
					$author$project$Types$Ghost$getVectorLength,
					A2($author$project$Types$Ghost$getNextCross, ghost.ak, 1),
					targetPos)) > 0) ? 0 : 1;
			var nextHorizontalCross = (_Utils_cmp(
				A2(
					$author$project$Types$Ghost$getVectorLength,
					A2($author$project$Types$Ghost$getNextCross, ghost.ak, 2),
					targetPos),
				A2(
					$author$project$Types$Ghost$getVectorLength,
					A2($author$project$Types$Ghost$getNextCross, ghost.ak, 3),
					targetPos)) > 0) ? 2 : 3;
			var moveOptions = ((_Utils_cmp(targetPos.t, ghost.ak.t) > 0) && (_Utils_cmp(targetPos.u, ghost.ak.u) > 0)) ? {l: 2, m: 0, n: 3, q: 1} : (((_Utils_cmp(targetPos.t, ghost.ak.t) > 0) && (_Utils_cmp(targetPos.u, ghost.ak.u) < 0)) ? {l: 2, m: 1, n: 3, q: 0} : (((_Utils_cmp(targetPos.t, ghost.ak.t) < 0) && (_Utils_cmp(targetPos.u, ghost.ak.u) < 0)) ? {l: 3, m: 1, n: 2, q: 0} : (((_Utils_cmp(targetPos.t, ghost.ak.t) < 0) && (_Utils_cmp(targetPos.u, ghost.ak.u) > 0)) ? {l: 3, m: 0, n: 2, q: 1} : (_Utils_eq(targetPos.t, ghost.ak.t) ? ((_Utils_cmp(targetPos.u, ghost.ak.u) > 0) ? {l: nextHorizontalCross, m: 0, n: nextHorizontalCross, q: 1} : {l: nextHorizontalCross, m: 1, n: nextHorizontalCross, q: 0}) : (_Utils_eq(targetPos.u, ghost.ak.u) ? ((_Utils_cmp(targetPos.t, ghost.ak.t) > 0) ? {l: 2, m: nextVerticalCross, n: 3, q: nextVerticalCross} : {l: 3, m: nextVerticalCross, n: 2, q: nextVerticalCross}) : {l: 4, m: 4, n: 4, q: 4})))));
			if ((A3($author$project$Movement$checkDir, ghost.ak, moveOptions.n, currentType) && (!_Utils_eq(ghost.aI, moveOptions.l))) && (A3($author$project$Movement$checkDir, ghost.ak, moveOptions.q, currentType) && (!_Utils_eq(ghost.aI, moveOptions.m)))) {
				return (_Utils_cmp(xDif, yDif) < 0) ? moveOptions.q : moveOptions.n;
			} else {
				if (A3($author$project$Movement$checkDir, ghost.ak, moveOptions.n, currentType) && (!_Utils_eq(ghost.aI, moveOptions.l))) {
					return moveOptions.n;
				} else {
					if (A3($author$project$Movement$checkDir, ghost.ak, moveOptions.q, currentType) && (!_Utils_eq(ghost.aI, moveOptions.m))) {
						return moveOptions.q;
					} else {
						if ((A3($author$project$Movement$checkDir, ghost.ak, moveOptions.l, currentType) && (!_Utils_eq(ghost.aI, moveOptions.n))) && (A3($author$project$Movement$checkDir, ghost.ak, moveOptions.m, currentType) && (!_Utils_eq(ghost.aI, moveOptions.q)))) {
							var verticalNextCross = A2($author$project$Types$Ghost$getNextCross, ghost.ak, moveOptions.m);
							var horizontalNextCross = A2($author$project$Types$Ghost$getNextCross, ghost.ak, moveOptions.l);
							return (_Utils_cmp(
								A2($author$project$Types$Ghost$getVectorLength, verticalNextCross, targetPos),
								A2($author$project$Types$Ghost$getVectorLength, horizontalNextCross, targetPos)) < 0) ? moveOptions.m : moveOptions.l;
						} else {
							if (A3($author$project$Movement$checkDir, ghost.ak, moveOptions.l, currentType) && (!_Utils_eq(ghost.aI, moveOptions.n))) {
								return moveOptions.l;
							} else {
								if (A3($author$project$Movement$checkDir, ghost.ak, moveOptions.m, currentType) && (!_Utils_eq(ghost.aI, moveOptions.q))) {
									return moveOptions.m;
								} else {
									return 4;
								}
							}
						}
					}
				}
			}
		} else {
			return ghost.aI;
		}
	});
var $author$project$Types$GameModels$White = 5;
var $author$project$Types$Ghost$huntedColorChange = function (ghost) {
	return (ghost.U === 'hunted') ? A2($author$project$Types$Ghost$changeGhostSrc, ghost, 5) : ((ghost.U === 'hunted_white') ? A2($author$project$Types$Ghost$changeGhostSrc, ghost, 4) : ghost);
};
var $author$project$Types$Ghost$moveGhoastToPosition = F2(
	function (ghost, target) {
		return {aG: ghost.aG, aH: ghost.aH, aI: ghost.aI, f: ghost.f, aL: ghost.aL, aM: ghost.aM, ak: target, b: ghost.b, U: ghost.U};
	});
var $author$project$Types$Ghost$moveGhost = F2(
	function (ghost, dir) {
		var ghostNextPos = function () {
			switch (dir) {
				case 2:
					return {t: ghost.ak.t - $author$project$Settings$movement, u: ghost.ak.u};
				case 3:
					return {t: ghost.ak.t + $author$project$Settings$movement, u: ghost.ak.u};
				case 0:
					return {t: ghost.ak.t, u: ghost.ak.u - $author$project$Settings$movement};
				case 1:
					return {t: ghost.ak.t, u: ghost.ak.u + $author$project$Settings$movement};
				default:
					return ghost.ak;
			}
		}();
		var activeState = (!ghost.aG) ? ((!ghost.aG) && _Utils_eq(ghostNextPos, $author$project$Settings$ghostSettings.N)) : ghost.aG;
		return (!ghost.aL) ? {
			aG: activeState,
			aH: ghost.aH,
			aI: dir,
			f: ghost.f,
			aL: ghost.aL,
			aM: ghost.aM,
			ak: ghostNextPos,
			b: ghost.b,
			U: A2($author$project$Types$Ghost$getGhostSrc, ghost.aH, dir)
		} : {
			aG: activeState,
			aH: ghost.aH,
			aI: dir,
			f: ghost.f,
			aL: ghost.aL,
			aM: ghost.aM,
			ak: ghostNextPos,
			b: ghost.b,
			U: A2($author$project$Types$Ghost$getGhostSrc, ghost.aH, dir)
		};
	});
var $author$project$Movement$outOfBounds = function (game) {
	return (game.aR.t < 0) || ((_Utils_cmp(game.aR.t, $author$project$Settings$fieldSettings.cW) > 0) || ((game.aR.u < 0) || (_Utils_cmp(game.aR.u, $author$project$Settings$fieldSettings.co) > 0)));
};
var $author$project$Types$Ghost$setActiveState = F2(
	function (ghost, state) {
		return {aG: state, aH: ghost.aH, aI: ghost.aI, f: ghost.f, aL: ghost.aL, aM: ghost.aM, ak: ghost.ak, b: ghost.b, U: ghost.U};
	});
var $author$project$Types$Ghost$setGhostRunning = function (ghost) {
	return {aG: ghost.aG, aH: ghost.aH, aI: ghost.aI, f: ghost.f, aL: ghost.aL, aM: ghost.aM, ak: ghost.ak, b: true, U: ghost.U};
};
var $author$project$Main$update = F2(
	function (msg, game) {
		update:
		while (true) {
			switch (msg.$) {
				case 0:
					var d = msg.a;
					switch (d) {
						case 2:
							if ($author$project$Movement$outOfBounds(game)) {
								return _Utils_Tuple3(
									_Utils_update(
										game,
										{
											aR: A2($author$project$Main$changeXPosition, $author$project$Settings$fieldSettings.cW, game),
											L: 180,
											a: $author$project$Types$GameModels$Running(d)
										}),
									$elm$core$Platform$Cmd$none,
									$MartinSStewart$elm_audio$Audio$cmdNone);
							} else {
								if (A3($author$project$Movement$checkDir, game.aR, d, 0) || A3($author$project$Movement$checkDir, game.aR, game.e, 0)) {
									return (A3($author$project$Movement$checkDir, game.aR, game.e, 0) && (!_Utils_eq(game.e, d))) ? _Utils_Tuple3(
										_Utils_update(
											game,
											{
												e: 4,
												a: $author$project$Types$GameModels$Running(game.e)
											}),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
										$author$project$Eatable$checkEatable(
											_Utils_update(
												game,
												{
													aR: A2($author$project$Main$changeXPosition, game.aR.t - $author$project$Settings$movement, game),
													L: 180,
													a: $author$project$Types$GameModels$Running(d)
												})),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone);
								} else {
									var $temp$msg = $author$project$Types$GameModels$DoNothing,
										$temp$game = game;
									msg = $temp$msg;
									game = $temp$game;
									continue update;
								}
							}
						case 3:
							if ($author$project$Movement$outOfBounds(game)) {
								return _Utils_Tuple3(
									_Utils_update(
										game,
										{
											aR: A2($author$project$Main$changeXPosition, 0, game),
											L: 0,
											a: $author$project$Types$GameModels$Running(d)
										}),
									$elm$core$Platform$Cmd$none,
									$MartinSStewart$elm_audio$Audio$cmdNone);
							} else {
								if (A3($author$project$Movement$checkDir, game.aR, d, 0) || A3($author$project$Movement$checkDir, game.aR, game.e, 0)) {
									return (A3($author$project$Movement$checkDir, game.aR, game.e, 0) && (!_Utils_eq(game.e, d))) ? _Utils_Tuple3(
										_Utils_update(
											game,
											{
												e: 4,
												a: $author$project$Types$GameModels$Running(game.e)
											}),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
										$author$project$Eatable$checkEatable(
											_Utils_update(
												game,
												{
													aR: A2($author$project$Main$changeXPosition, game.aR.t + $author$project$Settings$movement, game),
													L: 0,
													a: $author$project$Types$GameModels$Running(d)
												})),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone);
								} else {
									var $temp$msg = $author$project$Types$GameModels$DoNothing,
										$temp$game = game;
									msg = $temp$msg;
									game = $temp$game;
									continue update;
								}
							}
						case 0:
							if ($author$project$Movement$outOfBounds(game)) {
								return _Utils_Tuple3(
									_Utils_update(
										game,
										{
											aR: A2($author$project$Main$changeYPosition, $author$project$Settings$fieldSettings.co, game),
											L: -90,
											a: $author$project$Types$GameModels$Running(d)
										}),
									$elm$core$Platform$Cmd$none,
									$MartinSStewart$elm_audio$Audio$cmdNone);
							} else {
								if (A3($author$project$Movement$checkDir, game.aR, d, 0) || A3($author$project$Movement$checkDir, game.aR, game.e, 0)) {
									return (A3($author$project$Movement$checkDir, game.aR, game.e, 0) && (!_Utils_eq(game.e, d))) ? _Utils_Tuple3(
										_Utils_update(
											game,
											{
												e: 4,
												a: $author$project$Types$GameModels$Running(game.e)
											}),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
										$author$project$Eatable$checkEatable(
											_Utils_update(
												game,
												{
													aR: A2($author$project$Main$changeYPosition, game.aR.u - $author$project$Settings$movement, game),
													L: -90,
													a: $author$project$Types$GameModels$Running(d)
												})),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone);
								} else {
									var $temp$msg = $author$project$Types$GameModels$DoNothing,
										$temp$game = game;
									msg = $temp$msg;
									game = $temp$game;
									continue update;
								}
							}
						case 1:
							if ($author$project$Movement$outOfBounds(game)) {
								return _Utils_Tuple3(
									_Utils_update(
										game,
										{
											aR: A2($author$project$Main$changeYPosition, 0, game),
											L: 90,
											a: $author$project$Types$GameModels$Running(d)
										}),
									$elm$core$Platform$Cmd$none,
									$MartinSStewart$elm_audio$Audio$cmdNone);
							} else {
								if (A3($author$project$Movement$checkDir, game.aR, d, 0) || A3($author$project$Movement$checkDir, game.aR, game.e, 0)) {
									return (A3($author$project$Movement$checkDir, game.aR, game.e, 0) && (!_Utils_eq(game.e, d))) ? _Utils_Tuple3(
										_Utils_update(
											game,
											{
												e: 4,
												a: $author$project$Types$GameModels$Running(game.e)
											}),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
										$author$project$Eatable$checkEatable(
											_Utils_update(
												game,
												{
													aR: A2($author$project$Main$changeYPosition, game.aR.u + $author$project$Settings$movement, game),
													L: 90,
													a: $author$project$Types$GameModels$Running(d)
												})),
										$elm$core$Platform$Cmd$none,
										$MartinSStewart$elm_audio$Audio$cmdNone);
								} else {
									var $temp$msg = $author$project$Types$GameModels$DoNothing,
										$temp$game = game;
									msg = $temp$msg;
									game = $temp$game;
									continue update;
								}
							}
						default:
							var $temp$msg = $author$project$Types$GameModels$DoNothing,
								$temp$game = game;
							msg = $temp$msg;
							game = $temp$game;
							continue update;
					}
				case 1:
					var _v2 = game.a;
					switch (_v2.$) {
						case 0:
							var d = _v2.a;
							return _Utils_Tuple3(
								_Utils_update(
									game,
									{
										aj: $author$project$Settings$gameMessages.cG,
										a: $author$project$Types$GameModels$Stopped(d)
									}),
								$elm$core$Platform$Cmd$none,
								A2($MartinSStewart$elm_audio$Audio$loadAudio, $author$project$Types$GameModels$SoundLoaded, 'Assets/sounds/start_music.wav'));
						case 1:
							var d = _v2.a;
							return _Utils_Tuple3(
								_Utils_update(
									game,
									{
										aj: $author$project$Settings$gameMessages.bB,
										a: $author$project$Types$GameModels$Running(d)
									}),
								$elm$core$Platform$Cmd$none,
								$MartinSStewart$elm_audio$Audio$cmdNone);
						default:
							return _Utils_Tuple3(game, $elm$core$Platform$Cmd$none, $MartinSStewart$elm_audio$Audio$cmdNone);
					}
				case 2:
					return _Utils_Tuple3(game, $elm$core$Platform$Cmd$none, $MartinSStewart$elm_audio$Audio$cmdNone);
				case 3:
					var d = msg.a;
					return _Utils_Tuple3(
						_Utils_update(
							game,
							{e: d}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				case 4:
					return (game.av === 10) ? _Utils_Tuple3(
						_Utils_update(
							game,
							{au: false, av: 0}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
						_Utils_update(
							game,
							{av: game.av + 1}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				case 5:
					return (game.cI === 10) ? _Utils_Tuple3(
						_Utils_update(
							game,
							{
								bf: A2($author$project$Types$Ghost$changeGhostSrc, game.bf, 2),
								C: 0,
								cH: false,
								cI: 0,
								bF: A2($author$project$Types$Ghost$changeGhostSrc, game.bF, 1),
								bL: A2($author$project$Types$Ghost$changeGhostSrc, game.bL, 0),
								b4: A2($author$project$Types$Ghost$changeGhostSrc, game.b4, 3)
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
						_Utils_update(
							game,
							{cI: game.cI + 1}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				case 13:
					return (!game.a_) ? (((!game.bf.b) || ((!game.b4.b) || (!game.bF.b))) ? _Utils_Tuple3(
						_Utils_update(
							game,
							{a_: $author$project$Settings$itemSettings.a3, S: true}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(game, $elm$core$Platform$Cmd$none, $MartinSStewart$elm_audio$Audio$cmdNone)) : (_Utils_eq($author$project$Settings$itemSettings.a3 - 1000, game.a_) ? _Utils_Tuple3(
						_Utils_update(
							game,
							{a_: game.a_ - 1000, ay: false, _: $author$project$Settings$pacSettings.aP}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
						_Utils_update(
							game,
							{a_: game.a_ - 1000}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone));
				case 6:
					var ghostName = msg.a;
					var newYellowGhost = function () {
						if (ghostName === 3) {
							return (game.b4.f && (!_Utils_eq(game.b4.ak, $author$project$Settings$ghostSettings.N))) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhost,
									game.b4,
									A2($author$project$Types$Ghost$getGhostNextDir, game, game.b4)),
								6) : ((game.b4.f && _Utils_eq(game.b4.ak, $author$project$Settings$ghostSettings.N)) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhoastToPosition,
									A2(
										$author$project$Types$Ghost$changeGoBackInPrison,
										A2($author$project$Types$Ghost$setActiveState, game.b4, false),
										false),
									$author$project$Settings$ghostSettings.b5),
								3) : A2(
								$author$project$Types$Ghost$moveGhost,
								game.b4,
								A2($author$project$Types$Ghost$getGhostNextDir, game, game.b4)));
						} else {
							return game.b4;
						}
					}();
					var newRedGhost = function () {
						if (!ghostName) {
							return (game.bL.f && (!_Utils_eq(game.bL.ak, $author$project$Settings$ghostSettings.N))) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhost,
									game.bL,
									A2($author$project$Types$Ghost$getGhostNextDir, game, game.bL)),
								6) : ((game.bL.f && _Utils_eq(game.bL.ak, $author$project$Settings$ghostSettings.N)) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhoastToPosition,
									A2(
										$author$project$Types$Ghost$changeGoBackInPrison,
										A2($author$project$Types$Ghost$setActiveState, game.bL, false),
										false),
									$author$project$Settings$ghostSettings.a5),
								0) : A2(
								$author$project$Types$Ghost$moveGhost,
								game.bL,
								A2($author$project$Types$Ghost$getGhostNextDir, game, game.bL)));
						} else {
							return game.bL;
						}
					}();
					var newPinkGhost = function () {
						if (ghostName === 1) {
							return (game.bF.f && (!_Utils_eq(game.bF.ak, $author$project$Settings$ghostSettings.N))) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhost,
									game.bF,
									A2($author$project$Types$Ghost$getGhostNextDir, game, game.bF)),
								6) : ((game.bF.f && _Utils_eq(game.bF.ak, $author$project$Settings$ghostSettings.N)) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhoastToPosition,
									A2(
										$author$project$Types$Ghost$changeGoBackInPrison,
										A2($author$project$Types$Ghost$setActiveState, game.bF, false),
										false),
									$author$project$Settings$ghostSettings.a5),
								1) : A2(
								$author$project$Types$Ghost$moveGhost,
								game.bF,
								A2($author$project$Types$Ghost$getGhostNextDir, game, game.bF)));
						} else {
							return game.bF;
						}
					}();
					var newBlueGhost = function () {
						if (ghostName === 2) {
							return (game.bf.f && (!_Utils_eq(game.bf.ak, $author$project$Settings$ghostSettings.N))) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhost,
									game.bf,
									A2($author$project$Types$Ghost$getGhostNextDir, game, game.bf)),
								6) : ((game.bf.f && _Utils_eq(game.bf.ak, $author$project$Settings$ghostSettings.N)) ? A2(
								$author$project$Types$Ghost$changeGhostSrc,
								A2(
									$author$project$Types$Ghost$moveGhoastToPosition,
									A2(
										$author$project$Types$Ghost$changeGoBackInPrison,
										A2($author$project$Types$Ghost$setActiveState, game.bf, false),
										false),
									$author$project$Settings$ghostSettings.bg),
								2) : A2(
								$author$project$Types$Ghost$moveGhost,
								game.bf,
								A2($author$project$Types$Ghost$getGhostNextDir, game, game.bf)));
						} else {
							return game.bf;
						}
					}();
					if (A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.bL.ak) && (A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.bf.ak) && (A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.b4.ak) && (A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.bF.ak) && (!_Utils_eq(
						game.a,
						$author$project$Types$GameModels$Stopped(4))))))) {
						return (game.y < 3) ? (((((game.ai > 91) && (game.z === 3)) && (!game.b4.b)) || ((((game.ai > 32) && (game.z !== 3)) && (!game.b4.b)) || (game.S && (game.bf.b && (!game.b4.b))))) ? _Utils_Tuple3(
							_Utils_update(
								game,
								{
									bf: newBlueGhost,
									S: false,
									bF: newPinkGhost,
									bL: newRedGhost,
									b4: $author$project$Types$Ghost$setGhostRunning(newYellowGhost)
								}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone) : (((((game.ai > 31) && (game.z === 3)) && (!game.bf.b)) || ((((game.ai > 17) && (game.z !== 3)) && (!game.bf.b)) || (game.S && (game.bF.b && (!game.bf.b))))) ? _Utils_Tuple3(
							_Utils_update(
								game,
								{
									bf: $author$project$Types$Ghost$setGhostRunning(newBlueGhost),
									S: false,
									bF: newPinkGhost,
									bL: newRedGhost
								}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone) : (((((game.ai > 1) && (game.z === 3)) && (!game.bF.b)) || ((((game.ai > 7) && (game.z !== 3)) && (!game.bF.b)) || (game.S && (game.bL.b && (!game.bF.b))))) ? _Utils_Tuple3(
							_Utils_update(
								game,
								{
									S: false,
									bF: $author$project$Types$Ghost$setGhostRunning(newPinkGhost),
									bL: newRedGhost
								}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone) : ((game.bL.b && (game.bf.b && (game.b4.b && game.bF.b))) ? _Utils_Tuple3(
							_Utils_update(
								game,
								{bf: newBlueGhost, bF: newPinkGhost, bL: newRedGhost, b4: newYellowGhost}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone) : ((game.bL.b && (game.bf.b && game.bF.b)) ? _Utils_Tuple3(
							_Utils_update(
								game,
								{bf: newBlueGhost, bF: newPinkGhost, bL: newRedGhost}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone) : ((game.bL.b && game.bF.b) ? _Utils_Tuple3(
							_Utils_update(
								game,
								{bF: newPinkGhost, bL: newRedGhost}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
							_Utils_update(
								game,
								{bL: newRedGhost}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone))))))) : _Utils_Tuple3(
							_Utils_update(
								game,
								{
									bf: $author$project$Types$Ghost$setGhostRunning(newBlueGhost),
									bF: $author$project$Types$Ghost$setGhostRunning(newPinkGhost),
									bL: $author$project$Types$Ghost$setGhostRunning(newRedGhost),
									b4: $author$project$Types$Ghost$setGhostRunning(newYellowGhost)
								}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone);
					} else {
						if (!game.cH) {
							var _v3 = game.a;
							if (!_v3.$) {
								var d = _v3.a;
								return (!game.z) ? _Utils_Tuple3(
									_Utils_update(
										game,
										{
											aj: $author$project$Settings$gameMessages.cn,
											a: $author$project$Types$GameModels$Stopped(d)
										}),
									$elm$core$Platform$Cmd$none,
									$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
									_Utils_update(
										game,
										{
											z: game.z - 1,
											a: $author$project$Types$GameModels$Stopped(d)
										}),
									A3(
										$andrewMacmurray$elm_delay$Delay$after,
										3000,
										0,
										$author$project$Types$GameModels$ResetGame(1)),
									$MartinSStewart$elm_audio$Audio$cmdNone);
							} else {
								return _Utils_Tuple3(game, $elm$core$Platform$Cmd$none, $MartinSStewart$elm_audio$Audio$cmdNone);
							}
						} else {
							var ghostScore = (game.C + 1) * 200;
							var newScore = game.M + ghostScore;
							return ((!A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.bL.ak)) && (!game.bL.f)) ? _Utils_Tuple3(
								_Utils_update(
									game,
									{
										C: game.C + 1,
										bL: A2($author$project$Types$Ghost$changeGoBackInPrison, game.bL, true),
										M: newScore,
										a7: A2(
											$author$project$Eatable$setScoreMsg,
											game.bL.ak,
											$elm$core$String$fromInt(ghostScore)),
										a9: true
									}),
								$elm$core$Platform$Cmd$none,
								$MartinSStewart$elm_audio$Audio$cmdNone) : (((!A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.bf.ak)) && (!game.bf.f)) ? _Utils_Tuple3(
								_Utils_update(
									game,
									{
										bf: A2($author$project$Types$Ghost$changeGoBackInPrison, game.bf, true),
										C: game.C + 1,
										M: newScore,
										a7: A2(
											$author$project$Eatable$setScoreMsg,
											game.bf.ak,
											$elm$core$String$fromInt(ghostScore)),
										a9: true
									}),
								$elm$core$Platform$Cmd$none,
								$MartinSStewart$elm_audio$Audio$cmdNone) : (((!A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.b4.ak)) && (!game.b4.f)) ? _Utils_Tuple3(
								_Utils_update(
									game,
									{
										C: game.C + 1,
										M: newScore,
										a7: A2(
											$author$project$Eatable$setScoreMsg,
											game.b4.ak,
											$elm$core$String$fromInt(ghostScore)),
										a9: true,
										b4: A2($author$project$Types$Ghost$changeGoBackInPrison, game.b4, true)
									}),
								$elm$core$Platform$Cmd$none,
								$MartinSStewart$elm_audio$Audio$cmdNone) : (((!A2($author$project$Types$Ghost$checkGhoastEatingPacMan, game.aR, game.bF.ak)) && (!game.bF.f)) ? _Utils_Tuple3(
								_Utils_update(
									game,
									{
										C: game.C + 1,
										bF: A2($author$project$Types$Ghost$changeGoBackInPrison, game.bF, true),
										M: newScore,
										a7: A2(
											$author$project$Eatable$setScoreMsg,
											game.bF.ak,
											$elm$core$String$fromInt(ghostScore)),
										a9: true
									}),
								$elm$core$Platform$Cmd$none,
								$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(game, $elm$core$Platform$Cmd$none, $MartinSStewart$elm_audio$Audio$cmdNone))));
						}
					}
				case 7:
					return _Utils_Tuple3(
						_Utils_update(
							game,
							{
								bf: $author$project$Types$Ghost$huntedColorChange(game.bf),
								bF: $author$project$Types$Ghost$huntedColorChange(game.bF),
								bL: $author$project$Types$Ghost$huntedColorChange(game.bL),
								b4: $author$project$Types$Ghost$huntedColorChange(game.b4)
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				case 9:
					var mode = msg.a;
					return _Utils_Tuple3(
						A7($author$project$Main$resetGame, game.z, game.M, game.aw, game.aS, game.ai, game.y, mode),
						A3($andrewMacmurray$elm_delay$Delay$after, 4500, 0, $author$project$Types$GameModels$StartGame),
						$MartinSStewart$elm_audio$Audio$cmdNone);
				case 10:
					return _Utils_Tuple3(
						_Utils_update(
							game,
							{
								aj: $author$project$Settings$gameMessages.bB,
								a: $author$project$Types$GameModels$Running(3)
							}),
						$elm$core$Platform$Cmd$none,
						A2($MartinSStewart$elm_audio$Audio$loadAudio, $author$project$Types$GameModels$SoundLoaded, 'Assets/sounds/start_music.wav'));
				case 11:
					var x = msg.a;
					if (!x.$) {
						var sound = x.a;
						return _Utils_Tuple3(
							game,
							A2(
								$elm$core$Task$perform,
								$author$project$Types$GameModels$GetCurrentTime(sound),
								$elm$time$Time$now),
							$MartinSStewart$elm_audio$Audio$cmdNone);
					} else {
						return _Utils_Tuple3(
							_Utils_update(
								game,
								{am: $author$project$Types$GameModels$LoadFailedModel}),
							$elm$core$Platform$Cmd$none,
							$MartinSStewart$elm_audio$Audio$cmdNone);
					}
				case 12:
					var sound = msg.a;
					var posix = msg.b;
					return _Utils_Tuple3(
						_Utils_update(
							game,
							{
								am: $author$project$Types$GameModels$LoadedModel(
									{
										am: sound,
										bW: $author$project$Types$GameModels$Playing(posix)
									})
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				case 8:
					return _Utils_eq(game._, $author$project$Settings$pacSettings.aP) ? _Utils_Tuple3(
						_Utils_update(
							game,
							{ay: false, _: $author$project$Settings$pacSettings.cd}),
						A3($andrewMacmurray$elm_delay$Delay$after, 200, 0, $author$project$Types$GameModels$ChangePacmanSrc),
						$MartinSStewart$elm_audio$Audio$cmdNone) : _Utils_Tuple3(
						_Utils_update(
							game,
							{ay: false, _: $author$project$Settings$pacSettings.aP}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
				default:
					return _Utils_Tuple3(
						_Utils_update(
							game,
							{
								a7: A2(
									$author$project$Eatable$setScoreMsg,
									{t: 0, u: 0},
									''),
								a9: false
							}),
						$elm$core$Platform$Cmd$none,
						$MartinSStewart$elm_audio$Audio$cmdNone);
			}
		}
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Style$pointPopupCss = function (point) {
	return _List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'font-family', 'VT323, monospace'),
			A2($elm$html$Html$Attributes$style, 'font-size', '1em'),
			A2($elm$html$Html$Attributes$style, 'font-weight', 'bold'),
			A2($elm$html$Html$Attributes$style, 'transform', ' translate(-50%, -50%)'),
			A2(
			$elm$html$Html$Attributes$style,
			'left',
			$elm$core$String$fromInt(point.t) + 'px'),
			A2(
			$elm$html$Html$Attributes$style,
			'top',
			$elm$core$String$fromInt(point.u) + 'px'),
			A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
			A2($elm$html$Html$Attributes$style, 'color', '#5DADE2')
		]);
};
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Style$addMsgText = F2(
	function (point, str) {
		return A2(
			$elm$html$Html$div,
			$author$project$Style$pointPopupCss(point),
			_List_fromArray(
				[
					$elm$html$Html$text(str)
				]));
	});
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$image = $elm$svg$Svg$trustedNode('image');
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$Eatable$createFruit = F2(
	function (available, level) {
		var fruit = (level === 1) ? 'cherry' : ((level === 2) ? 'strawberry' : (((level === 3) || (level === 4)) ? 'orange' : (((level === 5) || (level === 6)) ? 'apple' : (((level === 7) || (level === 8)) ? 'grape' : (((level === 9) || (level === 10)) ? 'spaceship' : (((level === 11) || (level === 12)) ? 'bell' : 'key'))))));
		return available ? _List_fromArray(
			[
				A2(
				$elm$svg$Svg$image,
				_List_fromArray(
					[
						$elm$svg$Svg$Attributes$xlinkHref('Assets/img/fruits/' + (fruit + '.svg')),
						$elm$svg$Svg$Attributes$width(
						$elm$core$String$fromInt($author$project$Settings$fruitSettings.aT)),
						$elm$svg$Svg$Attributes$height(
						$elm$core$String$fromInt($author$project$Settings$fruitSettings.aT)),
						$elm$svg$Svg$Attributes$x(
						$elm$core$String$fromInt(
							$author$project$Settings$fruitSettings.ak.t - $elm$core$Basics$round($author$project$Settings$fruitSettings.aT / 2))),
						$elm$svg$Svg$Attributes$y(
						$elm$core$String$fromInt(
							$author$project$Settings$fruitSettings.ak.u - $elm$core$Basics$round($author$project$Settings$fruitSettings.aT / 2)))
					]),
				_List_Nil)
			]) : _List_Nil;
	});
var $elm$svg$Svg$Attributes$d = _VirtualDom_attribute('d');
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$html$Html$Attributes$height = function (n) {
	return A2(
		_VirtualDom_attribute,
		'height',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$html$Html$Attributes$width = function (n) {
	return A2(
		_VirtualDom_attribute,
		'width',
		$elm$core$String$fromInt(n));
};
var $author$project$Main$fruitSvgList = F3(
	function (list, counter, level) {
		fruitSvgList:
		while (true) {
			if (_Utils_cmp(counter, level) < 1) {
				if (counter === 1) {
					var $temp$list = A2(
						$elm$core$List$cons,
						A2(
							$elm$html$Html$img,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$src('Assets/img/fruits/cherry.svg'),
									$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
									$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
								]),
							_List_Nil),
						list),
						$temp$counter = counter + 1,
						$temp$level = level;
					list = $temp$list;
					counter = $temp$counter;
					level = $temp$level;
					continue fruitSvgList;
				} else {
					if (counter === 2) {
						var $temp$list = A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src('Assets/img/fruits/strawberry.svg'),
										$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
										$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
									]),
								_List_Nil),
							list),
							$temp$counter = counter + 1,
							$temp$level = level;
						list = $temp$list;
						counter = $temp$counter;
						level = $temp$level;
						continue fruitSvgList;
					} else {
						if (counter === 3) {
							var $temp$list = A2(
								$elm$core$List$cons,
								A2(
									$elm$html$Html$img,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$src('Assets/img/fruits/orange.svg'),
											$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
											$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
										]),
									_List_Nil),
								list),
								$temp$counter = counter + 2,
								$temp$level = level;
							list = $temp$list;
							counter = $temp$counter;
							level = $temp$level;
							continue fruitSvgList;
						} else {
							if (counter === 5) {
								var $temp$list = A2(
									$elm$core$List$cons,
									A2(
										$elm$html$Html$img,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$src('Assets/img/fruits/apple.svg'),
												$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
												$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
											]),
										_List_Nil),
									list),
									$temp$counter = counter + 2,
									$temp$level = level;
								list = $temp$list;
								counter = $temp$counter;
								level = $temp$level;
								continue fruitSvgList;
							} else {
								if (counter === 7) {
									var $temp$list = A2(
										$elm$core$List$cons,
										A2(
											$elm$html$Html$img,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$src('Assets/img/fruits/grape.svg'),
													$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
													$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
												]),
											_List_Nil),
										list),
										$temp$counter = counter + 2,
										$temp$level = level;
									list = $temp$list;
									counter = $temp$counter;
									level = $temp$level;
									continue fruitSvgList;
								} else {
									if (counter === 9) {
										var $temp$list = A2(
											$elm$core$List$cons,
											A2(
												$elm$html$Html$img,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$src('Assets/img/fruits/spaceship.svg'),
														$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
														$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
													]),
												_List_Nil),
											list),
											$temp$counter = counter + 2,
											$temp$level = level;
										list = $temp$list;
										counter = $temp$counter;
										level = $temp$level;
										continue fruitSvgList;
									} else {
										if (counter === 11) {
											var $temp$list = A2(
												$elm$core$List$cons,
												A2(
													$elm$html$Html$img,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$src('Assets/img/fruits/bell.svg'),
															$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
															$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
														]),
													_List_Nil),
												list),
												$temp$counter = counter + 2,
												$temp$level = level;
											list = $temp$list;
											counter = $temp$counter;
											level = $temp$level;
											continue fruitSvgList;
										} else {
											var $temp$list = A2(
												$elm$core$List$cons,
												A2(
													$elm$html$Html$img,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$src('Assets/img/fruits/key.svg'),
															$elm$html$Html$Attributes$width($author$project$Settings$fruitSettings.aT),
															$elm$html$Html$Attributes$height($author$project$Settings$fruitSettings.aT)
														]),
													_List_Nil),
												list),
												$temp$counter = level + 1,
												$temp$level = level;
											list = $temp$list;
											counter = $temp$counter;
											level = $temp$level;
											continue fruitSvgList;
										}
									}
								}
							}
						}
					}
				}
			} else {
				return list;
			}
		}
	});
var $author$project$Style$gameChildCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'top', '0'),
		A2($elm$html$Html$Attributes$style, 'left', '0'),
		A2(
		$elm$html$Html$Attributes$style,
		'width',
		$elm$core$String$fromInt($author$project$Settings$fieldSettings.cW) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'height',
		$elm$core$String$fromInt($author$project$Settings$fieldSettings.co) + 'px'),
		A2($elm$html$Html$Attributes$style, 'overflow', 'hidden')
	]);
var $author$project$Style$gameCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'position', 'relative'),
		A2(
		$elm$html$Html$Attributes$style,
		'width',
		$elm$core$String$fromInt($author$project$Settings$fieldSettings.cW) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'height',
		$elm$core$String$fromInt($author$project$Settings$fieldSettings.co) + 'px'),
		A2($elm$html$Html$Attributes$style, 'margin', '0em auto'),
		A2($elm$html$Html$Attributes$style, 'border-left', '10px solid #000'),
		A2($elm$html$Html$Attributes$style, 'border-right', '10px solid #000'),
		A2($elm$html$Html$Attributes$style, 'background-color', '#000'),
		A2($elm$html$Html$Attributes$style, 'display', 'block')
	]);
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Style$ghostSvgCss = _List_fromArray(
	[
		$elm$html$Html$Attributes$id('ghost'),
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2(
		$elm$html$Html$Attributes$style,
		'width',
		$elm$core$String$fromInt($author$project$Settings$ghostSettings.aT) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'height',
		$elm$core$String$fromInt($author$project$Settings$ghostSettings.aT) + 'px')
	]);
var $author$project$Style$headlineCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'width', '500px'),
		A2($elm$html$Html$Attributes$style, 'height', 'auto'),
		A2($elm$html$Html$Attributes$style, 'background-color', '#000'),
		A2($elm$html$Html$Attributes$style, 'border', '10px solid #000'),
		A2($elm$html$Html$Attributes$style, 'color', '#fff'),
		A2($elm$html$Html$Attributes$style, 'margin', '0em auto'),
		A2($elm$html$Html$Attributes$style, 'padding', '0em'),
		A2($elm$html$Html$Attributes$style, 'display', 'flex'),
		A2($elm$html$Html$Attributes$style, 'justify-content', 'space-between')
	]);
var $elm$svg$Svg$line = $elm$svg$Svg$trustedNode('line');
var $author$project$Style$messageCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'left', '0px'),
		A2($elm$html$Html$Attributes$style, 'top', '267px'),
		A2($elm$html$Html$Attributes$style, 'position', 'relative'),
		A2($elm$html$Html$Attributes$style, 'color', '#ffcc00')
	]);
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $author$project$Main$pacManSvgList = F2(
	function (list, amount) {
		pacManSvgList:
		while (true) {
			if (amount > 0) {
				var $temp$list = A2(
					$elm$core$List$cons,
					A2(
						$elm$html$Html$img,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$src($author$project$Main$initialModel._),
								$elm$html$Html$Attributes$width($author$project$Settings$pacSettings.aT),
								$elm$html$Html$Attributes$height($author$project$Settings$pacSettings.aT)
							]),
						_List_Nil),
					list),
					$temp$amount = amount - 1;
				list = $temp$list;
				amount = $temp$amount;
				continue pacManSvgList;
			} else {
				return list;
			}
		}
	});
var $author$project$Style$pacmanSvgCss = _List_fromArray(
	[
		$elm$html$Html$Attributes$id('pacman'),
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2(
		$elm$html$Html$Attributes$style,
		'width',
		$elm$core$String$fromInt($author$project$Settings$pacSettings.aT) + 'px'),
		A2(
		$elm$html$Html$Attributes$style,
		'height',
		$elm$core$String$fromInt($author$project$Settings$pacSettings.aT) + 'px')
	]);
var $elm$svg$Svg$path = $elm$svg$Svg$trustedNode('path');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $elm$svg$Svg$rect = $elm$svg$Svg$trustedNode('rect');
var $author$project$Eatable$createItemSvg = F2(
	function (_v0, point) {
		return A2(
			$elm$svg$Svg$rect,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromInt(
						point.t - $elm$core$Basics$round($author$project$Settings$itemSettings.aW / 2))),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromInt(
						point.u - $elm$core$Basics$round($author$project$Settings$itemSettings.aW / 2))),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromInt($author$project$Settings$itemSettings.aW)),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromInt($author$project$Settings$itemSettings.aW)),
					$elm$svg$Svg$Attributes$fill($author$project$Settings$itemSettings.bo)
				]),
			_List_Nil);
	});
var $elm$svg$Svg$circle = $elm$svg$Svg$trustedNode('circle');
var $elm$svg$Svg$Attributes$cx = _VirtualDom_attribute('cx');
var $elm$svg$Svg$Attributes$cy = _VirtualDom_attribute('cy');
var $elm$svg$Svg$Attributes$r = _VirtualDom_attribute('r');
var $author$project$Eatable$createPillSvg = F2(
	function (_v0, point) {
		return A2(
			$elm$svg$Svg$circle,
			_List_fromArray(
				[
					$elm$svg$Svg$Attributes$cx(
					$elm$core$String$fromInt(point.t)),
					$elm$svg$Svg$Attributes$cy(
					$elm$core$String$fromInt(point.u)),
					$elm$svg$Svg$Attributes$r(
					$elm$core$String$fromInt($author$project$Settings$pillSettings.cK)),
					$elm$svg$Svg$Attributes$fill($author$project$Settings$pillSettings.bo)
				]),
			_List_Nil);
	});
var $author$project$Eatable$indexedMap = F2(
	function (func, list) {
		return function (_v0) {
			var v = _v0.a;
			return v;
		}(
			A3(
				$elm$core$List$foldl,
				F2(
					function (x, _v1) {
						var ys = _v1.a;
						var l = _v1.b;
						return _Utils_Tuple2(
							A2(
								$elm$core$List$cons,
								A2(func, l, x),
								ys),
							l + 1);
					}),
				_Utils_Tuple2(_List_Nil, 0),
				list));
	});
var $author$project$Eatable$pointsToSvg = F2(
	function (points, mode) {
		switch (mode) {
			case 1:
				return A2($author$project$Eatable$indexedMap, $author$project$Eatable$createItemSvg, points);
			case 2:
				return A2($author$project$Eatable$indexedMap, $author$project$Eatable$createPillSvg, points);
			default:
				return _List_Nil;
		}
	});
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $author$project$Style$styleContents = '\r\n    @import url(\'https://fonts.googleapis.com/css2?family=VT323&display=swap\');\r\n    body {\r\n        background-color: black;\r\n    }\r\n    .headline {\r\n        font-family: \'VT323\', monospace;\r\n    }\r\n    ';
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $author$project$Style$textCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'text-align', 'center'),
		A2($elm$html$Html$Attributes$style, 'font-family', 'VT323, monospace'),
		A2($elm$html$Html$Attributes$style, 'font-weight', 'bold'),
		A2($elm$html$Html$Attributes$style, 'font-size', '1.5em')
	]);
var $author$project$Style$wrapperCss = _List_fromArray(
	[
		A2($elm$html$Html$Attributes$style, 'width', '99%'),
		A2($elm$html$Html$Attributes$style, 'height', 'auto'),
		A2($elm$html$Html$Attributes$style, 'position', 'absolute'),
		A2($elm$html$Html$Attributes$style, 'top', '50%'),
		A2($elm$html$Html$Attributes$style, 'transform', 'translateY(-50%)')
	]);
var $elm$svg$Svg$Attributes$x1 = _VirtualDom_attribute('x1');
var $elm$svg$Svg$Attributes$x2 = _VirtualDom_attribute('x2');
var $elm$svg$Svg$Attributes$y1 = _VirtualDom_attribute('y1');
var $elm$svg$Svg$Attributes$y2 = _VirtualDom_attribute('y2');
var $author$project$Main$view = function (game) {
	return A3(
		$elm$html$Html$node,
		'main',
		_List_Nil,
		_List_fromArray(
			[
				A3(
				$elm$html$Html$node,
				'style',
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text($author$project$Style$styleContents)
					])),
				A2(
				$elm$html$Html$div,
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('wrapper'),
					$author$project$Style$wrapperCss),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class('headline'),
							$author$project$Style$headlineCss),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_Utils_ap(
									$author$project$Style$textCss,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase')
										])),
								_List_fromArray(
									[
										$elm$html$Html$text(
										'Lvl ' + $elm$core$String$fromInt(game.y))
									])),
								A2(
								$elm$html$Html$div,
								$author$project$Style$textCss,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$core$String$fromInt(game.M))
									])),
								A2(
								$elm$html$Html$div,
								$author$project$Style$textCss,
								_List_fromArray(
									[
										$elm$html$Html$text('500x500')
									]))
							])),
						A2(
						$elm$html$Html$div,
						$author$project$Style$gameCss,
						_List_fromArray(
							[
								A2(
								$elm$svg$Svg$svg,
								_Utils_ap(
									$author$project$Style$gameChildCss,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('gameField')
										])),
								_Utils_ap(
									A2($author$project$Eatable$pointsToSvg, game.aw, 1),
									_Utils_ap(
										A2($author$project$Eatable$pointsToSvg, game.aS, 2),
										_Utils_ap(
											A2($author$project$Eatable$createFruit, game.au, game.y),
											_List_fromArray(
												[
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M94,70.7H43.7c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.3-5,5-5H94c2.8,0,5,2.3,5,5v23.3C99,68.4,96.8,70.7,94,70.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M200.3,70.7h-66c-2.8,0-5-2.3-5-5V42.3c0-2.8,2.2-5,5-5h66c2.8,0,5,2.3,5,5v23.3 C205.3,68.4,203.1,70.7,200.3,70.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M366.1,71.6h-67.5c-3,0-5.3-2.3-5.3-5V43.2c0-2.8,2.5-5,5.3-5h67.5c3,0,5.3,2.3,5.3,5v23.3 C371.5,69.3,369.1,71.6,366.1,71.6z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M94.6,131.6H42.4c-2.8,0-5-2.6-5-5.6v-21.3c0-3.2,2.3-5.6,5-5.6h52.3c2.8,0,5,2.6,5,5.6v21.3 C99.6,129.1,97.5,131.6,94.6,131.6z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M457,70.1h-52.5c-3,0-5.3-2.3-5.3-5V42.7c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5V65 C462.3,67.8,460,70.1,457,70.1z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M199.7,158.3h-35.4c-2.4,0-4.3-2.2-4.3-5v-49.8c0-2.8-2-5-4.3-5h-22.1c-2.4,0-4.3,2.3-4.3,5v112.8 c0,2.8,2,5,4.3,5h22.1c2.4,0,4.3-2.3,4.3-5v-34.7c0-2.8,1.9-5,4.3-5h35.4c2.4,0,4.3-2.3,4.3-5v-8.3 C204,160.6,202.1,158.3,199.7,158.3z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M305.8,98.6H193.2c-2.8,0-5,2.3-5,5v22.7c0,2.8,2.3,5,5,5h36.3c2.8,0,5,2.2,5,5v35.3c0,2.8,2.3,5,5,5h18.3 c2.8,0,5-2.3,5-5v-35.3c0-2.8,2.2-5,5-5h38c2.8,0,5-2.3,5-5v-22.7C310.8,100.8,308.6,98.6,305.8,98.6z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M298.2,175.7h36.4c2.4,0,4.2,2.2,4.2,5v33.7c0,2.8,1.9,5,4.2,5h24.8c2.4,0,4.2-2.3,4.2-5l-0.9-111.9 c0-2.8-1.9-5-4.2-5h-23.8c-2.4,0-4.2,2.3-4.2,5l-0.1,49.9c0,2.8-1.8,5-4.2,5h-36.4c-2.4,0-4.2,2.3-4.2,5v8.3 C294,173.4,295.9,175.7,298.2,175.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M457,131.7h-50.5c-3,0-5.3-2.3-5.3-5v-22.4c0-2.8,2.5-5,5.3-5H457c3,0,5.3,2.3,5.3,5v22.3 C462.3,129.4,460,131.7,457,131.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M155,311.7h-21c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h21c2.8,0,5,2.3,5,5v51.7 C160,309.4,157.8,311.7,155,311.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M307.3,294H193.7c-2.8,0-5,2.3-5,5v7.7c0,2.8,2.3,5,5,5H228c2.8,0,5,2.2,5,5V351c0,2.8,2.3,5,5,5h22.3 c2.8,0,5-2.3,5-5v-34.3c0-2.8,2.2-5,5-5h37c2.8,0,5-2.3,5-5V299C312.3,296.2,310.1,294,307.3,294z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M366,312.7h-22c-2.8,0-5-2.3-5-5V255c0-2.8,2.3-5,5-5h22c2.8,0,5,2.3,6,5v52.7   C371,310.4,368.8,312.7,366,312.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M94,340.3H81.3h-1H43c-2.8,0-5,2.2-5,5v7.3c0,2.8,2.3,5,5,5h21.3c2.8,0,5,2.2,5,5V411c0,2.8,2.3,5,5,5H94 c2.8,0,5-2.3,5-5v-65.7C99,342.6,96.8,340.3,94,340.3z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M202.3,356.7h-69c-2.8,0-5-2.3-5-5v-8.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v8.3   C207.3,354.4,205.1,356.7,202.3,356.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M368,355.7h-69c-2.8,0-5-2.3-5-5v-7.3c0-2.8,2.2-5,5-5h69c2.8,0,5,2.3,5,5v7.3   C373,353.4,370.8,355.7,368,355.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M404.3,338.3h4.7h5h42.3c2.8,0,5,2.2,5,5v7.3c0,2.8-2.3,5-5,5H435c-2.8,0-5,2.2-5,5V412c0,2.8-2.3,5-5,5h-20.7 c-2.8,0-5-2.3-5-5v-68.7C399.3,340.6,401.6,338.3,404.3,338.3z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M202,442.7h-37c-2.8,0-5-2.2-5-5v-50.3c0-2.8-2.3-5-5-5h-20.9c-2.8,0-5,2.3-5,5v50.3c0,2.8-2.2,5-5,5H44 c-2.8,0-5,2.2-5,5v9.3c0,2.8,2.3,5,5,5H202c2.8,0,5-2.3,5-5v-9.3C207,444.9,204.8,442.7,202,442.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M298.9,443.7h35.5c2.7,0,4.9-2.2,4.9-5v-52.3c0-2.8,2.3-5,4.9-5H366c2.7,0,4.9,2.3,4.9,5v52.3 c0,2.8,2.2,5,4.9,5h81.5c2.7,0,4.9,2.2,4.9,5v8.3c0,2.8-2.3,5-4.9,5H298.9c-2.7,0-4.9-2.3-4.9-5v-8.3 C294,445.9,296.3,443.7,298.9,443.7z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M304.3,383.3H195.7c-2.8,0-5,2.3-5,5V411c0,2.8,2.3,5,5,5H229c2.8,0,5,3.2,5,6v35.3c0,2.8,2.3,5,5,5h21.3 c2.8,0,5-2.3,5-5V422c0-2.8,2.2-6,5-6h34c2.8,0,5-2.3,5-5v-22.7C309.3,385.6,307.1,383.3,304.3,383.3z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$line,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$x1('234.5'),
															$elm$svg$Svg$Attributes$y1('205.5'),
															$elm$svg$Svg$Attributes$x2('263.5'),
															$elm$svg$Svg$Attributes$y2('205.5'),
															$elm$svg$Svg$Attributes$stroke('#FFBA00'),
															$elm$svg$Svg$Attributes$strokeWidth('2px')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M403.7,211.2v-43.8c0-2.9,2.2-5.1,5-5.1h84.9c1.5,0,2.7-1.2,2.7-2.7V7.9c0-1.5-1.2-2.7-2.7-2.7H6.8 c-1.5,0-2.7,1.2-2.7,2.7v152.2c0,1.5,1.2,2.7,2.7,2.7h82.8c2.8,0,5,2.2,5,5.1v41.8c0,2.9-2.2,6.1-5,6.1H0v5.5h97.3 c1.5,0,2.7-1.2,2.7-2.7v-58.3c0-1.5-1.2-2.7-2.7-2.7H16.5c-2.8,0-5-2.2-5-5.1V15.9c0-2.9,2.2-5.1,5-5.1h211c2.8,0,5,2.2,5,5.1v51 c0,2.9,2.2,5.1,5,5.1H261c2.8,0,5-2.2,5-5.1v-51c0-2.9,2.2-5.1,5-5.1h212.9c2.8,0,5,2.2,5,5.1v136c0,2.9-2.2,5.1-5,5.1H401 c-1.5,0-2.7,1.2-2.7,2.7v59.1c0,0.1,0,0.1,0,0.2c0,0.1,0,0.1,0,0.2c0,1.5,1.2,2.7,2.7,2.7h99v-5.5h-91.3 C405.9,216.3,403.7,214.1,403.7,211.2z')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$polygon,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$points('306.9,203.5 263.5,203.5 263.5,208.3 306.9,208.3 306.9,261.9 192.4,261.9 192.4,208.3 234.5,208.3 234.5,203.5 192.4,203.5 188.4,203.5 188.4,208.3 188.4,261.9 188.4,266.7 192.4,266.7 306.9,266.7 311,266.7 311,261.9 311,208.3 311,203.5')
														]),
													_List_Nil),
													A2(
													$elm$svg$Svg$path,
													_List_fromArray(
														[
															$elm$svg$Svg$Attributes$fill($author$project$Settings$fieldSettings.d),
															$elm$svg$Svg$Attributes$d('M406.4,254.4H500V249h-99.5c-1.2,0-2.2,1.2-2.2,2.7l0,0l0,0v56.3c0,1.5,1.2,2.7,2.7,2.7h85.4 c1.5,0,2.7,1.2,2.7,2.7v65.9c0,2.9-2.2,5.1-5,5.1h-19.9c-2.8,0-5,2.2-5,5.1v20.8c0,2.9,2.2,5.1,5,5.1h19.9c2.8,0,5,2.2,5,5.1v66.4 c0,1.5-1.2,2.7-2.7,2.7H14.1c-1.5,0-2.7-1.2-2.7-2.7v-66.5c0-2.9,2.2-4.1,5-4.1h20.1c2.8,0,5-2.2,5-5.1v-21.8c0-2.9-2.2-5.1-5-5.1 H16.4c-2.8,0-5-2.2-5-5.1V314c0-1.5,1.2-2.7,2.7-2.7h83.2c0.1,0,0.2,0,0.3,0c0.1,0,0.2,0,0.3,0c1.5,0,2.7-1.2,2.7-2.7v-57 c0-1.5-1.2-2.7-2.7-2.7H0v5.5h92.6c1.5,0,2.7,1.2,2.7,2.7v46c0,1.5-1.2,2.7-2.7,2.7H6.7c-1.5,0-2.7,1.2-2.7,2.7v183.9 c0,1.5,1.2,2.7,2.7,2.7h487.1c1.5,0,2.7-1.2,2.7-2.7V308c0-1.5-1.2-2.7-2.7-2.7h-87.4c-1.5,0-2.7-1.2-2.7-2.7V257 C403.7,255.6,404.9,254.4,406.4,254.4z')
														]),
													_List_Nil)
												]))))),
								A2(
								$elm$html$Html$div,
								_Utils_ap(
									$author$project$Style$gameChildCss,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('pacmanArea')
										])),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$img,
										_Utils_ap(
											$author$project$Style$pacmanSvgCss,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$src(game._),
													A2(
													$elm$html$Html$Attributes$style,
													'top',
													$elm$core$String$fromInt(
														game.aR.u - $elm$core$Basics$round($author$project$Settings$pacSettings.aT / 2)) + 'px'),
													A2(
													$elm$html$Html$Attributes$style,
													'left',
													$elm$core$String$fromInt(
														game.aR.t - $elm$core$Basics$round($author$project$Settings$pacSettings.aT / 2)) + 'px'),
													A2(
													$elm$html$Html$Attributes$style,
													'transform',
													'rotate(' + ($elm$core$String$fromInt(game.L) + 'deg)'))
												])),
										_List_Nil)
									])),
								A2(
								$elm$html$Html$div,
								_Utils_ap(
									$author$project$Style$gameChildCss,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('ghostArea')
										])),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$img,
										_Utils_ap(
											$author$project$Style$ghostSvgCss,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$src('Assets/img/ghosts/' + (game.bL.U + '.svg')),
													A2(
													$elm$html$Html$Attributes$style,
													'top',
													$elm$core$String$fromInt(
														game.bL.ak.u - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px'),
													A2(
													$elm$html$Html$Attributes$style,
													'left',
													$elm$core$String$fromInt(
														game.bL.ak.t - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px')
												])),
										_List_Nil),
										A2(
										$elm$html$Html$img,
										_Utils_ap(
											$author$project$Style$ghostSvgCss,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$src('Assets/img/ghosts/' + (game.bF.U + '.svg')),
													A2(
													$elm$html$Html$Attributes$style,
													'top',
													$elm$core$String$fromInt(
														game.bF.ak.u - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px'),
													A2(
													$elm$html$Html$Attributes$style,
													'left',
													$elm$core$String$fromInt(
														game.bF.ak.t - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px')
												])),
										_List_Nil),
										A2(
										$elm$html$Html$img,
										_Utils_ap(
											$author$project$Style$ghostSvgCss,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$src('Assets/img/ghosts/' + (game.bf.U + '.svg')),
													A2(
													$elm$html$Html$Attributes$style,
													'top',
													$elm$core$String$fromInt(
														game.bf.ak.u - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px'),
													A2(
													$elm$html$Html$Attributes$style,
													'left',
													$elm$core$String$fromInt(
														game.bf.ak.t - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px')
												])),
										_List_Nil),
										A2(
										$elm$html$Html$img,
										_Utils_ap(
											$author$project$Style$ghostSvgCss,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$src('Assets/img/ghosts/' + (game.b4.U + '.svg')),
													A2(
													$elm$html$Html$Attributes$style,
													'top',
													$elm$core$String$fromInt(
														game.b4.ak.u - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px'),
													A2(
													$elm$html$Html$Attributes$style,
													'left',
													$elm$core$String$fromInt(
														game.b4.ak.t - $elm$core$Basics$round($author$project$Settings$ghostSettings.aT / 2)) + 'px')
												])),
										_List_Nil)
									])),
								A2(
								$elm$html$Html$div,
								_Utils_ap(
									$author$project$Style$gameChildCss,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$id('otherArea')
										])),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$div,
										_Utils_ap($author$project$Style$textCss, $author$project$Style$messageCss),
										_List_fromArray(
											[
												$elm$html$Html$text(game.aj)
											])),
										A2($author$project$Style$addMsgText, game.a7.cJ, game.a7.cv)
									]))
							])),
						A2(
						$elm$html$Html$div,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class('headline'),
							$author$project$Style$headlineCss),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_Utils_ap(
									$author$project$Style$textCss,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase')
										])),
								_List_fromArray(
									[
										$elm$html$Html$text('Leben:')
									])),
								A2(
								$elm$html$Html$div,
								$author$project$Style$textCss,
								A2($author$project$Main$pacManSvgList, _List_Nil, game.z)),
								A2(
								$elm$html$Html$div,
								_Utils_ap(
									$author$project$Style$textCss,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'text-transform', 'uppercase')
										])),
								_List_fromArray(
									[
										$elm$html$Html$text('Früchte:')
									])),
								A2(
								$elm$html$Html$div,
								$author$project$Style$textCss,
								A3($author$project$Main$fruitSvgList, _List_Nil, 1, game.y))
							]))
					]))
			]));
};
var $author$project$Main$main = $MartinSStewart$elm_audio$Audio$elementWithAudio(
	{
		b8: $author$project$Main$gameToAudio,
		b9: {cm: $author$project$Main$audioPortFromJS, cT: $author$project$Main$audioPortToJS},
		cr: $author$project$Main$init,
		cQ: $author$project$Main$subscriptions,
		cU: $author$project$Main$update,
		cV: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));