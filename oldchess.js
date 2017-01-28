// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.target.offsetLeft || 0),
	    posy - (e.target.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}

function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=function(_1,_2,_){var _3=A(_1,[_]);return A(_2,[_]);},_4=function(_5,_6,_){return _0(_5,_6,_);},_7=function(_8,_9,_){var _a=A(_8,[_]);return A(_9,[_a,_]);},_b=unCStr("GHC.IO.Exception"),_c=unCStr("base"),_d=unCStr("IOException"),_e=[0],_f=new T(function(){var _g=hs_wordToWord64(4053623282),_h=hs_wordToWord64(3693590983);return [0,_g,_h,[0,_g,_h,_c,_b,_d],_e];}),_i=function(_j){return E(_f);},_k=function(_l){return E(E(_l)[1]);},_m=unCStr("Maybe.fromJust: Nothing"),_n=new T(function(){return err(_m);}),_o=function(_p,_q,_r){var _s=new T(function(){var _t=A(_p,[_r]),_u=A(_q,[new T(function(){var _v=E(_s);return _v[0]==0?E(_n):E(_v[1]);})]),_w=hs_eqWord64(_t[1],_u[1]);if(!E(_w)){return [0];}else{var _x=hs_eqWord64(_t[2],_u[2]);return E(_x)==0?[0]:[1,_r];}});return E(_s);},_y=function(_z){var _A=E(_z);return _o(_k(_A[1]),_i,_A[2]);},_B=unCStr(": "),_C=[0,41],_D=unCStr(" ("),_E=function(_F,_G){var _H=E(_F);return _H[0]==0?E(_G):[1,_H[1],new T(function(){return _E(_H[2],_G);})];},_I=unCStr("already exists"),_J=unCStr("does not exist"),_K=unCStr("protocol error"),_L=unCStr("failed"),_M=unCStr("invalid argument"),_N=unCStr("inappropriate type"),_O=unCStr("hardware fault"),_P=unCStr("unsupported operation"),_Q=unCStr("timeout"),_R=unCStr("resource vanished"),_S=unCStr("interrupted"),_T=unCStr("resource busy"),_U=unCStr("resource exhausted"),_V=unCStr("end of file"),_W=unCStr("illegal operation"),_X=unCStr("permission denied"),_Y=unCStr("user error"),_Z=unCStr("unsatisified constraints"),_10=unCStr("system error"),_11=function(_12,_13){switch(E(_12)){case 0:return _E(_I,_13);case 1:return _E(_J,_13);case 2:return _E(_T,_13);case 3:return _E(_U,_13);case 4:return _E(_V,_13);case 5:return _E(_W,_13);case 6:return _E(_X,_13);case 7:return _E(_Y,_13);case 8:return _E(_Z,_13);case 9:return _E(_10,_13);case 10:return _E(_K,_13);case 11:return _E(_L,_13);case 12:return _E(_M,_13);case 13:return _E(_N,_13);case 14:return _E(_O,_13);case 15:return _E(_P,_13);case 16:return _E(_Q,_13);case 17:return _E(_R,_13);default:return _E(_S,_13);}},_14=[0,125],_15=unCStr("{handle: "),_16=function(_17,_18,_19,_1a,_1b,_1c){var _1d=new T(function(){var _1e=new T(function(){return _11(_18,new T(function(){var _1f=E(_1a);return _1f[0]==0?E(_1c):_E(_D,new T(function(){return _E(_1f,[1,_C,_1c]);}));}));}),_1g=E(_19);return _1g[0]==0?E(_1e):_E(_1g,new T(function(){return _E(_B,_1e);}));}),_1h=E(_1b);if(!_1h[0]){var _1i=E(_17);if(!_1i[0]){return E(_1d);}else{var _1j=E(_1i[1]);return _1j[0]==0?_E(_15,new T(function(){return _E(_1j[1],[1,_14,new T(function(){return _E(_B,_1d);})]);})):_E(_15,new T(function(){return _E(_1j[1],[1,_14,new T(function(){return _E(_B,_1d);})]);}));}}else{return _E(_1h[1],new T(function(){return _E(_B,_1d);}));}},_1k=function(_1l){var _1m=E(_1l);return _16(_1m[1],_1m[2],_1m[3],_1m[4],_1m[6],_e);},_1n=function(_1o,_1p){var _1q=E(_1o);return _16(_1q[1],_1q[2],_1q[3],_1q[4],_1q[6],_1p);},_1r=[0,44],_1s=[0,93],_1t=[0,91],_1u=function(_1v,_1w,_1x){var _1y=E(_1w);return _1y[0]==0?unAppCStr("[]",_1x):[1,_1t,new T(function(){return A(_1v,[_1y[1],new T(function(){var _1z=function(_1A){var _1B=E(_1A);return _1B[0]==0?E([1,_1s,_1x]):[1,_1r,new T(function(){return A(_1v,[_1B[1],new T(function(){return _1z(_1B[2]);})]);})];};return _1z(_1y[2]);})]);})];},_1C=function(_1D,_1E){return _1u(_1n,_1D,_1E);},_1F=function(_1G,_1H,_1I){var _1J=E(_1H);return _16(_1J[1],_1J[2],_1J[3],_1J[4],_1J[6],_1I);},_1K=[0,_1F,_1k,_1C],_1L=new T(function(){return [0,_i,_1K,_1M,_y];}),_1M=function(_1N){return [0,_1L,_1N];},_1O=[0],_1P=7,_1Q=function(_1R){return [0,_1O,_1P,_e,_1R,_1O,_1O];},_1S=function(_1T,_){return die(new T(function(){return _1M(new T(function(){return _1Q(_1T);}));}));},_1U=function(_1V,_){return _1S(_1V,_);},_1W=function(_1X,_){return _1X;},_1Y=[0,_7,_4,_1W,_1U],_1Z=function(_20,_21){return A(_20,[function(_){return jsFind(toJSStr(E(_21)));}]);},_22=function(_23,_24){while(1){var _25=E(_24);if(!_25[0]){return false;}else{if(!A(_23,[_25[1]])){_24=_25[2];continue;}else{return true;}}}},_26=function(_27){var _28=E(_27);return _28[0]==0?E(_n):E(_28[1]);},_29=function(_2a){return E(_2a)[0]==0?true:false;},_2b=function(_2c,_2d){var _2e=E(_2d);return _2e[0]==0?[0]:[1,new T(function(){return A(_2c,[_2e[1]]);}),new T(function(){return _2b(_2c,_2e[2]);})];},_2f=[0,34],_2g=function(_2h,_2i){while(1){var _2j=(function(_2k,_2l){var _2m=E(_2k);if(!_2m[0]){return [0];}else{var _2n=_2m[2],_2o=E(_2l);if(!_2o[0]){return [0];}else{var _2p=_2o[2];if(!E(_2o[1])[0]){return [1,_2m[1],new T(function(){return _2g(_2n,_2p);})];}else{_2h=_2n;_2i=_2p;return null;}}}})(_2h,_2i);if(_2j!=null){return _2j;}}},_2q=new T(function(){return unAppCStr("[]",_e);}),_2r=unCStr("Prelude.(!!): negative index\n"),_2s=new T(function(){return err(_2r);}),_2t=unCStr("Prelude.(!!): index too large\n"),_2u=new T(function(){return err(_2t);}),_2v=function(_2w,_2x){while(1){var _2y=E(_2w);if(!_2y[0]){return E(_2u);}else{var _2z=E(_2x);if(!_2z){return E(_2y[1]);}else{_2w=_2y[2];_2x=_2z-1|0;continue;}}}},_2A=unCStr("ACK"),_2B=unCStr("BEL"),_2C=unCStr("BS"),_2D=unCStr("SP"),_2E=[1,_2D,_e],_2F=unCStr("US"),_2G=[1,_2F,_2E],_2H=unCStr("RS"),_2I=[1,_2H,_2G],_2J=unCStr("GS"),_2K=[1,_2J,_2I],_2L=unCStr("FS"),_2M=[1,_2L,_2K],_2N=unCStr("ESC"),_2O=[1,_2N,_2M],_2P=unCStr("SUB"),_2Q=[1,_2P,_2O],_2R=unCStr("EM"),_2S=[1,_2R,_2Q],_2T=unCStr("CAN"),_2U=[1,_2T,_2S],_2V=unCStr("ETB"),_2W=[1,_2V,_2U],_2X=unCStr("SYN"),_2Y=[1,_2X,_2W],_2Z=unCStr("NAK"),_30=[1,_2Z,_2Y],_31=unCStr("DC4"),_32=[1,_31,_30],_33=unCStr("DC3"),_34=[1,_33,_32],_35=unCStr("DC2"),_36=[1,_35,_34],_37=unCStr("DC1"),_38=[1,_37,_36],_39=unCStr("DLE"),_3a=[1,_39,_38],_3b=unCStr("SI"),_3c=[1,_3b,_3a],_3d=unCStr("SO"),_3e=[1,_3d,_3c],_3f=unCStr("CR"),_3g=[1,_3f,_3e],_3h=unCStr("FF"),_3i=[1,_3h,_3g],_3j=unCStr("VT"),_3k=[1,_3j,_3i],_3l=unCStr("LF"),_3m=[1,_3l,_3k],_3n=unCStr("HT"),_3o=[1,_3n,_3m],_3p=[1,_2C,_3o],_3q=[1,_2B,_3p],_3r=[1,_2A,_3q],_3s=unCStr("ENQ"),_3t=[1,_3s,_3r],_3u=unCStr("EOT"),_3v=[1,_3u,_3t],_3w=unCStr("ETX"),_3x=[1,_3w,_3v],_3y=unCStr("STX"),_3z=[1,_3y,_3x],_3A=unCStr("SOH"),_3B=[1,_3A,_3z],_3C=unCStr("NUL"),_3D=[1,_3C,_3B],_3E=[0,92],_3F=unCStr("\\DEL"),_3G=unCStr("\\a"),_3H=unCStr("\\\\"),_3I=unCStr("\\SO"),_3J=unCStr("\\r"),_3K=unCStr("\\f"),_3L=unCStr("\\v"),_3M=unCStr("\\n"),_3N=unCStr("\\t"),_3O=unCStr("\\b"),_3P=function(_3Q,_3R){if(_3Q<=127){var _3S=E(_3Q);switch(_3S){case 92:return _E(_3H,_3R);case 127:return _E(_3F,_3R);default:if(_3S<32){var _3T=E(_3S);switch(_3T){case 7:return _E(_3G,_3R);case 8:return _E(_3O,_3R);case 9:return _E(_3N,_3R);case 10:return _E(_3M,_3R);case 11:return _E(_3L,_3R);case 12:return _E(_3K,_3R);case 13:return _E(_3J,_3R);case 14:return _E(_3I,new T(function(){var _3U=E(_3R);return _3U[0]==0?[0]:E(E(_3U[1])[1])==72?unAppCStr("\\&",_3U):E(_3U);}));default:return _E([1,_3E,new T(function(){var _3V=_3T;return _3V>=0?_2v(_3D,_3V):E(_2s);})],_3R);}}else{return [1,[0,_3S],_3R];}}}else{return [1,_3E,new T(function(){var _3W=jsShowI(_3Q);return _E(fromJSStr(_3W),new T(function(){var _3X=E(_3R);if(!_3X[0]){return [0];}else{var _3Y=E(_3X[1])[1];return _3Y<48?E(_3X):_3Y>57?E(_3X):unAppCStr("\\&",_3X);}}));})];}},_3Z=unCStr("\\\""),_40=function(_41,_42){var _43=E(_41);if(!_43[0]){return E(_42);}else{var _44=_43[2],_45=E(E(_43[1])[1]);return _45==34?_E(_3Z,new T(function(){return _40(_44,_42);})):_3P(_45,new T(function(){return _40(_44,_42);}));}},_46=[1,_1s,_e],_47=function(_48){var _49=E(_48);return _49[0]==0?E(_46):[1,_1r,[1,_2f,new T(function(){return _40(_49[1],[1,_2f,new T(function(){return _47(_49[2]);})]);})]];},_4a=function(_4b,_4c){return err(unAppCStr("Elements with the following IDs could not be found: ",new T(function(){var _4d=_2g(_4c,_4b);return _4d[0]==0?E(_2q):[1,_1t,[1,_2f,new T(function(){return _40(_4d[1],[1,_2f,new T(function(){return _47(_4d[2]);})]);})]];})));},_4e=function(_4f,_4g,_4h,_4i){var _4j=E(_4f),_4k=_4j[1],_4l=_4j[3];return A(_4k,[new T(function(){var _4m=new T(function(){return A(_4l,[_e]);}),_4n=function(_4o){var _4p=E(_4o);if(!_4p[0]){return E(_4m);}else{var _4q=new T(function(){return _4n(_4p[2]);});return A(_4k,[new T(function(){return _1Z(_4g,_4p[1]);}),function(_4r){return A(_4k,[_4q,function(_4s){return A(_4l,[[1,_4r,_4s]]);}]);}]);}};return _4n(_4h);}),function(_4t){return !_22(_29,_4t)?A(_4i,[new T(function(){return _2b(_26,_4t);})]):_4a(_4t,_4h);}]);},_4u=function(_4v){return E(_4v);},_4w=function(_4x){return E(E(_4x)[1]);},_4y=function(_4z,_4A,_4B,_4C,_4D,_4E){return !A(_4z,[_4B,_4D])?true:!A(_4w,[_4A,_4C,_4E])?true:false;},_4F=function(_4G,_4H,_4I,_4J){var _4K=E(_4I),_4L=E(_4J);return _4y(E(_4G)[1],_4H,_4K[1],_4K[2],_4L[1],_4L[2]);},_4M=function(_4N,_4O,_4P,_4Q,_4R,_4S){return !A(_4N,[_4P,_4R])?false:A(_4w,[_4O,_4Q,_4S]);},_4T=function(_4U,_4V,_4W,_4X){var _4Y=E(_4W),_4Z=E(_4X);return _4M(E(_4U)[1],_4V,_4Y[1],_4Y[2],_4Z[1],_4Z[2]);},_50=function(_51,_52){return [0,function(_53,_54){return _4T(_51,_52,_53,_54);},function(_53,_54){return _4F(_51,_52,_53,_54);}];},_55=function(_56,_57){return E(_56)[1]==E(_57)[1];},_58=function(_59,_5a){return E(_59)[1]!=E(_5a)[1];},_5b=[0,_55,_58],_5c=new T(function(){return _50(_5b,_5b);}),_5d=new T(function(){return _50(_5c,_5c);}),_5e=function(_5f,_5g,_5h){while(1){var _5i=E(_5g);if(!_5i[0]){return E(_5h)[0]==0?true:false;}else{var _5j=E(_5h);if(!_5j[0]){return false;}else{if(!A(_4w,[_5f,_5i[1],_5j[1]])){return false;}else{_5g=_5i[2];_5h=_5j[2];continue;}}}}},_5k=function(_5l,_5m){return [0,function(_){var _5n=A(_5l,[_]);return new T(function(){return A(_5m,[_5n]);});}];},_5o=unCStr("Knight"),_5p=unCStr("Pawn"),_5q=unCStr("King"),_5r=unCStr("Queen"),_5s=unCStr("Rook"),_5t=unCStr("Bishop"),_5u=function(_5v){switch(E(_5v)){case 0:return E(_5p);case 1:return E(_5o);case 2:return E(_5t);case 3:return E(_5s);case 4:return E(_5r);default:return E(_5q);}},_5w=unCStr("Black"),_5x=unCStr("White"),_5y=unCStr("ArithException"),_5z=unCStr("GHC.Exception"),_5A=unCStr("base"),_5B=new T(function(){var _5C=hs_wordToWord64(4194982440),_5D=hs_wordToWord64(3110813675);return [0,_5C,_5D,[0,_5C,_5D,_5A,_5z,_5y],_e];}),_5E=function(_5F){return E(_5B);},_5G=function(_5H){var _5I=E(_5H);return _o(_k(_5I[1]),_5E,_5I[2]);},_5J=unCStr("arithmetic underflow"),_5K=unCStr("arithmetic overflow"),_5L=unCStr("Ratio has zero denominator"),_5M=unCStr("denormal"),_5N=unCStr("divide by zero"),_5O=unCStr("loss of precision"),_5P=function(_5Q){switch(E(_5Q)){case 0:return E(_5K);case 1:return E(_5J);case 2:return E(_5O);case 3:return E(_5N);case 4:return E(_5M);default:return E(_5L);}},_5R=function(_5S){return _E(_5J,_5S);},_5T=function(_5S){return _E(_5K,_5S);},_5U=function(_5S){return _E(_5L,_5S);},_5V=function(_5S){return _E(_5M,_5S);},_5W=function(_5S){return _E(_5N,_5S);},_5X=function(_5S){return _E(_5O,_5S);},_5Y=function(_5Z){switch(E(_5Z)){case 0:return E(_5T);case 1:return E(_5R);case 2:return E(_5X);case 3:return E(_5W);case 4:return E(_5V);default:return E(_5U);}},_60=function(_61,_62){return _1u(_5Y,_61,_62);},_63=function(_64,_65){switch(E(_65)){case 0:return E(_5T);case 1:return E(_5R);case 2:return E(_5X);case 3:return E(_5W);case 4:return E(_5V);default:return E(_5U);}},_66=[0,_63,_5P,_60],_67=new T(function(){return [0,_5E,_66,_68,_5G];}),_68=function(_5S){return [0,_67,_5S];},_69=3,_6a=function(_6b,_6c){return die(new T(function(){return A(_6c,[_6b]);}));},_6d=new T(function(){return _6a(_69,_68);}),_6e=function(_6f){var _6g=jsRound(_6f);return [0,_6g];},_6h=new T(function(){return [0,"(function(s){return s[0];})"];}),_6i=function(_6j){var _6k=A(_6j,[_]);return E(_6k);},_6l=function(_6m){return _6i(function(_){var _=0;return eval(E(_6m)[1]);});},_6n=new T(function(){return _6l(_6h);}),_6o=function(_6p,_){var _6q=A(_6n,[E(_6p),_]);return new T(function(){return _6e(_6q);});},_6r=function(_6s,_){return _6o(_6s,_);},_6t=function(_6u,_6v){var _6w=_6u%_6v;if(_6u<=0){if(_6u>=0){return E(_6w);}else{if(_6v<=0){return E(_6w);}else{var _6x=E(_6w);return _6x==0?0:_6x+_6v|0;}}}else{if(_6v>=0){if(_6u>=0){return E(_6w);}else{if(_6v<=0){return E(_6w);}else{var _6y=E(_6w);return _6y==0?0:_6y+_6v|0;}}}else{var _6z=E(_6w);return _6z==0?0:_6z+_6v|0;}}},_6A=new T(function(){return [0,"(function(s){return md51(s.join(\',\'));})"];}),_6B=new T(function(){return _6l(_6A);}),_6C=function(_6D,_){return A(_6B,[E(_6D),_]);},_6E=function(_6s,_){return _6C(_6s,_);},_6F=function(_6G){return _6i(function(_){var _=0;return _6E(_6G,_);});},_6H=function(_6I,_6J,_6K){while(1){var _6L=(function(_6M,_6N,_6O){if(_6M>_6N){var _6P=_6N,_6Q=_6M,_6R=_6O;_6I=_6P;_6J=_6Q;_6K=_6R;return null;}else{return [0,new T(function(){var _6S=(_6N-_6M|0)+1|0;switch(_6S){case -1:return [0,_6M];case 0:return E(_6d);default:return [0,_6t(_6i(function(_){var _=0;return _6r(_6O,_);})[1],_6S)+_6M|0];}}),new T(function(){return _6F(_6O);})];}})(_6I,_6J,_6K);if(_6L!=null){return _6L;}}},_6T=0,_6U=function(_6V,_6W,_6X,_){var _6Y=E(_6X),_6Z=_6Y[1],_70=jsPushState(_6Z),_71=jsRotate(_6Z,_6V),_72=A(_6W,[_6Y,_]),_73=jsPopState(_6Z);return _6T;},_74=unCStr("Control.Exception.Base"),_75=unCStr("base"),_76=unCStr("PatternMatchFail"),_77=new T(function(){var _78=hs_wordToWord64(18445595),_79=hs_wordToWord64(52003073);return [0,_78,_79,[0,_78,_79,_75,_74,_76],_e];}),_7a=function(_7b){return E(_77);},_7c=function(_7d){var _7e=E(_7d);return _o(_k(_7e[1]),_7a,_7e[2]);},_7f=function(_7g){return E(E(_7g)[1]);},_7h=function(_7i,_7j){return _E(E(_7i)[1],_7j);},_7k=function(_7l,_7m){return _1u(_7h,_7l,_7m);},_7n=function(_7o,_7p,_7q){return _E(E(_7p)[1],_7q);},_7r=[0,_7n,_7f,_7k],_7s=new T(function(){return [0,_7a,_7r,_7t,_7c];}),_7t=function(_7u){return [0,_7s,_7u];},_7v=unCStr("Non-exhaustive patterns in"),_7w=function(_7x,_7y){var _7z=E(_7y);if(!_7z[0]){return [0,_e,_e];}else{var _7A=_7z[1];if(!A(_7x,[_7A])){return [0,_e,_7z];}else{var _7B=new T(function(){var _7C=_7w(_7x,_7z[2]);return [0,_7C[1],_7C[2]];});return [0,[1,_7A,new T(function(){return E(E(_7B)[1]);})],new T(function(){return E(E(_7B)[2]);})];}}},_7D=[0,32],_7E=[0,10],_7F=[1,_7E,_e],_7G=function(_7H){return E(E(_7H)[1])==124?false:true;},_7I=function(_7J,_7K){var _7L=_7w(_7G,unCStr(_7J)),_7M=_7L[1],_7N=function(_7O,_7P){return _E(_7O,new T(function(){return unAppCStr(": ",new T(function(){return _E(_7K,new T(function(){return _E(_7P,_7F);}));}));}));},_7Q=E(_7L[2]);return _7Q[0]==0?_7N(_7M,_e):E(E(_7Q[1])[1])==124?_7N(_7M,[1,_7D,_7Q[2]]):_7N(_7M,_e);},_7R=function(_7S){return _6a([0,new T(function(){return _7I(_7S,_7v);})],_7t);},_7T=new T(function(){return _7R("chess.hs:30:1-28|function side");}),_7U=function(_7V,_7W,_7X,_7Y){var _7Z=new T(function(){return [0,E(_7Y)[1]+20|0];});return function(_80){return [0,function(_){var _81=E(_7V),_82=_81[1],_83=jsPushState(_82),_84=jsTranslate(_82,E(new T(function(){var _85=E(_7X)[1],_86=E(_7W);return _86[0]==0?E(_7T):E(E(_86[1])[1])==0?[0,_85+5|0]:[0,_85+35|0];}))[1],E(_7Z)[1]),_87=A(new T(function(){var _88=E(_7W);if(!_88[0]){return E(_7T);}else{var _89=E(_88[1]),_8a=_89[2];return E(_89[1])==0?function(_8b){return function(_){var _8c=jsDrawText(E(_8b)[1],E(new T(function(){switch(E(_8a)){case 0:return [0,toJSStr(E(_5p))];case 1:return [0,toJSStr(E(_5o))];case 2:return [0,toJSStr(E(_5t))];case 3:return [0,toJSStr(E(_5s))];case 4:return [0,toJSStr(E(_5r))];default:return [0,toJSStr(E(_5q))];}}))[1],0,0);return _6T;};}:function(_8d,_8e){return _6U(3.141592653589793,function(_8f,_){var _8g=jsDrawText(E(_8f)[1],E(new T(function(){switch(E(_8a)){case 0:return [0,toJSStr(E(_5p))];case 1:return [0,toJSStr(E(_5o))];case 2:return [0,toJSStr(E(_5t))];case 3:return [0,toJSStr(E(_5s))];case 4:return [0,toJSStr(E(_5r))];default:return [0,toJSStr(E(_5q))];}}))[1],0,0);return _6T;},_8d,_8e);};}}),[_81,_]),_8h=jsPopState(_82);return new T(function(){return A(_80,[_6T]);});}];};},_8i=function(_8j,_8k,_8l,_8m,_){var _8n=E(_8m),_8o=_8n[1],_8p=jsPushState(_8o),_8q=jsTranslate(_8o,_8j,_8k),_8r=A(_8l,[_8n,_]),_8s=jsPopState(_8o);return _6T;},_8t=2,_8u=0,_8v=unCStr("(Array.!): undefined array element"),_8w=new T(function(){return err(_8v);}),_8x=function(_8y,_8z,_8A){var _8B=E(_8A);if(!_8B[0]){return [0];}else{var _8C=_8B[1],_8D=_8B[2];return !A(_8y,[_8z,_8C])?[1,_8C,new T(function(){return _8x(_8y,_8z,_8D);})]:E(_8D);}},_8E=unCStr("Error in array index"),_8F=new T(function(){return err(_8E);}),_8G=[0,5],_8H=[0,3],_8I=1,_8J=4,_8K=[0,_8I,_8J],_8L=[1,_8K],_8M=function(_8N,_8O){var _8P=E(_8N),_8Q=E(_8O);return E(_8P[1])[1]!=E(_8Q[1])[1]?false:_55(_8P[2],_8Q[2]);},_8R=new T(function(){return _7R("chess.hs:32:1-29|function piece");}),_8S=function(_8T){var _8U=A(_8T,[_]);return E(_8U);},_8V=function(_8W,_8X,_8Y,_8Z,_90,_91){var _92=E(_91),_93=E(_92[1]),_94=_93[1],_95=_93[2],_96=E(_92[2]),_97=_96[1],_98=_96[2];return [0,new T(function(){var _99=E(_8W),_9a=_99[3],_9b=_99[4],_9c=E(_99[1]),_9d=E(_99[2]);return _8S(function(_){var _9e=newArr(_9a,_8w),_=(function(_9f,_){while(1){if(_9f!=_9a){var _=_9e[_9f]=_9b[_9f],_9g=_9f+1|0;_9f=_9g;continue;}else{return E(_);}}})(0,_),_9h=E(_9c[1])[1],_9i=E(_9d[1])[1],_9j=E(_94)[1];if(_9h>_9j){return E(_8F);}else{if(_9j>_9i){return E(_8F);}else{var _9k=E(_9c[2])[1],_9l=E(_9d[2])[1],_9m=E(_95),_9n=_9m[1];if(_9k>_9n){return E(_8F);}else{if(_9n>_9l){return E(_8F);}else{var _9o=E(_9b[(imul(_9j-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9n-_9k|0)|0]);if(!_9o[0]){return E(_8R);}else{var _9p=[1,new T(function(){if(_9h>_9j){return E(_8F);}else{if(_9j>_9i){return E(_8F);}else{if(_9k>_9n){return E(_8F);}else{if(_9n>_9l){return E(_8F);}else{var _9q=E(_9b[(imul(_9j-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9n-_9k|0)|0]);if(!_9q[0]){return [0,_96,_1O];}else{var _9r=E(_9q[1]);return E(_9r[2])==0?E(_9r[1])==0?E(E(_98)[1])==0?[0,_96,[1,[0,_8u,_90]]]:[0,_96,_9q]:E(E(_98)[1])==7?[0,_96,_8L]:[0,_96,_9q]:[0,_96,_9q];}}}}}}),_e];switch(E(E(_9o[1])[2])){case 0:var _9s=E(_8Z);if(!_9s[0]){return (function(_,_9t,_9u,_9v,_9w){if(_9h>_9t){return E(_8F);}else{if(_9t>_9i){return E(_8F);}else{if(_9k>_9u){return E(_8F);}else{if(_9u>_9l){return E(_8F);}else{var _=_9e[(imul(_9t-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9u-_9k|0)|0]=_9v;return (function(_9x,_){while(1){var _9y=E(_9x);if(!_9y[0]){var _9z=_9e;return [0,E(_9c),E(_9d),_9a,_9z];}else{var _9A=E(_9y[1]),_9B=E(_9A[1]),_9C=E(_9B[1])[1];if(_9h>_9C){return E(_8F);}else{if(_9C>_9i){return E(_8F);}else{var _9D=E(_9B[2])[1];if(_9k>_9D){return E(_8F);}else{if(_9D>_9l){return E(_8F);}else{var _=_9e[(imul(_9C-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9D-_9k|0)|0]=_9A[2];_9x=_9y[2];continue;}}}}}}})(_9w,_);}}}}})(_,_9j,_9n,_1O,_9p);}else{var _9E=E(E(_9s[1])[2]),_9F=E(_97)[1],_9G=function(_){return (function(_,_9H,_9I,_9J,_9K){if(_9h>_9H){return E(_8F);}else{if(_9H>_9i){return E(_8F);}else{if(_9k>_9I){return E(_8F);}else{if(_9I>_9l){return E(_8F);}else{var _=_9e[(imul(_9H-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9I-_9k|0)|0]=_9J;return (function(_9L,_){while(1){var _9M=E(_9L);if(!_9M[0]){var _9N=_9e;return [0,E(_9c),E(_9d),_9a,_9N];}else{var _9O=E(_9M[1]),_9P=E(_9O[1]),_9Q=E(_9P[1])[1];if(_9h>_9Q){return E(_8F);}else{if(_9Q>_9i){return E(_8F);}else{var _9R=E(_9P[2])[1];if(_9k>_9R){return E(_8F);}else{if(_9R>_9l){return E(_8F);}else{var _=_9e[(imul(_9Q-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9R-_9k|0)|0]=_9O[2];_9L=_9M[2];continue;}}}}}}})(_9K,_);}}}}})(_,_9j,_9n,_1O,_9p);};if(_9F!=E(_9E[1])[1]){return _9G(_);}else{var _9S=E(_98)[1];return _9S!=E(_9E[2])[1]?_9G(_):(function(_,_9T,_9U,_9V,_9W){if(_9h>_9T){return E(_8F);}else{if(_9T>_9i){return E(_8F);}else{var _9X=E(_9U)[1];if(_9k>_9X){return E(_8F);}else{if(_9X>_9l){return E(_8F);}else{var _=_9e[(imul(_9T-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9X-_9k|0)|0]=_9V;return (function(_9Y,_){while(1){var _9Z=E(_9Y);if(!_9Z[0]){var _a0=_9e;return [0,E(_9c),E(_9d),_9a,_a0];}else{var _a1=E(_9Z[1]),_a2=E(_a1[1]),_a3=E(_a2[1])[1];if(_9h>_a3){return E(_8F);}else{if(_a3>_9i){return E(_8F);}else{var _a4=E(_a2[2])[1];if(_9k>_a4){return E(_8F);}else{if(_a4>_9l){return E(_8F);}else{var _=_9e[(imul(_a3-_9h|0,(_9l-_9k|0)+1|0)|0)+(_a4-_9k|0)|0]=_a1[2];_9Y=_9Z[2];continue;}}}}}}})(_9W,_);}}}}})(_,_9F,new T(function(){return E(_8X)==0?[0,_9S+1|0]:[0,_9S-1|0];}),_1O,[1,[0,_93,_1O],_9p]);}}break;case 1:return (function(_,_a5,_a6,_a7,_a8){if(_9h>_a5){return E(_8F);}else{if(_a5>_9i){return E(_8F);}else{if(_9k>_a6){return E(_8F);}else{if(_a6>_9l){return E(_8F);}else{var _=_9e[(imul(_a5-_9h|0,(_9l-_9k|0)+1|0)|0)+(_a6-_9k|0)|0]=_a7;return (function(_a9,_){while(1){var _aa=E(_a9);if(!_aa[0]){var _ab=_9e;return [0,E(_9c),E(_9d),_9a,_ab];}else{var _ac=E(_aa[1]),_ad=E(_ac[1]),_ae=E(_ad[1])[1];if(_9h>_ae){return E(_8F);}else{if(_ae>_9i){return E(_8F);}else{var _af=E(_ad[2])[1];if(_9k>_af){return E(_8F);}else{if(_af>_9l){return E(_8F);}else{var _=_9e[(imul(_ae-_9h|0,(_9l-_9k|0)+1|0)|0)+(_af-_9k|0)|0]=_ac[2];_a9=_aa[2];continue;}}}}}}})(_a8,_);}}}}})(_,_9j,_9n,_1O,_9p);case 2:return (function(_,_ag,_ah,_ai,_aj){if(_9h>_ag){return E(_8F);}else{if(_ag>_9i){return E(_8F);}else{if(_9k>_ah){return E(_8F);}else{if(_ah>_9l){return E(_8F);}else{var _=_9e[(imul(_ag-_9h|0,(_9l-_9k|0)+1|0)|0)+(_ah-_9k|0)|0]=_ai;return (function(_ak,_){while(1){var _al=E(_ak);if(!_al[0]){var _am=_9e;return [0,E(_9c),E(_9d),_9a,_am];}else{var _an=E(_al[1]),_ao=E(_an[1]),_ap=E(_ao[1])[1];if(_9h>_ap){return E(_8F);}else{if(_ap>_9i){return E(_8F);}else{var _aq=E(_ao[2])[1];if(_9k>_aq){return E(_8F);}else{if(_aq>_9l){return E(_8F);}else{var _=_9e[(imul(_ap-_9h|0,(_9l-_9k|0)+1|0)|0)+(_aq-_9k|0)|0]=_an[2];_ak=_al[2];continue;}}}}}}})(_aj,_);}}}}})(_,_9j,_9n,_1O,_9p);case 3:return (function(_,_ar,_as,_at,_au){if(_9h>_ar){return E(_8F);}else{if(_ar>_9i){return E(_8F);}else{if(_9k>_as){return E(_8F);}else{if(_as>_9l){return E(_8F);}else{var _=_9e[(imul(_ar-_9h|0,(_9l-_9k|0)+1|0)|0)+(_as-_9k|0)|0]=_at;return (function(_av,_){while(1){var _aw=E(_av);if(!_aw[0]){var _ax=_9e;return [0,E(_9c),E(_9d),_9a,_ax];}else{var _ay=E(_aw[1]),_az=E(_ay[1]),_aA=E(_az[1])[1];if(_9h>_aA){return E(_8F);}else{if(_aA>_9i){return E(_8F);}else{var _aB=E(_az[2])[1];if(_9k>_aB){return E(_8F);}else{if(_aB>_9l){return E(_8F);}else{var _=_9e[(imul(_aA-_9h|0,(_9l-_9k|0)+1|0)|0)+(_aB-_9k|0)|0]=_ay[2];_av=_aw[2];continue;}}}}}}})(_au,_);}}}}})(_,_9j,_9n,_1O,_9p);case 4:return (function(_,_aC,_aD,_aE,_aF){if(_9h>_aC){return E(_8F);}else{if(_aC>_9i){return E(_8F);}else{if(_9k>_aD){return E(_8F);}else{if(_aD>_9l){return E(_8F);}else{var _=_9e[(imul(_aC-_9h|0,(_9l-_9k|0)+1|0)|0)+(_aD-_9k|0)|0]=_aE;return (function(_aG,_){while(1){var _aH=E(_aG);if(!_aH[0]){var _aI=_9e;return [0,E(_9c),E(_9d),_9a,_aI];}else{var _aJ=E(_aH[1]),_aK=E(_aJ[1]),_aL=E(_aK[1])[1];if(_9h>_aL){return E(_8F);}else{if(_aL>_9i){return E(_8F);}else{var _aM=E(_aK[2])[1];if(_9k>_aM){return E(_8F);}else{if(_aM>_9l){return E(_8F);}else{var _=_9e[(imul(_aL-_9h|0,(_9l-_9k|0)+1|0)|0)+(_aM-_9k|0)|0]=_aJ[2];_aG=_aH[2];continue;}}}}}}})(_aF,_);}}}}})(_,_9j,_9n,_1O,_9p);default:switch(_9j-E(_97)[1]|0){case -2:return (function(_,_aN,_aO,_aP,_aQ){if(_9h>_aN){return E(_8F);}else{if(_aN>_9i){return E(_8F);}else{if(_9k>_aO){return E(_8F);}else{if(_aO>_9l){return E(_8F);}else{var _=_9e[(imul(_aN-_9h|0,(_9l-_9k|0)+1|0)|0)+(_aO-_9k|0)|0]=_aP;return (function(_aR,_){while(1){var _aS=E(_aR);if(!_aS[0]){var _aT=_9e;return [0,E(_9c),E(_9d),_9a,_aT];}else{var _aU=E(_aS[1]),_aV=E(_aU[1]),_aW=E(_aV[1])[1];if(_9h>_aW){return E(_8F);}else{if(_aW>_9i){return E(_8F);}else{var _aX=E(_aV[2])[1];if(_9k>_aX){return E(_8F);}else{if(_aX>_9l){return E(_8F);}else{var _=_9e[(imul(_aW-_9h|0,(_9l-_9k|0)+1|0)|0)+(_aX-_9k|0)|0]=_aU[2];_aR=_aS[2];continue;}}}}}}})(_aQ,_);}}}}})(_,7,_9n,_1O,[1,[0,[0,_8G,_9m],new T(function(){return _9h>7?E(_8F):7>_9i?E(_8F):E(_9b[(imul(7-_9h|0,(_9l-_9k|0)+1|0)|0)+(_9n-_9k|0)|0]);})],[1,[0,_93,_1O],_9p]]);case 2:return (function(_,_aY,_aZ,_b0,_b1){if(_9h>_aY){return E(_8F);}else{if(_aY>_9i){return E(_8F);}else{if(_9k>_aZ){return E(_8F);}else{if(_aZ>_9l){return E(_8F);}else{var _=_9e[(imul(_aY-_9h|0,(_9l-_9k|0)+1|0)|0)+(_aZ-_9k|0)|0]=_b0;return (function(_b2,_){while(1){var _b3=E(_b2);if(!_b3[0]){var _b4=_9e;return [0,E(_9c),E(_9d),_9a,_b4];}else{var _b5=E(_b3[1]),_b6=E(_b5[1]),_b7=E(_b6[1])[1];if(_9h>_b7){return E(_8F);}else{if(_b7>_9i){return E(_8F);}else{var _b8=E(_b6[2])[1];if(_9k>_b8){return E(_8F);}else{if(_b8>_9l){return E(_8F);}else{var _=_9e[(imul(_b7-_9h|0,(_9l-_9k|0)+1|0)|0)+(_b8-_9k|0)|0]=_b5[2];_b2=_b3[2];continue;}}}}}}})(_b1,_);}}}}})(_,0,_9n,_1O,[1,[0,[0,_8H,_9m],new T(function(){return _9h>0?E(_8F):0>_9i?E(_8F):E(_9b[(imul( -_9h|0,(_9l-_9k|0)+1|0)|0)+(_9n-_9k|0)|0]);})],[1,[0,_93,_1O],_9p]]);default:return (function(_,_b9,_ba,_bb,_bc){if(_9h>_b9){return E(_8F);}else{if(_b9>_9i){return E(_8F);}else{if(_9k>_ba){return E(_8F);}else{if(_ba>_9l){return E(_8F);}else{var _=_9e[(imul(_b9-_9h|0,(_9l-_9k|0)+1|0)|0)+(_ba-_9k|0)|0]=_bb;return (function(_bd,_){while(1){var _be=E(_bd);if(!_be[0]){var _bf=_9e;return [0,E(_9c),E(_9d),_9a,_bf];}else{var _bg=E(_be[1]),_bh=E(_bg[1]),_bi=E(_bh[1])[1];if(_9h>_bi){return E(_8F);}else{if(_bi>_9i){return E(_8F);}else{var _bj=E(_bh[2])[1];if(_9k>_bj){return E(_8F);}else{if(_bj>_9l){return E(_8F);}else{var _=_9e[(imul(_bi-_9h|0,(_9l-_9k|0)+1|0)|0)+(_bj-_9k|0)|0]=_bg[2];_bd=_be[2];continue;}}}}}}})(_bc,_);}}}}})(_,_9j,_9n,_1O,_9p);}}}}}}}});}),_8t,new T(function(){return E(_8X)==0?1:0;}),_1O,_1O,new T(function(){return _8x(_8M,_93,_8Y);}),new T(function(){var _bk=E(_8W),_bl=E(_bk[1]),_bm=E(_bk[2]),_bn=E(_bl[1])[1],_bo=E(_94),_bp=_bo[1];if(_bn>_bp){return E(_8F);}else{if(_bp>E(_bm[1])[1]){return E(_8F);}else{var _bq=E(_bl[2])[1],_br=E(_bm[2])[1],_bs=E(_95)[1];if(_bq>_bs){return E(_8F);}else{if(_bs>_br){return E(_8F);}else{var _bt=E(_bk[4][(imul(_bp-_bn|0,(_br-_bq|0)+1|0)|0)+(_bs-_bq|0)|0]);if(!_bt[0]){return E(_8R);}else{switch(E(E(_bt[1])[2])){case 0:var _bu=function(_bv){return (_bs+_bv|0)==E(_98)[1]?[0]:[1,[0,_8X,[0,_bo,new T(function(){return E(_8X)==0?[0,_bs+(-1)|0]:[0,_bs+1|0];})]]];};return E(_8X)==0?_bu(-1):_bu(1);case 1:return [0];case 2:return [0];case 3:return [0];case 4:return [0];default:return [0];}}}}}}}),_92,_90];},_bw=function(_bx,_by){return [0,E(_bx)[1]+E(_by)[1]|0];},_bz=true,_bA=[0,0],_bB=unCStr("Irrefutable pattern failed for pattern"),_bC=function(_bD){return _6a([0,new T(function(){return _7I(_bD,_bB);})],_7t);},_bE=new T(function(){return _bC("chess.hs:134:96-113|Data.Maybe.Just (es, ej)");}),_bF=[0,-1],_bG=[0,1],_bH=function(_bI){var _bJ=E(_bI);return [0,_bJ[2],_bJ[1]];},_bK=[1,_bH,_e],_bL=function(_bM,_bN){if(_bM<=_bN){var _bO=function(_bP){return [1,[0,_bP],new T(function(){return _bP!=_bN?_bO(_bP+1|0):[0];})];};return _bO(_bM);}else{return [0];}},_bQ=new T(function(){return _bL(-1,1);}),_bR=function(_bS,_bT){var _bU=A(_bS,[_bT]);if(!_bU[0]){return [0];}else{var _bV=E(_bU[1]);return [1,_bV[1],new T(function(){return _bR(_bS,_bV[2]);})];}},_bW=function(_bX,_bY,_bZ,_c0){var _c1=E(_bZ),_c2=_c1[4],_c3=E(_c1[1]),_c4=E(_c1[2]),_c5=E(_c3[1])[1],_c6=E(_c3[2])[1],_c7=E(_c4[1])[1],_c8=E(_c4[2])[1];if(_c5>_bX){return E(_8F);}else{if(_bX>_c7){return E(_8F);}else{if(_c6>_bY){return E(_8F);}else{if(_bY>_c8){return E(_8F);}else{var _c9=E(_c2[(imul(_bX-_c5|0,(_c8-_c6|0)+1|0)|0)+(_bY-_c6|0)|0]);if(!_c9[0]){return E(_8R);}else{var _ca=function(_cb,_cc){if(0>_cb){return false;}else{if(_cb>7){return false;}else{var _cd=E(_cc)[1];return 0>_cd?false:_cd>7?false:_c5>_cb?E(_8F):_cb>_c7?E(_8F):_c6>_cd?E(_8F):_cd>_c8?E(_8F):E(_c2[(imul(_cb-_c5|0,(_c8-_c6|0)+1|0)|0)+(_cd-_c6|0)|0])[0]==0?true:false;}}},_ce=new T(function(){if(_c5>_bX){return E(_8F);}else{if(_bX>_c7){return E(_8F);}else{if(_c6>_bY){return E(_8F);}else{if(_bY>_c8){return E(_8F);}else{var _cf=E(_c2[(imul(_bX-_c5|0,(_c8-_c6|0)+1|0)|0)+(_bY-_c6|0)|0]);return _cf[0]==0?E(_7T):E(E(_cf[1])[1]);}}}}}),_cg=function(_ch,_ci){if(0>_ch){return false;}else{if(_ch>7){return false;}else{if(0>_ci){return false;}else{if(_ci>7){return false;}else{if(_c5>_ch){return E(_8F);}else{if(_ch>_c7){return E(_8F);}else{if(_c6>_ci){return E(_8F);}else{if(_ci>_c8){return E(_8F);}else{var _cj=E(_c2[(imul(_ch-_c5|0,(_c8-_c6|0)+1|0)|0)+(_ci-_c6|0)|0]);return _cj[0]==0?true:E(E(_cj[1])[1])==0?E(_ce)==0?false:true:E(_ce)==0?true:false;}}}}}}}}},_ck=function(_cl,_cm){if(0>_cl){return false;}else{if(_cl>7){return false;}else{var _cn=E(_cm)[1];if(0>_cn){return false;}else{if(_cn>7){return false;}else{if(_c5>_cl){return E(_8F);}else{if(_cl>_c7){return E(_8F);}else{if(_c6>_cn){return E(_8F);}else{if(_cn>_c8){return E(_8F);}else{var _co=E(_c2[(imul(_cl-_c5|0,(_c8-_c6|0)+1|0)|0)+(_cn-_c6|0)|0]);return _co[0]==0?true:E(E(_co[1])[1])==0?E(_ce)==0?false:true:E(_ce)==0?true:false;}}}}}}}}},_cp=function(_cq,_cr){return _bR(function(_cs){var _ct=E(_cs),_cu=_ct[2];if(!E(_ct[3])){return [0];}else{var _cv=E(_ct[1])[1];return !_ck(_bX+_cv|0,new T(function(){return [0,_bY+E(_cu)[1]|0];}))?[0]:[1,[0,[0,[0,_bX+_cv|0],new T(function(){return [0,_bY+E(_cu)[1]|0];})],[0,new T(function(){return [0,_cv+E(_cq)[1]|0];}),new T(function(){return _bw(_cu,_cr);}),new T(function(){return _ca(_bX+_cv|0,new T(function(){return [0,_bY+E(_cu)[1]|0];}));})]]];}},[0,_cq,_cr,_bz]);};switch(E(E(_c9[1])[2])){case 0:var _cw=new T(function(){return E(_ce)==0?[0,_bY+(-1)|0]:[0,_bY+1|0];}),_cx=new T(function(){var _cy=new T(function(){var _cz=E(_c0);return _cz[0]==0?E(_bE):E(_cz[1]);}),_cA=new T(function(){return E(E(_cy)[1])==0?E(_ce)==0?false:true:E(_ce)==0?true:false;}),_cB=function(_cC,_cD){var _cE=_bX+_cC|0,_cF=[0,[0,_cE],_cw],_cG=new T(function(){if(!E(_c0)[0]){return E(_cD);}else{var _cH=E(E(_cy)[2]);return _cE!=E(_cH[1])[1]?E(_cD):E(_cw)[1]!=E(_cH[2])[1]?E(_cD):!E(_cA)?E(_cD):[1,_cF,_cD];}});if(0>_cE){return E(_cG);}else{if(_cE>7){return E(_cG);}else{var _cI=E(_cw)[1];if(0>_cI){return E(_cG);}else{if(_cI>7){return E(_cG);}else{if(_c5>_cE){return E(_8F);}else{if(_cE>_c7){return E(_8F);}else{if(_c6>_cI){return E(_8F);}else{if(_cI>_c8){return E(_8F);}else{var _cJ=E(_c2[(imul(_cE-_c5|0,(_c8-_c6|0)+1|0)|0)+(_cI-_c6|0)|0]);return _cJ[0]==0?E(_cG):E(E(_cJ[1])[1])==0?E(_ce)==0?E(_cG):[1,_cF,_cD]:E(_ce)==0?[1,_cF,_cD]:E(_cG);}}}}}}}}};return _cB(-1,new T(function(){return _cB(1,_e);}));});if(!_ca(_bX,_cw)){return E(_cx);}else{var _cK=[0,_bX];return _E([1,[0,_cK,_cw],new T(function(){var _cL=new T(function(){return E(_ce)==0?[0,_bY+(-2)|0]:[0,_bY+2|0];});return E(_ce)==0?E(_bY)==6?!_ca(_bX,_cL)?[0]:[1,[0,_cK,_cL],_e]:[0]:E(_bY)==1?!_ca(_bX,_cL)?[0]:[1,[0,_cK,_cL],_e]:[0];})],_cx);}break;case 1:var _cM=function(_cN,_cO){var _cP=function(_cQ,_cR){var _cS=_bX+(imul(2,_cN)|0)|0,_cT=_bY+_cQ|0,_cU=new T(function(){var _cV=_bX+_cN|0,_cW=_bY+(imul(2,_cQ)|0)|0;return !_cg(_cV,_cW)?E(_cR):[1,[0,[0,_cV],[0,_cW]],_cR];});return !_cg(_cS,_cT)?E(_cU):[1,[0,[0,_cS],[0,_cT]],_cU];};return _cP(-1,new T(function(){return _cP(1,_cO);}));};return _cM(-1,new T(function(){return _cM(1,_e);}));case 2:var _cX=function(_cY,_cZ){return _E(_cp(_cY,_bF),new T(function(){return _E(_cp(_cY,_bG),_cZ);}));};return _cX(_bF,new T(function(){return _cX(_bG,_e);}));case 3:var _d0=function(_d1,_d2){var _d3=[0,_d1,_bA],_d4=function(_d5){var _d6=E(_d5);if(!_d6[0]){return E(_d2);}else{var _d7=new T(function(){return A(_d6[1],[_d3]);});return _E(_cp(new T(function(){return E(E(_d7)[1]);}),new T(function(){return E(E(_d7)[2]);})),new T(function(){return _d4(_d6[2]);}));}};return (function(_d8,_d9){var _da=new T(function(){return A(_d8,[_d3]);});return _E(_cp(new T(function(){return E(E(_da)[1]);}),new T(function(){return E(E(_da)[2]);})),new T(function(){return _d4(_d9);}));})(_4u,_bK);};return _d0(_bF,new T(function(){return _d0(_bG,_e);}));case 4:var _db=function(_dc){var _dd=E(_dc);if(!_dd[0]){return [0];}else{var _de=new T(function(){return _db(_dd[2]);}),_df=function(_dg){var _dh=E(_dg);return _dh[0]==0?E(_de):_E(_cp(_dd[1],_dh[1]),new T(function(){return _df(_dh[2]);}));};return _df(_bQ);}};return _db(_bQ);default:var _di=function(_dj){var _dk=E(_dj);if(!_dk[0]){return [0];}else{var _dl=new T(function(){return _di(_dk[2]);}),_dm=E(_bQ);if(!_dm[0]){return E(_dl);}else{var _dn=_dm[2],_do=_bX+E(_dk[1])[1]|0,_dp=[0,_do],_dq=new T(function(){return [0,_bY+E(_dm[1])[1]|0];});if(!_ck(_do,_dq)){var _dr=function(_ds){while(1){var _dt=(function(_du){var _dv=E(_du);if(!_dv[0]){return E(_dl);}else{var _dw=_dv[2],_dx=new T(function(){return [0,_bY+E(_dv[1])[1]|0];});if(!_ck(_do,_dx)){_ds=_dw;return null;}else{return [1,[0,_dp,_dx],new T(function(){return _dr(_dw);})];}}})(_ds);if(_dt!=null){return _dt;}}};return _dr(_dn);}else{return [1,[0,_dp,_dq],new T(function(){var _dy=function(_dz){while(1){var _dA=(function(_dB){var _dC=E(_dB);if(!_dC[0]){return E(_dl);}else{var _dD=_dC[2],_dE=new T(function(){return [0,_bY+E(_dC[1])[1]|0];});if(!_ck(_do,_dE)){_dz=_dD;return null;}else{return [1,[0,_dp,_dE],new T(function(){return _dy(_dD);})];}}})(_dz);if(_dA!=null){return _dA;}}};return _dy(_dn);})];}}}};return _di(_bQ);}}}}}}},_dF=[0,7],_dG=function(_dH,_dI,_dJ){while(1){var _dK=E(_dJ);if(!_dK[0]){return false;}else{if(!A(_4w,[_dH,_dI,_dK[1]])){_dJ=_dK[2];continue;}else{return true;}}}},_dL=unCStr(": empty list"),_dM=unCStr("Prelude."),_dN=function(_dO){return err(_E(_dM,new T(function(){return _E(_dO,_dL);})));},_dP=unCStr("head"),_dQ=new T(function(){return _dN(_dP);}),_dR=function(_dS){var _dT=function(_dU){return [1,[0,[0,_dS],[0,_dU]],new T(function(){var _dV=E(_dU);return _dV==7?E(new T(function(){var _dW=E(_dS);return _dW==7?[0]:_dR(_dW+1|0);})):_dT(_dV+1|0);})];};return _dT(0);},_dX=new T(function(){return _dR(0);}),_dY=function(_dZ){var _e0=function(_e1){return [1,[0,[0,_dZ],[0,_e1]],new T(function(){var _e2=E(_e1);return _e2==7?E(new T(function(){var _e3=E(_dZ);return _e3==7?[0]:_dY(_e3+1|0);})):_e0(_e2+1|0);})];};return _e0(0);},_e4=new T(function(){return _dY(0);}),_e5=function(_e6,_e7){var _e8=new T(function(){return E(E(_e7)[1]);}),_e9=new T(function(){var _ea=E(_dX);if(!_ea[0]){return E(_dQ);}else{var _eb=_ea[2],_ec=E(_e8),_ed=_ec[4],_ee=E(_ec[1]),_ef=E(_ec[2]),_eg=E(_ea[1]),_eh=E(_ee[1])[1],_ei=E(_ef[1])[1],_ej=E(_eg[1]),_ek=_ej[1];if(_eh>_ek){return E(_8F);}else{if(_ek>_ei){return E(_8F);}else{var _el=E(_ee[2])[1],_em=E(_ef[2])[1],_en=E(_eg[2]),_eo=_en[1];if(_el>_eo){return E(_8F);}else{if(_eo>_em){return E(_8F);}else{var _ep=E(_ed[(imul(_ek-_eh|0,(_em-_el|0)+1|0)|0)+(_eo-_el|0)|0]);if(!_ep[0]){var _eq=(function(_er){while(1){var _es=E(_er);if(!_es[0]){return E(_dQ);}else{var _et=_es[2],_eu=E(_es[1]),_ev=E(_eu[1]),_ew=_ev[1];if(_eh>_ew){return E(_8F);}else{if(_ew>_ei){return E(_8F);}else{var _ex=E(_eu[2]),_ey=_ex[1];if(_el>_ey){return E(_8F);}else{if(_ey>_em){return E(_8F);}else{var _ez=E(_ed[(imul(_ew-_eh|0,(_em-_el|0)+1|0)|0)+(_ey-_el|0)|0]);if(!_ez[0]){_er=_et;continue;}else{var _eA=E(_ez[1]),_eB=_eA[2];if(!E(_eA[1])){if(!E(_e6)){if(E(_eB)==5){return [0,_ev,_ex];}else{_er=_et;continue;}}else{_er=_et;continue;}}else{if(!E(_e6)){_er=_et;continue;}else{if(E(_eB)==5){return [0,_ev,_ex];}else{_er=_et;continue;}}}}}}}}}}})(_eb);return [0,_eq[1],_eq[2]];}else{var _eC=E(_ep[1]),_eD=_eC[2];if(!E(_eC[1])){if(!E(_e6)){if(E(_eD)==5){return [0,_ej,_en];}else{var _eE=(function(_eF){while(1){var _eG=E(_eF);if(!_eG[0]){return E(_dQ);}else{var _eH=_eG[2],_eI=E(_eG[1]),_eJ=E(_eI[1]),_eK=_eJ[1];if(_eh>_eK){return E(_8F);}else{if(_eK>_ei){return E(_8F);}else{var _eL=E(_eI[2]),_eM=_eL[1];if(_el>_eM){return E(_8F);}else{if(_eM>_em){return E(_8F);}else{var _eN=E(_ed[(imul(_eK-_eh|0,(_em-_el|0)+1|0)|0)+(_eM-_el|0)|0]);if(!_eN[0]){_eF=_eH;continue;}else{var _eO=E(_eN[1]);if(!E(_eO[1])){if(E(_eO[2])==5){return [0,_eJ,_eL];}else{_eF=_eH;continue;}}else{_eF=_eH;continue;}}}}}}}}})(_eb);return [0,_eE[1],_eE[2]];}}else{var _eP=(function(_eQ){while(1){var _eR=E(_eQ);if(!_eR[0]){return E(_dQ);}else{var _eS=_eR[2],_eT=E(_eR[1]),_eU=E(_eT[1]),_eV=_eU[1];if(_eh>_eV){return E(_8F);}else{if(_eV>_ei){return E(_8F);}else{var _eW=E(_eT[2]),_eX=_eW[1];if(_el>_eX){return E(_8F);}else{if(_eX>_em){return E(_8F);}else{var _eY=E(_ed[(imul(_eV-_eh|0,(_em-_el|0)+1|0)|0)+(_eX-_el|0)|0]);if(!_eY[0]){_eQ=_eS;continue;}else{var _eZ=E(_eY[1]);if(!E(_eZ[1])){_eQ=_eS;continue;}else{if(E(_eZ[2])==5){return [0,_eU,_eW];}else{_eQ=_eS;continue;}}}}}}}}}})(_eb);return [0,_eP[1],_eP[2]];}}else{if(!E(_e6)){var _f0=(function(_f1){while(1){var _f2=E(_f1);if(!_f2[0]){return E(_dQ);}else{var _f3=_f2[2],_f4=E(_f2[1]),_f5=E(_f4[1]),_f6=_f5[1];if(_eh>_f6){return E(_8F);}else{if(_f6>_ei){return E(_8F);}else{var _f7=E(_f4[2]),_f8=_f7[1];if(_el>_f8){return E(_8F);}else{if(_f8>_em){return E(_8F);}else{var _f9=E(_ed[(imul(_f6-_eh|0,(_em-_el|0)+1|0)|0)+(_f8-_el|0)|0]);if(!_f9[0]){_f1=_f3;continue;}else{var _fa=E(_f9[1]);if(!E(_fa[1])){if(E(_fa[2])==5){return [0,_f5,_f7];}else{_f1=_f3;continue;}}else{_f1=_f3;continue;}}}}}}}}})(_eb);return [0,_f0[1],_f0[2]];}else{if(E(_eD)==5){return [0,_ej,_en];}else{var _fb=(function(_fc){while(1){var _fd=E(_fc);if(!_fd[0]){return E(_dQ);}else{var _fe=_fd[2],_ff=E(_fd[1]),_fg=E(_ff[1]),_fh=_fg[1];if(_eh>_fh){return E(_8F);}else{if(_fh>_ei){return E(_8F);}else{var _fi=E(_ff[2]),_fj=_fi[1];if(_el>_fj){return E(_8F);}else{if(_fj>_em){return E(_8F);}else{var _fk=E(_ed[(imul(_fh-_eh|0,(_em-_el|0)+1|0)|0)+(_fj-_el|0)|0]);if(!_fk[0]){_fc=_fe;continue;}else{var _fl=E(_fk[1]);if(!E(_fl[1])){_fc=_fe;continue;}else{if(E(_fl[2])==5){return [0,_fg,_fi];}else{_fc=_fe;continue;}}}}}}}}}})(_eb);return [0,_fb[1],_fb[2]];}}}}}}}}}}),_fm=E(_e4);if(!_fm[0]){return false;}else{var _fn=E(_e8),_fo=_fn[4],_fp=E(_fn[1]),_fq=E(_fn[2]),_fr=E(_fm[1]),_fs=E(_fp[1])[1],_ft=E(_fq[1])[1],_fu=E(_fr[1])[1];if(_fs>_fu){return E(_8F);}else{if(_fu>_ft){return E(_8F);}else{var _fv=E(_fp[2])[1],_fw=E(_fq[2])[1],_fx=E(_fr[2])[1];if(_fv>_fx){return E(_8F);}else{if(_fx>_fw){return E(_8F);}else{var _fy=new T(function(){var _fz=function(_fA){var _fB=E(_fA);if(!_fB[0]){return false;}else{var _fC=E(_fB[1]),_fD=E(_fC[1])[1];if(_fs>_fD){return E(_8F);}else{if(_fD>_ft){return E(_8F);}else{var _fE=E(_fC[2])[1];if(_fv>_fE){return E(_8F);}else{if(_fE>_fw){return E(_8F);}else{var _fF=new T(function(){return _fz(_fB[2]);}),_fG=E(_fo[(imul(_fD-_fs|0,(_fw-_fv|0)+1|0)|0)+(_fE-_fv|0)|0]);if(!_fG[0]){return E(_fF);}else{var _fH=new T(function(){var _fI=E(_e7);return !_dG(_5c,_e9,_bW(_fD,_fE,_fI[1],_fI[7]))?E(_fF):true;});return E(E(_fG[1])[1])==0?E(_e6)==0?E(_fF):E(_fH):E(_e6)==0?E(_fH):E(_fF);}}}}}}};return _fz(_fm[2]);}),_fJ=E(_fo[(imul(_fu-_fs|0,(_fw-_fv|0)+1|0)|0)+(_fx-_fv|0)|0]);if(!_fJ[0]){return E(_fy);}else{var _fK=new T(function(){var _fL=E(_e7);return !_dG(_5c,_e9,_bW(_fu,_fx,_fL[1],_fL[7]))?E(_fy):true;});return E(E(_fJ[1])[1])==0?E(_e6)==0?E(_fy):E(_fK):E(_e6)==0?E(_fK):E(_fy);}}}}}}},_fM=[0,2],_fN=new T(function(){return _bL(1,3);}),_fO=[1,_8H,_e],_fP=[0,6],_fQ=[1,_fP,_e],_fR=function(_fS,_fT,_fU,_fV,_fW,_fX){var _fY=E(_fS),_fZ=E(_fY[1])[1],_g0=E(_fY[2]),_g1=_g0[1],_g2=function(_g3){while(1){var _g4=(function(_g5){var _g6=E(_g5);if(!_g6[0]){return E(new T(function(){if(!_dG(_5c,_fY,_fV)){return [0];}else{if(E(_fZ)==4){var _g7=new T(function(){if(!_dG(_5c,[0,_dF,_g0],_fV)){return [0];}else{var _g8=E(_fT),_g9=_g8[4],_ga=E(_g8[1]),_gb=E(_g8[2]),_gc=E(_ga[1])[1],_gd=E(_gb[1])[1];if(_gc>5){return E(_8F);}else{if(5>_gd){return E(_8F);}else{var _ge=E(_ga[2])[1],_gf=E(_gb[2])[1];return _ge>_g1?E(_8F):_g1>_gf?E(_8F):E(_g9[(imul(5-_gc|0,(_gf-_ge|0)+1|0)|0)+(_g1-_ge|0)|0])[0]==0?!(function(_gg,_gh){return _gc>_gg?E(_8F):_gg>_gd?E(_8F):_ge>_g1?E(_8F):_g1>_gf?E(_8F):E(_g9[(imul(_gg-_gc|0,(_gf-_ge|0)+1|0)|0)+(_g1-_ge|0)|0])[0]==0?(function(_gi){while(1){var _gj=E(_gi);if(!_gj[0]){return true;}else{var _gk=E(_gj[1])[1];if(_gc>_gk){return E(_8F);}else{if(_gk>_gd){return E(_8F);}else{if(_ge>_g1){return E(_8F);}else{if(_g1>_gf){return E(_8F);}else{if(!E(_g9[(imul(_gk-_gc|0,(_gf-_ge|0)+1|0)|0)+(_g1-_ge|0)|0])[0]){_gi=_gj[2];continue;}else{return false;}}}}}}}})(_gh):false;})(6,_e)?[0]:!(function(_gl,_gm){return !_e5(_fU,new T(function(){var _gn=_8V(_g8,_fU,_fV,_fW,_fX,[0,_fY,[0,_gl,_g0]]);return [0,_gn[1],_gn[2],_gn[3],_gn[4],_gn[5],_gn[6],_gn[7],_gn[8],_gn[9]];}))?(function(_go){while(1){var _gp=(function(_gq){var _gr=E(_gq);if(!_gr[0]){return true;}else{if(!_e5(_fU,new T(function(){var _gs=_8V(_g8,_fU,_fV,_fW,_fX,[0,_fY,[0,_gr[1],_g0]]);return [0,_gs[1],_gs[2],_gs[3],_gs[4],_gs[5],_gs[6],_gs[7],_gs[8],_gs[9]];}))){_go=_gr[2];return null;}else{return false;}}})(_go);if(_gp!=null){return _gp;}}})(_gm):false;})(_8G,_fQ)?[0]:[1,[0,_fP,_g0],_e]:[0];}}}});if(!_dG(_5c,[0,_bA,_g0],_fV)){return E(_g7);}else{var _gt=function(_){return !(function(_gu,_gv){return !_e5(_fU,new T(function(){var _gw=_8V(_fT,_fU,_fV,_fW,_fX,[0,_fY,[0,_gu,_g0]]);return [0,_gw[1],_gw[2],_gw[3],_gw[4],_gw[5],_gw[6],_gw[7],_gw[8],_gw[9]];}))?(function(_gx){while(1){var _gy=(function(_gz){var _gA=E(_gz);if(!_gA[0]){return true;}else{if(!_e5(_fU,new T(function(){var _gB=_8V(_fT,_fU,_fV,_fW,_fX,[0,_fY,[0,_gA[1],_g0]]);return [0,_gB[1],_gB[2],_gB[3],_gB[4],_gB[5],_gB[6],_gB[7],_gB[8],_gB[9]];}))){_gx=_gA[2];return null;}else{return false;}}})(_gx);if(_gy!=null){return _gy;}}})(_gv):false;})(_fM,_fO)?E(_g7):[1,[0,_fM,_g0],_g7];},_gC=E(_fN);if(!_gC[0]){return _gt(_);}else{var _gD=E(_fT),_gE=_gD[4],_gF=E(_gD[1]),_gG=E(_gD[2]),_gH=E(_gF[1])[1],_gI=E(_gG[1])[1],_gJ=E(_gC[1])[1];if(_gH>_gJ){return E(_8F);}else{if(_gJ>_gI){return E(_8F);}else{var _gK=E(_gF[2])[1],_gL=E(_gG[2])[1];return _gK>_g1?E(_8F):_g1>_gL?E(_8F):E(_gE[(imul(_gJ-_gH|0,(_gL-_gK|0)+1|0)|0)+(_g1-_gK|0)|0])[0]==0?!(function(_gM){while(1){var _gN=E(_gM);if(!_gN[0]){return true;}else{var _gO=E(_gN[1])[1];if(_gH>_gO){return E(_8F);}else{if(_gO>_gI){return E(_8F);}else{if(_gK>_g1){return E(_8F);}else{if(_g1>_gL){return E(_8F);}else{if(!E(_gE[(imul(_gO-_gH|0,(_gL-_gK|0)+1|0)|0)+(_g1-_gK|0)|0])[0]){_gM=_gN[2];continue;}else{return false;}}}}}}}})(_gC[2])?E(_g7):_gt(_):E(_g7);}}}}}else{return [0];}}}));}else{var _gP=_g6[1],_gQ=_g6[2];if(!_e5(_fU,new T(function(){var _gR=_8V(_fT,_fU,_fV,_fW,_fX,[0,_fY,_gP]);return [0,_gR[1],_gR[2],_gR[3],_gR[4],_gR[5],_gR[6],_gR[7],_gR[8],_gR[9]];}))){return [1,_gP,new T(function(){return _g2(_gQ);})];}else{_g3=_gQ;return null;}}})(_g3);if(_g4!=null){return _g4;}}};return _g2(_bW(_fZ,_g1,_fT,_fW));},_gS=function(_gT,_gU){while(1){var _gV=E(_gT);if(!_gV[0]){return E(_gU);}else{_gT=_gV[2];var _gW=_gU+1|0;_gU=_gW;continue;}}},_gX=[0,0],_gY=function(_gZ,_h0){var _h1=E(_gZ);if(!_h1[0]){var _h2=_h1[1],_h3=E(_h0);return _h3[0]==0?_h2==_h3[1]:I_compareInt(_h3[1],_h2)==0?true:false;}else{var _h4=_h1[1],_h5=E(_h0);return _h5[0]==0?I_compareInt(_h4,_h5[1])==0?true:false:I_compare(_h4,_h5[1])==0?true:false;}},_h6=[0,1],_h7=function(_h8,_h9){while(1){var _ha=E(_h8);if(!_ha[0]){var _hb=_ha[1],_hc=E(_h9);if(!_hc[0]){var _hd=_hc[1],_he=subC(_hb,_hd);if(!E(_he[2])){return [0,_he[1]];}else{_h8=[1,I_fromInt(_hb)];_h9=[1,I_fromInt(_hd)];continue;}}else{_h8=[1,I_fromInt(_hb)];_h9=_hc;continue;}}else{var _hf=E(_h9);if(!_hf[0]){_h8=_ha;_h9=[1,I_fromInt(_hf[1])];continue;}else{return [1,I_sub(_ha[1],_hf[1])];}}}},_hg=function(_hh,_hi){if(!_gY(_hh,_gX)){var _hj=E(_hi);return [0,_hj[1],new T(function(){var _hk=new T(function(){return _h7(_hh,_h6);});return _2b(function(_hl){var _hm=_hg(_hk,_hl);return [0,_hm[1],_hm[2]];},_hj[2]);})];}else{return [0,E(_hi)[1],_e];}},_hn=function(_ho,_hp,_hq,_hr){return A(_ho,[new T(function(){return function(_){var _hs=jsSet(E(_hp)[1],toJSStr(E(_hq)),toJSStr(E(_hr)));return _6T;};})]);},_ht=function(_hu,_hv){var _hw=E(_hu);if(!_hw){return [0,_e,_hv];}else{var _hx=E(_hv);if(!_hx[0]){return [0,_e,_e];}else{var _hy=new T(function(){var _hz=_ht(_hw-1|0,_hx[2]);return [0,_hz[1],_hz[2]];});return [0,[1,_hx[1],new T(function(){return E(E(_hy)[1]);})],new T(function(){return E(E(_hy)[2]);})];}}},_hA=function(_hB,_hC,_hD,_hE,_){var _hF=jsDrawText(E(_hB)[1],E(_hC)[1],E(_hD)[1],E(_hE)[1]);return _6T;},_hG=function(_hH,_hI,_hJ,_hK,_){return _hA(_hH,_hI,_hJ,_hK,_);},_hL=function(_hM,_hN,_hO){var _hP=new T(function(){return [0,toJSStr(E(_hO))];});return function(_hQ,_){return _hG(_hQ,_hP,_hM,_hN,_);};},_hR=function(_hS,_hT){var _hU=_hV(_hS,_hT);return [0,_hU[1],_hU[2]];},_hV=function(_hW,_hX){var _hY=new T(function(){return A(_hW,[_hX]);});return [0,new T(function(){return E(E(_hY)[1]);}),new T(function(){return _2b(function(_hZ){return _hR(_hW,_hZ);},E(_hY)[2]);})];},_i0=[2],_i1=[1,_dF,_e],_i2=function(_i3){var _i4=E(_i3);if(!_i4[0]){return [0];}else{var _i5=_i4[1],_i6=new T(function(){return _i2(_i4[2]);}),_i7=function(_i8){var _i9=E(_i8);return _i9[0]==0?E(_i6):[1,[0,_i5,_i9[1]],new T(function(){return _i7(_i9[2]);})];};return (function(_ia,_ib){return [1,[0,_i5,_ia],new T(function(){return _i7(_ib);})];})(_bA,_i1);}},_ic=[0,4],_id=[1,_dF,_e],_ie=[1,_ic,_id],_if=[1,_bA,_ie],_ig=new T(function(){return _i2(_if);}),_ih=function(_ii){var _ij=function(_ik){return [1,[0,[0,_ii],[0,_ik]],new T(function(){var _il=E(_ik);return _il==7?E(new T(function(){var _im=E(_ii);return _im==7?[0]:_ih(_im+1|0);})):_ij(_il+1|0);})];};return _ij(0);},_in=new T(function(){return _ih(0);}),_io=function(_ip){var _iq=E(_in);if(!_iq[0]){return [0];}else{var _ir=E(_ip),_is=_ir[3],_it=_ir[6],_iu=_ir[7],_iv=_ir[9],_iw=E(_ir[1]),_ix=_iw[4],_iy=E(_iw[1]),_iz=E(_iw[2]),_iA=E(_iq[1]),_iB=E(_iy[1])[1],_iC=E(_iz[1])[1],_iD=E(_iA[1])[1];if(_iB>_iD){return E(_8F);}else{if(_iD>_iC){return E(_8F);}else{var _iE=E(_iy[2])[1],_iF=E(_iz[2])[1],_iG=E(_iA[2])[1];if(_iE>_iG){return E(_8F);}else{if(_iG>_iF){return E(_8F);}else{var _iH=new T(function(){var _iI=function(_iJ){var _iK=E(_iJ);if(!_iK[0]){return [0];}else{var _iL=E(_iK[1]),_iM=E(_iL[1])[1];if(_iB>_iM){return E(_8F);}else{if(_iM>_iC){return E(_8F);}else{var _iN=E(_iL[2])[1];if(_iE>_iN){return E(_8F);}else{if(_iN>_iF){return E(_8F);}else{var _iO=new T(function(){return _iI(_iK[2]);}),_iP=E(_ix[(imul(_iM-_iB|0,(_iF-_iE|0)+1|0)|0)+(_iN-_iE|0)|0]);if(!_iP[0]){return E(_iO);}else{var _iQ=new T(function(){var _iR=function(_iS){var _iT=E(_iS);return _iT[0]==0?E(_iO):[1,[0,_iL,_iT[1]],new T(function(){return _iR(_iT[2]);})];};return _iR(_fR(_iL,_iw,_is,_it,_iu,_iv));});return E(E(_iP[1])[1])==0?E(_is)==0?E(_iQ):E(_iO):E(_is)==0?E(_iO):E(_iQ);}}}}}}};return _iI(_iq[2]);}),_iU=E(_ix[(imul(_iD-_iB|0,(_iF-_iE|0)+1|0)|0)+(_iG-_iE|0)|0]);if(!_iU[0]){return E(_iH);}else{var _iV=new T(function(){var _iW=function(_iX){var _iY=E(_iX);return _iY[0]==0?E(_iH):[1,[0,_iA,_iY[1]],new T(function(){return _iW(_iY[2]);})];};return _iW(_fR(_iA,_iw,_is,_it,_iu,_iv));});return E(E(_iU[1])[1])==0?E(_is)==0?E(_iV):E(_iH):E(_is)==0?E(_iH):E(_iV);}}}}}}},_iZ=function(_j0){return [0,_j0,new T(function(){var _j1=E(_j0),_j2=_j1[3];if(E(_j1[2])==2){var _j3=function(_j4){var _j5=E(_j4);return _j5[0]==0?[0]:[1,new T(function(){var _j6=_8V(_j1[1],_j2,_j1[6],_j1[7],_j1[9],_j5[1]),_j7=_j6[1],_j8=_j6[3],_j9=_j6[4],_ja=_j6[5],_jb=_j6[6],_jc=_j6[7],_jd=_j6[8],_je=_j6[9],_jf=[0,_j7,_j6[2],_j8,_j9,_ja,_jb,_jc,_jd,_je];return !_5e(_5d,_io(_jf),_e)?E(_jf):[0,_j7,new T(function(){return !_e5(_j8,_jf)?0:1;}),_j2,_j9,_ja,_jb,_jc,_jd,_je];}),new T(function(){return _j3(_j5[2]);})];};return _j3(_io(_j1));}else{return [0];}})];},_jg=function(_jh){var _ji=_iZ(_jh);return [0,_ji[1],_ji[2]];},_jj=unCStr("Pattern match failure in do expression at chess.hs:175:3-13"),_jk=unCStr("Pattern match failure in do expression at chess.hs:176:3-17"),_jl=unCStr("Pattern match failure in do expression at chess.hs:179:3-17"),_jm=unCStr("Pattern match failure in do expression at chess.hs:182:3-14"),_jn=unCStr("Pattern match failure in do expression at chess.hs:185:3-12"),_jo=unCStr("Pattern match failure in do expression at chess.hs:188:3-15"),_jp=unCStr("Pattern match failure in do expression at chess.hs:190:3-10"),_jq=function(_jr){return A(_jr,[_e]);},_js=function(_jt,_ju){var _jv=E(_jt);if(!_jv[0]){return A(_ju,[_6T]);}else{var _jw=new T(function(){return _js(_jv[2],_ju);});return A(_jv[1],[function(_jx){return E(_jw);}]);}},_jy=function(_jz,_){while(1){var _jA=E(_jz);if(!_jA[0]){return _6T;}else{var _jB=A(_jA[1],[_]);_jz=_jA[2];continue;}}},_jC=function(_jD,_){var _jE=jsHasCtx2D(_jD);if(!E(_jE)){return _1O;}else{var _jF=jsGetCtx2D(_jD);return [1,[0,[0,_jF],[0,_jD]]];}},_jG=unCStr("height"),_jH=unCStr("width"),_jI=unCStr("canvas"),_jJ=function(_jK,_jL,_){var _jM=jsCreateElem(toJSStr(E(_jI))),_jN=[0,_jM],_jO=A(_hn,[_4u,_jN,_jH,new T(function(){var _jP=String(E(_jK)[1]);return fromJSStr(_jP);}),_]),_jQ=A(_hn,[_4u,_jN,_jG,new T(function(){var _jR=String(E(_jL)[1]);return fromJSStr(_jR);}),_]);return _jC(_jM,_);},_jS=new T(function(){return [0,"strokeStyle"];}),_jT=new T(function(){return [0,"fillStyle"];}),_jU=[0,44],_jV=[1,_jU,_e],_jW=new T(function(){return [0,toJSStr(_jV)];}),_jX=[1,_jU,_e],_jY=new T(function(){return [0,toJSStr(_jX)];}),_jZ=new T(function(){return [0,"rgba("];}),_k0=new T(function(){return [0,toJSStr(_e)];}),_k1=[0,41],_k2=[1,_k1,_e],_k3=new T(function(){return [0,toJSStr(_k2)];}),_k4=[1,_k3,_e],_k5=[1,_jU,_e],_k6=new T(function(){return [0,toJSStr(_k5)];}),_k7=[1,_jU,_e],_k8=new T(function(){return [0,toJSStr(_k7)];}),_k9=new T(function(){return [0,"rgb("];}),_ka=[1,_k1,_e],_kb=new T(function(){return [0,toJSStr(_ka)];}),_kc=[1,_kb,_e],_kd=[1,_jU,_e],_ke=new T(function(){return [0,toJSStr(_kd)];}),_kf=function(_kg){var _kh=String(E(_kg)[1]);return [0,_kh];},_ki=function(_kj){var _kk=E(_kj);if(!_kk[0]){var _kl=jsCat([1,_k9,[1,new T(function(){return _kf(_kk[1]);}),[1,_k8,[1,new T(function(){return _kf(_kk[2]);}),[1,_k6,[1,new T(function(){return _kf(_kk[3]);}),_k4]]]]]],E(_k0)[1]);return [0,_kl];}else{var _km=jsCat([1,_jZ,[1,new T(function(){return _kf(_kk[1]);}),[1,_jY,[1,new T(function(){return _kf(_kk[2]);}),[1,_jW,[1,new T(function(){return _kf(_kk[3]);}),[1,_ke,[1,new T(function(){return _kf(_kk[4]);}),_kc]]]]]]]],E(_k0)[1]);return [0,_km];}},_kn=function(_ko,_kp){var _kq=new T(function(){return _ki(_ko);});return function(_kr,_){var _ks=E(_kr),_kt=_ks[1],_ku=E(_jT)[1],_kv=jsGet(_kt,_ku),_kw=E(_jS)[1],_kx=jsGet(_kt,_kw),_ky=E(_kq)[1],_kz=jsSet(_kt,_ku,_ky),_kA=jsSet(_kt,_kw,_ky),_kB=A(_kp,[_ks,_]),_kC=jsSet(_kt,_ku,_kv),_kD=jsSet(_kt,_kw,_kx);return _6T;};},_kE=function(_kF){return [2];},_kG=function(_kH,_){while(1){var _kI=E(_kH);if(!_kI[0]){return _6T;}else{var _kJ=_kI[2],_kK=E(_kI[1]);switch(_kK[0]){case 0:var _kL=A(_kK[1],[_]);_kH=_E(_kJ,[1,_kL,_e]);continue;case 1:_kH=_E(_kJ,_kK[1]);continue;default:_kH=_kJ;continue;}}}},_kM=new T(function(){return [0,"(function(){return md51(jsRand().toString());})"];}),_kN=function(_){return A(_6l,[_kM,_]);},_kO=function(_){return _kN(_);},_kP=function(_kQ,_kR){return _kQ<=0?_kQ>=0?quot(_kQ,_kR):_kR<=0?quot(_kQ,_kR):quot(_kQ+1|0,_kR)-1|0:_kR>=0?_kQ>=0?quot(_kQ,_kR):_kR<=0?quot(_kQ,_kR):quot(_kQ+1|0,_kR)-1|0:quot(_kQ-1|0,_kR)-1|0;},_kS=function(_kT,_kU){while(1){var _kV=E(_kT);if(!_kV[0]){return E(_kU)[0]==0?true:false;}else{var _kW=E(_kU);if(!_kW[0]){return false;}else{if(E(_kV[1])[1]!=E(_kW[1])[1]){return false;}else{_kT=_kV[2];_kU=_kW[2];continue;}}}}},_kX=new T(function(){return [0,"keydown"];}),_kY=new T(function(){return [0,"mousedown"];}),_kZ=function(_l0,_l1,_){var _l2=E(_l1),_l3=_l2[1],_l4=jsBeginPath(_l3),_l5=A(_l0,[_l2,_]),_l6=jsFill(_l3);return _6T;},_l7=function(_l8){return _kE(_l8);},_l9=[0,_bA,_bA],_la=[0,_dF,_dF],_lb=0,_lc=[0,_8u,_lb],_ld=[1,_lc],_le=[0,_8I,_lb],_lf=[1,_le],_lg=3,_lh=1,_li=2,_lj=5,_lk=[1,_lg,_e],_ll=[1,_lh,_lk],_lm=[1,_li,_ll],_ln=[1,_lj,_lm],_lo=[1,_8J,_ln],_lp=[1,_li,_lo],_lq=[1,_lh,_lp],_lr=[1,_lg,_lq],_ls=function(_){var _lt=newArr(64,_8w),_lu=function(_lv,_){var _lw=new T(function(){return _lv>=0?_2v(_lr,_lv):E(_2s);}),_lx=[1,[0,_8I,new T(function(){return _lv>=0?_2v(_lr,_lv):E(_2s);})]];if(0>_lv){return E(_8F);}else{if(_lv>7){return E(_8F);}else{var _=_lt[imul(_lv,8)|0]=_lx;return (function(_ly,_){while(1){var _lz=(function(_lA,_){if(0>_lA){return E(_8F);}else{if(_lA>7){return E(_8F);}else{var _=_lt[(imul(_lv,8)|0)+_lA|0]=new T(function(){switch(E(_lA)){case 0:return E(_lx);case 1:return E(_lf);case 6:return E(_ld);case 7:return E([1,[0,_8u,_lw]]);default:return [0];}}),_lB=E(_lA);if(_lB==7){var _lC=E(_lv);if(_lC==7){var _lD=_lt;return [0,E(_l9),E(_la),64,_lD];}else{return _lu(_lC+1|0,_);}}else{_ly=_lB+1|0;return null;}}}})(_ly,_);if(_lz!=null){return _lz;}}})(1,_);}}};return _lu(0,_);},_lE=new T(function(){return _8S(_ls);}),_lF=unCStr("Prelude.undefined"),_lG=new T(function(){return err(_lF);}),_lH=[0,_lE,_8t,_8u,_1O,_1O,_ig,_1O,_lG,_8J],_lI=function(_lJ,_lK){while(1){var _lL=E(_lK);if(!_lL[0]){return E(_lJ);}else{_lJ=_lL[1];_lK=_lL[2];continue;}}},_lM=unCStr("last"),_lN=new T(function(){return _dN(_lM);}),_lO=new T(function(){return _7R("chess.hs:(174,59)-(269,41)|lambda");}),_lP=[0,255],_lQ=[0,_lP,_lP,_lP],_lR=function(_lS,_){var _lT=E(_lS)[1],_lU=jsBeginPath(_lT),_lV=jsMoveTo(_lT,30,20),_lW=jsArc(_lT,20,20,10,0,6.283185307179586),_lX=jsFill(_lT);return _6T;},_lY=new T(function(){return _kn(_lQ,_lR);}),_lZ=[0,_bA,_bA,_bA],_m0=function(_m1,_){var _m2=E(_m1)[1],_m3=jsBeginPath(_m2),_m4=jsMoveTo(_m2,31,20),_m5=jsArc(_m2,20,20,11,0,6.283185307179586),_m6=jsStroke(_m2);return _6T;},_m7=new T(function(){return _kn(_lZ,_m0);}),_m8=[0,_bA,_bA,_bA],_m9=function(_ma,_){var _mb=E(_ma)[1],_mc=jsBeginPath(_mb),_md=jsMoveTo(_mb,31,20),_me=jsArc(_mb,20,20,11,0,6.283185307179586),_mf=jsFill(_mb);return _6T;},_mg=new T(function(){return _kn(_m8,_m9);}),_mh=function(_mi,_mj,_){while(1){var _mk=E(_mi);if(!_mk[0]){return _6T;}else{var _ml=A(_mk[1],[_mj,_]);_mi=_mk[2];continue;}}},_mm=function(_mn,_){return _6T;},_mo=function(_mp){var _mq=E(_mp);if(!_mq[0]){return E(_mm);}else{var _mr=E(_mq[1]);return function(_ms,_){var _mt=E(_ms)[1],_mu=jsMoveTo(_mt,E(_mr[1])[1],E(_mr[2])[1]);return (function(_mv,_){while(1){var _mw=E(_mv);if(!_mw[0]){return _6T;}else{var _mx=E(_mw[1]),_my=jsLineTo(_mt,E(_mx[1])[1],E(_mx[2])[1]);_mv=_mw[2];continue;}}})(_mq[2],_);};}},_mz=function(_mA,_mB,_mC,_mD){return _mo([1,[0,_mA,_mB],[1,[0,_mC,_mB],[1,[0,_mC,_mD],[1,[0,_mA,_mD],[1,[0,_mA,_mB],_e]]]]]);},_mE=[0,40],_mF=new T(function(){return _mz(_bA,_bA,_8G,_mE);}),_mG=function(_mH,_){var _mI=E(_mH),_mJ=_mI[1],_mK=jsBeginPath(_mJ),_mL=A(_mF,[_mI,_]),_mM=jsFill(_mJ);return _6T;},_mN=new T(function(){return _mz(_bA,_bA,_mE,_8G);}),_mO=function(_mP,_){var _mQ=E(_mP),_mR=_mQ[1],_mS=jsBeginPath(_mR),_mT=A(_mN,[_mQ,_]),_mU=jsFill(_mR);return _6T;},_mV=[0,35],_mW=[0,75],_mX=new T(function(){return _mz(_mV,_bA,_mW,_mE);}),_mY=function(_mZ,_){var _n0=E(_mZ),_n1=_n0[1],_n2=jsBeginPath(_n1),_n3=A(_mX,[_n0,_]),_n4=jsFill(_n1);return _6T;},_n5=new T(function(){return _mz(_bA,_mV,_mE,_mW);}),_n6=function(_n7,_){var _n8=E(_n7),_n9=_n8[1],_na=jsBeginPath(_n9),_nb=A(_n5,[_n8,_]),_nc=jsFill(_n9);return _6T;},_nd=[1,_n6,_e],_ne=[1,_mY,_nd],_nf=[1,_mO,_ne],_ng=[1,_mG,_nf],_nh=function(_ni,_){return _mh(_ng,_ni,_);},_nj=[0,127],_nk=[0,15],_nl=[0,_nj,_nk,_nk],_nm=new T(function(){return _kn(_nl,_nh);}),_nn=[0,191],_no=[0,0.3],_np=[1,_bA,_nn,_bA,_no],_nq=new T(function(){return _mz(_bA,_bA,_mE,_mE);}),_nr=function(_ni,_){return _kZ(_nq,_ni,_);},_ns=new T(function(){return _kn(_np,_nr);}),_nt=[0,320],_nu=[0,_nn,_nn,_nn],_nv=[0,_lP,_lP,_lP],_nw=function(_nx){var _ny=function(_nz){return [1,[0,[0,_nx],[0,_nz]],new T(function(){var _nA=E(_nz);return _nA==7?E(new T(function(){var _nB=E(_nx);return _nB==7?[0]:_nw(_nB+1|0);})):_ny(_nA+1|0);})];};return _ny(0);},_nC=new T(function(){return _nw(0);}),_nD=new T(function(){return _bC("chess.hs:212:11-34|(a, b : bs)");}),_nE=[0,0],_nF=function(_nG){var _nH=function(_nI){return [1,[0,[0,_nG],[0,_nI]],new T(function(){var _nJ=E(_nI);return _nJ==7?E(new T(function(){var _nK=E(_nG);return _nK==7?[0]:_nF(_nK+1|0);})):_nH(_nJ+1|0);})];};return _nH(0);},_nL=new T(function(){return _nF(0);}),_nM=unCStr("innerHTML"),_nN=unCStr(" to move"),_nO=function(_nP){var _nQ=function(_nR){return [1,[0,[0,_nP],[0,_nR]],new T(function(){var _nS=E(_nR);return _nS==7?E(new T(function(){var _nT=E(_nP);return _nT==7?[0]:_nO(_nT+1|0);})):_nQ(_nS+1|0);})];};return _nQ(0);},_nU=new T(function(){return _nO(0);}),_nV=unCStr(" draws"),_nW=unCStr(" wins"),_nX=[0,3],_nY=unCStr("value"),_nZ=function(_o0){return A(_o0,[_6T]);},_o1=new T(function(){return _E(_5x,_nN);}),_o2=function(_o3){return E(E(_o3)[2]);},_o4=function(_o5){var _o6=E(_o5);if(!_o6[0]){return [0];}else{var _o7=_o6[1];return [1,[0,new T(function(){return E(E(_o7)[1]);}),new T(function(){return _2b(_o2,_o8(_o7));})],new T(function(){return _o4(_o6[2]);})];}},_o9=function(_oa,_ob){return [1,[0,new T(function(){return E(E(_oa)[1]);}),new T(function(){return _2b(_o2,_o8(_oa));})],new T(function(){return _o4(_ob);})];},_oc=function(_od,_oe){while(1){var _of=E(_od);if(!_of[0]){return E(_oe);}else{_od=_of[2];var _og=_oe+E(_of[1])[1]|0;_oe=_og;continue;}}},_oh=function(_oi){var _oj=function(_ok){return [1,[0,[0,_oi],[0,_ok]],new T(function(){var _ol=E(_ok);return _ol==7?E(new T(function(){var _om=E(_oi);return _om==7?[0]:_oh(_om+1|0);})):_oj(_ol+1|0);})];};return _oj(0);},_on=new T(function(){return _oh(0);}),_oo=unCStr("Negative exponent"),_op=new T(function(){return err(_oo);}),_oq=[0,0],_or=[0,2],_os=[0,2],_ot=function(_ou,_ov){while(1){var _ow=E(_ou);if(!_ow[0]){var _ox=E(_ow[1]);if(_ox==(-2147483648)){_ou=[1,I_fromInt(-2147483648)];continue;}else{var _oy=E(_ov);if(!_oy[0]){return [0,quot(_ox,_oy[1])];}else{_ou=[1,I_fromInt(_ox)];_ov=_oy;continue;}}}else{var _oz=_ow[1],_oA=E(_ov);return _oA[0]==0?[0,I_toInt(I_quot(_oz,I_fromInt(_oA[1])))]:[1,I_quot(_oz,_oA[1])];}}},_oB=function(_oC,_oD){while(1){var _oE=E(_oC);if(!_oE[0]){var _oF=E(_oE[1]);if(_oF==(-2147483648)){_oC=[1,I_fromInt(-2147483648)];continue;}else{var _oG=E(_oD);if(!_oG[0]){return [0,_oF%_oG[1]];}else{_oC=[1,I_fromInt(_oF)];_oD=_oG;continue;}}}else{var _oH=_oE[1],_oI=E(_oD);return _oI[0]==0?[0,I_toInt(I_rem(_oH,I_fromInt(_oI[1])))]:[1,I_rem(_oH,_oI[1])];}}},_oJ=function(_oK,_oL,_oM){while(1){if(!_gY(_oB(_oL,_or),_oq)){if(!_gY(_oL,_h6)){var _oN=imul(_oK,_oK)|0,_oO=_ot(_h7(_oL,_h6),_os),_oP=imul(_oK,_oM)|0;_oK=_oN;_oL=_oO;_oM=_oP;continue;}else{return imul(_oK,_oM)|0;}}else{var _oN=imul(_oK,_oK)|0,_oO=_ot(_oL,_os);_oK=_oN;_oL=_oO;continue;}}},_oQ=function(_oR,_oS){while(1){if(!_gY(_oB(_oS,_or),_oq)){return !_gY(_oS,_h6)?_oJ(imul(_oR,_oR)|0,_ot(_h7(_oS,_h6),_os),_oR):E(_oR);}else{var _oT=imul(_oR,_oR)|0,_oU=_ot(_oS,_os);_oR=_oT;_oS=_oU;continue;}}},_oV=function(_oW,_oX){var _oY=E(_oW);if(!_oY[0]){var _oZ=_oY[1],_p0=E(_oX);return _p0[0]==0?_oZ<_p0[1]:I_compareInt(_p0[1],_oZ)>0;}else{var _p1=_oY[1],_p2=E(_oX);return _p2[0]==0?I_compareInt(_p1,_p2[1])<0:I_compare(_p1,_p2[1])<0;}},_p3=function(_p4,_p5){return !_oV(_p5,_gX)?!_gY(_p5,_gX)?_oQ(E(_p4)[1],_p5):1:E(_op);},_p6=[0,16],_p7=new T(function(){return [0, -_p3(_fM,_p6)];}),_p8=new T(function(){return [0,_p3(_fM,_p6)];}),_p9=function(_pa,_pb,_pc){switch(E(_pb)){case 0:return E(_bA);case 1:return E(_pc)==0?E(_p7):E(_p8);default:var _pd=E(_on);if(!_pd[0]){return [0,_oc(_e,0)];}else{var _pe=_pd[2],_pf=E(_pa),_pg=_pf[4],_ph=E(_pf[1]),_pi=E(_pf[2]),_pj=E(_pd[1]),_pk=E(_ph[1])[1],_pl=E(_pi[1])[1],_pm=E(_pj[1])[1];if(_pk>_pm){return E(_8F);}else{if(_pm>_pl){return E(_8F);}else{var _pn=E(_ph[2])[1],_po=E(_pi[2])[1],_pp=E(_pj[2])[1];if(_pn>_pp){return E(_8F);}else{if(_pp>_po){return E(_8F);}else{var _pq=E(_pg[(imul(_pm-_pk|0,(_po-_pn|0)+1|0)|0)+(_pp-_pn|0)|0]);if(!_pq[0]){var _pr=function(_ps){while(1){var _pt=(function(_pu){var _pv=E(_pu);if(!_pv[0]){return [0];}else{var _pw=_pv[2],_px=E(_pv[1]),_py=E(_px[1])[1];if(_pk>_py){return E(_8F);}else{if(_py>_pl){return E(_8F);}else{var _pz=E(_px[2])[1];if(_pn>_pz){return E(_8F);}else{if(_pz>_po){return E(_8F);}else{var _pA=E(_pg[(imul(_py-_pk|0,(_po-_pn|0)+1|0)|0)+(_pz-_pn|0)|0]);if(!_pA[0]){_ps=_pw;return null;}else{return [1,new T(function(){var _pB=E(_pA[1]),_pC=_pB[2];if(!E(_pB[1])){switch(E(_pC)){case 0:return [0,-100];case 1:return [0,-300];case 2:return [0,-350];case 3:return [0,-500];case 4:return [0,-1000];default:return [0,0];}}else{switch(E(_pC)){case 0:return [0,100];case 1:return [0,300];case 2:return [0,350];case 3:return [0,500];case 4:return [0,1000];default:return [0,0];}}}),new T(function(){return _pr(_pw);})];}}}}}}})(_ps);if(_pt!=null){return _pt;}}};return [0,_oc(_pr(_pe),0)];}else{return [0,_oc([1,new T(function(){var _pD=E(_pq[1]),_pE=_pD[2];if(!E(_pD[1])){switch(E(_pE)){case 0:return [0,-100];case 1:return [0,-300];case 2:return [0,-350];case 3:return [0,-500];case 4:return [0,-1000];default:return [0,0];}}else{switch(E(_pE)){case 0:return [0,100];case 1:return [0,300];case 2:return [0,350];case 3:return [0,500];case 4:return [0,1000];default:return [0,0];}}}),new T(function(){var _pF=function(_pG){while(1){var _pH=(function(_pI){var _pJ=E(_pI);if(!_pJ[0]){return [0];}else{var _pK=_pJ[2],_pL=E(_pJ[1]),_pM=E(_pL[1])[1];if(_pk>_pM){return E(_8F);}else{if(_pM>_pl){return E(_8F);}else{var _pN=E(_pL[2])[1];if(_pn>_pN){return E(_8F);}else{if(_pN>_po){return E(_8F);}else{var _pO=E(_pg[(imul(_pM-_pk|0,(_po-_pn|0)+1|0)|0)+(_pN-_pn|0)|0]);if(!_pO[0]){_pG=_pK;return null;}else{return [1,new T(function(){var _pP=E(_pO[1]),_pQ=_pP[2];if(!E(_pP[1])){switch(E(_pQ)){case 0:return [0,-100];case 1:return [0,-300];case 2:return [0,-350];case 3:return [0,-500];case 4:return [0,-1000];default:return [0,0];}}else{switch(E(_pQ)){case 0:return [0,100];case 1:return [0,300];case 2:return [0,350];case 3:return [0,500];case 4:return [0,1000];default:return [0,0];}}}),new T(function(){return _pF(_pK);})];}}}}}}})(_pG);if(_pH!=null){return _pH;}}};return _pF(_pe);})],0)];}}}}}}}},_pR=function(_pS){var _pT=E(_pS);return _p9(_pT[1],_pT[2],_pT[3]);},_pU=function(_pV,_pW){return E(_pV)[1]>=E(_pW)[1];},_pX=function(_pY){var _pZ=E(_pY);return _pZ[0]==0?E(_lN):_lI(_pZ[1],_pZ[2]);},_q0=new T(function(){return _7R("chess.hs:(62,1)-(66,38)|function omitWith");}),_q1=function(_q2,_q3){var _q4=E(_q3);if(!_q4[0]){return E(_q0);}else{var _q5=E(_q4[1]),_q6=_q5[2];return [1,[0,_q5[1],new T(function(){return _pX(_q6);})],new T(function(){var _q7=function(_q8,_q9){while(1){var _qa=(function(_qb,_qc){var _qd=E(_qc);if(!_qd[0]){return [0];}else{var _qe=_qd[2],_qf=E(_qd[1]),_qg=_qf[2];if(!(function(_qh){while(1){var _qi=E(_qh);if(!_qi[0]){return false;}else{if(!A(_q2,[_qi[1],_qb])){_qh=_qi[2];continue;}else{return true;}}}})(_qg)){return [1,[0,_qf[1],new T(function(){return _pX(_qg);})],new T(function(){return _q7(new T(function(){return _pX(_qg);}),_qe);})];}else{var _qj=_qb;_q9=_qe;_q8=_qj;return null;}}})(_q8,_q9);if(_qa!=null){return _qa;}}};return _q7(new T(function(){return _pX(_q6);}),_q4[2]);})];}},_qk=function(_ql){var _qm=E(_ql);if(!_qm[0]){return [0];}else{var _qn=_qm[1];return [1,[0,new T(function(){return E(E(_qn)[1]);}),new T(function(){var _qo=E(_qn),_qp=E(_qo[2]);return _qp[0]==0?[1,new T(function(){return _pR(_qo[1]);}),_e]:_2b(_o2,_q1(_pU,_o9(_qp[1],_qp[2])));})],new T(function(){return _qk(_qm[2]);})];}},_qq=function(_qr,_qs){return [1,[0,new T(function(){return E(E(_qr)[1]);}),new T(function(){var _qt=E(_qr),_qu=E(_qt[2]);return _qu[0]==0?[1,new T(function(){return _pR(_qt[1]);}),_e]:_2b(_o2,_q1(_pU,_o9(_qu[1],_qu[2])));})],new T(function(){return _qk(_qs);})];},_qv=function(_qw,_qx){return E(_qw)[1]<=E(_qx)[1];},_o8=function(_qy){var _qz=E(_qy),_qA=E(_qz[2]);return _qA[0]==0?[1,[0,_lG,new T(function(){return _pR(_qz[1]);})],_e]:_q1(_qv,_qq(_qA[1],_qA[2]));},_qB=function(_qC,_qD,_qE){return [0,function(_){var _qF=E(_qC)[1],_qG=rMV(_qF),_qH=E(_qG);if(!_qH[0]){var _=wMV(_qF,[0,_qH[1],new T(function(){var _qI=new T(function(){return A(_qE,[_6T]);});return _E(_qH[2],[1,[0,_qD,function(_qJ){return E(_qI);}],_e]);})]);return _i0;}else{var _qK=E(_qH[1]);if(!_qK[0]){var _=wMV(_qF,[0,_qD,_e]);return new T(function(){return A(_qE,[_6T]);});}else{var _=wMV(_qF,[1,_qK[2]]);return [1,[1,new T(function(){return A(_qE,[_6T]);}),[1,new T(function(){return A(_qK[1],[_qD,_l7]);}),_e]]];}}}];},_qL=[1,_e],_qM=function(_qN,_qO){return [0,function(_){var _qP=E(_qN)[1],_qQ=rMV(_qP),_qR=E(_qQ);if(!_qR[0]){var _qS=_qR[1],_qT=E(_qR[2]);if(!_qT[0]){var _=wMV(_qP,_qL);return new T(function(){return A(_qO,[_qS]);});}else{var _qU=E(_qT[1]),_=wMV(_qP,[0,_qU[1],_qT[2]]);return [1,[1,new T(function(){return A(_qO,[_qS]);}),[1,new T(function(){return A(_qU[2],[_l7]);}),_e]]];}}else{var _=wMV(_qP,[1,new T(function(){return _E(_qR[1],[1,function(_qV){var _qW=new T(function(){return A(_qO,[_qV]);});return function(_qX){return E(_qW);};},_e]);})]);return _i0;}}];},_qY=function(_qZ,_){var _r0=E(_qZ);if(!_r0[0]){return E(_lO);}else{var _r1=E(_r0[2]);if(!_r1[0]){return E(_lO);}else{var _r2=E(_r1[2]);if(!_r2[0]){return E(_lO);}else{var _r3=_r2[1],_r4=E(_r2[2]);if(!_r4[0]){return E(_lO);}else{if(!E(_r4[2])[0]){var _r5=E(_r1[1])[1],_r6=jsHasCtx2D(_r5);if(!E(_r6)){return _1U(_jj,_);}else{var _r7=jsGetCtx2D(_r5),_r8=_jJ(_mE,_mE,_),_r9=E(_r8);if(!_r9[0]){return _1U(_jk,_);}else{var _ra=E(_r9[1])[1],_rb=A(_lY,[_ra,_]),_rc=A(_m7,[_ra,_]),_rd=_jJ(_mE,_mE,_),_re=E(_rd);if(!_re[0]){return _1U(_jl,_);}else{var _rf=A(_mg,[E(_re[1])[1],_]),_rg=_jJ(_mE,_mE,_),_rh=E(_rg);if(!_rh[0]){return _1U(_jm,_);}else{var _ri=E(_rh[1]),_rj=E(_ri[2])[1],_rk=jsResetCanvas(_rj),_rl=A(_nm,[_ri[1],_]),_rm=_jJ(_mE,_mE,_),_rn=E(_rm);if(!_rn[0]){return _1U(_jn,_);}else{var _ro=E(_rn[1]),_rp=E(_ro[2])[1],_rq=jsResetCanvas(_rp),_rr=A(_ns,[_ro[1],_]),_rs=_jJ(_nt,_nt,_),_rt=E(_rs);if(!_rt[0]){return _1U(_jo,_);}else{var _ru=_rt[1],_rv=function(_){var _rw=_jJ(_nt,_nt,_),_rx=E(_rw);if(!_rx[0]){return _1U(_jp,_);}else{var _ry=_rx[1],_rz=nMV(_qL),_rA=[0,_rz],_rB=jsSetCB(_r5,E(_kY)[1],function(_rC,_rD,_){var _rE=E(_rD);return _kG([1,new T(function(){return _qB(_rA,[1,_rE[1],_rE[2]],_kE);}),_e],_);}),_rF=jsSetCB(E(_r0[1])[1],E(_kX)[1],function(_rG,_){return _kG([1,new T(function(){return _qB(_rA,[0,_rG],_kE);}),_e],_);}),_rH=_kO(_),_rI=nMV([0,_rH,_e]),_rJ=_jy([1,function(_){var _rK=E(_ry),_rL=jsResetCanvas(E(_rK[2])[1]),_rM=jsDrawImage(E(_rK[1])[1],E(E(_ru)[2])[1],0,0);return _6T;},new T(function(){var _rN=E(_nL);if(!_rN[0]){return [0];}else{var _rO=_rN[2],_rP=E(_rN[1]),_rQ=E(_lE),_rR=_rQ[4],_rS=E(_rQ[1]),_rT=E(_rQ[2]),_rU=E(_rS[1])[1],_rV=E(_rT[1])[1],_rW=E(_rP[1])[1];if(_rU>_rW){return E(_8F);}else{if(_rW>_rV){return E(_8F);}else{var _rX=E(_rS[2])[1],_rY=E(_rT[2])[1],_rZ=E(_rP[2])[1];if(_rX>_rZ){return E(_8F);}else{if(_rZ>_rY){return E(_8F);}else{var _s0=E(_rR[(imul(_rW-_rU|0,(_rY-_rX|0)+1|0)|0)+(_rZ-_rX|0)|0]);if(!_s0[0]){var _s1=function(_s2){while(1){var _s3=(function(_s4){var _s5=E(_s4);if(!_s5[0]){return [0];}else{var _s6=_s5[2],_s7=E(_s5[1]),_s8=E(_s7[1])[1];if(_rU>_s8){return E(_8F);}else{if(_s8>_rV){return E(_8F);}else{var _s9=E(_s7[2])[1];if(_rX>_s9){return E(_8F);}else{if(_s9>_rY){return E(_8F);}else{var _sa=E(_rR[(imul(_s8-_rU|0,(_rY-_rX|0)+1|0)|0)+(_s9-_rX|0)|0]);if(!_sa[0]){_s2=_s6;return null;}else{return [1,new T(function(){var _sb=E(_ry)[1];return function(_){var _sc=E(_sa[1]),_sd=_sc[1],_se=_sc[2],_sf=function(_sg){var _sh=(imul(_s9,40)|0)+20|0;if(!E(_sd)){return _8i(_sg,_sh,_hL(_nE,_nE,new T(function(){return _5u(_se);})),_sb,_);}else{var _si=function(_sj){return _8i(_sg,_sh,function(_sk,_){return _6U(3.141592653589793,function(_sl,_){var _sm=jsDrawText(E(_sl)[1],_sj,0,0);return _6T;},_sk,_);},_sb,_);};switch(E(_se)){case 0:return _si(toJSStr(E(_5p)));case 1:return _si(toJSStr(E(_5o)));case 2:return _si(toJSStr(E(_5t)));case 3:return _si(toJSStr(E(_5s)));case 4:return _si(toJSStr(E(_5r)));default:return _si(toJSStr(E(_5q)));}}};return E(_sd)==0?_sf((imul(_s8,40)|0)+5|0):_sf((imul(_s8,40)|0)+35|0);};}),new T(function(){return _s1(_s6);})];}}}}}}})(_s2);if(_s3!=null){return _s3;}}};return _s1(_rO);}else{return [1,new T(function(){var _sn=E(_ry)[1];return function(_){var _so=E(_s0[1]),_sp=_so[1],_sq=_so[2],_sr=function(_ss){var _st=(imul(_rZ,40)|0)+20|0;if(!E(_sp)){return _8i(_ss,_st,_hL(_nE,_nE,new T(function(){return _5u(_sq);})),_sn,_);}else{var _su=function(_sv){return _8i(_ss,_st,function(_sw,_){return _6U(3.141592653589793,function(_sx,_){var _sy=jsDrawText(E(_sx)[1],_sv,0,0);return _6T;},_sw,_);},_sn,_);};switch(E(_sq)){case 0:return _su(toJSStr(E(_5p)));case 1:return _su(toJSStr(E(_5o)));case 2:return _su(toJSStr(E(_5t)));case 3:return _su(toJSStr(E(_5s)));case 4:return _su(toJSStr(E(_5r)));default:return _su(toJSStr(E(_5q)));}}};return E(_sp)==0?_sr((imul(_rW,40)|0)+5|0):_sr((imul(_rW,40)|0)+35|0);};}),new T(function(){var _sz=function(_sA){while(1){var _sB=(function(_sC){var _sD=E(_sC);if(!_sD[0]){return [0];}else{var _sE=_sD[2],_sF=E(_sD[1]),_sG=E(_sF[1])[1];if(_rU>_sG){return E(_8F);}else{if(_sG>_rV){return E(_8F);}else{var _sH=E(_sF[2])[1];if(_rX>_sH){return E(_8F);}else{if(_sH>_rY){return E(_8F);}else{var _sI=E(_rR[(imul(_sG-_rU|0,(_rY-_rX|0)+1|0)|0)+(_sH-_rX|0)|0]);if(!_sI[0]){_sA=_sE;return null;}else{return [1,new T(function(){var _sJ=E(_ry)[1];return function(_){var _sK=E(_sI[1]),_sL=_sK[1],_sM=_sK[2],_sN=function(_sO){var _sP=(imul(_sH,40)|0)+20|0;if(!E(_sL)){return _8i(_sO,_sP,_hL(_nE,_nE,new T(function(){return _5u(_sM);})),_sJ,_);}else{var _sQ=function(_sR){return _8i(_sO,_sP,function(_sS,_){return _6U(3.141592653589793,function(_sT,_){var _sU=jsDrawText(E(_sT)[1],_sR,0,0);return _6T;},_sS,_);},_sJ,_);};switch(E(_sM)){case 0:return _sQ(toJSStr(E(_5p)));case 1:return _sQ(toJSStr(E(_5o)));case 2:return _sQ(toJSStr(E(_5t)));case 3:return _sQ(toJSStr(E(_5s)));case 4:return _sQ(toJSStr(E(_5r)));default:return _sQ(toJSStr(E(_5q)));}}};return E(_sL)==0?_sN((imul(_sG,40)|0)+5|0):_sN((imul(_sG,40)|0)+35|0);};}),new T(function(){return _sz(_sE);})];}}}}}}})(_sA);if(_sB!=null){return _sB;}}};return _sz(_rO);})];}}}}}}})],_),_sV=jsResetCanvas(_r5),_sW=E(_ry),_sX=_sW[1],_sY=E(_sW[2])[1],_sZ=jsDrawImage(_r7,_sY,0,0),_t0=A(_hn,[_4u,_r3,_nM,_o1,_]);return _kG([1,[1,[1,_i0,[1,new T(function(){var _t1=[0,_rI],_t2=function(_t3){var _t4=E(_t3);if(!_t4[0]){return E(_jq);}else{var _t5=new T(function(){return [0,_gS(_t4,0)-1|0];}),_t6=new T(function(){return _E(_e,_t4[2]);});return function(_t7){var _t8=new T(function(){return A(_t2,[_t6,function(_t9){return A(_t7,[[1,_t4[1],_t9]]);}]);});return _qM(_t1,function(_ta){var _tb=new T(function(){var _tc=_6H(0,E(_t5)[1],_ta);return [0,_tc[1],_tc[2]];});return _qB(_t1,new T(function(){return E(E(_tb)[2]);}),function(_td){var _te=E(E(_tb)[1])[1];if(_te>=0){var _tf=_ht(_te,_t4),_tg=E(_tf[2]);return _tg[0]==0?E(_nD):A(_t2,[_E(_tf[1],_tg[2]),function(_th){return A(_t7,[[1,_tg[1],_th]]);}]);}else{return E(_t8);}});});};}},_ti=function(_tj){var _tk=E(_tj);if(!_tk[0]){return E(_nZ);}else{var _tl=E(_tk[1]),_tm=new T(function(){return [0,imul(E(_tl[2])[1],40)|0];}),_tn=new T(function(){return [0,imul(E(_tl[1])[1],40)|0];}),_to=new T(function(){return _ti(_tk[2]);});return function(_tp){return [0,function(_){var _tq=jsDrawImage(_r7,_rp,E(_tn)[1],E(_tm)[1]);return new T(function(){return A(_to,[_tp]);});}];};}},_tr=function(_ts){var _tt=new T(function(){return _hn(_5k,_r3,_nM,new T(function(){var _tu=E(_ts),_tv=_tu[2];return E(_tu[3])==0?_E(_5x,new T(function(){switch(E(_tv)){case 0:return E(_nV);case 1:return E(_nW);default:return E(_nN);}})):_E(_5w,new T(function(){switch(E(_tv)){case 0:return E(_nV);case 1:return E(_nW);default:return E(_nN);}}));}));});return function(_tw){return _js([1,function(_tx){return [0,function(_){var _ty=jsResetCanvas(_sY),_tz=jsDrawImage(E(_sX)[1],E(E(_ru)[2])[1],0,0);return new T(function(){return A(_tx,[_6T]);});}];},new T(function(){var _tA=E(_nU);if(!_tA[0]){return [0];}else{var _tB=_tA[2],_tC=E(_tA[1]),_tD=E(E(_ts)[1]),_tE=_tD[4],_tF=E(_tD[1]),_tG=E(_tD[2]),_tH=E(_tF[1])[1],_tI=E(_tG[1])[1],_tJ=E(_tC[1])[1];if(_tH>_tJ){return E(_8F);}else{if(_tJ>_tI){return E(_8F);}else{var _tK=E(_tF[2])[1],_tL=E(_tG[2])[1],_tM=E(_tC[2])[1];if(_tK>_tM){return E(_8F);}else{if(_tM>_tL){return E(_8F);}else{var _tN=E(_tE[(imul(_tJ-_tH|0,(_tL-_tK|0)+1|0)|0)+(_tM-_tK|0)|0]);if(!_tN[0]){var _tO=function(_tP){while(1){var _tQ=(function(_tR){var _tS=E(_tR);if(!_tS[0]){return [0];}else{var _tT=_tS[2],_tU=E(_tS[1]),_tV=E(_tU[1])[1];if(_tH>_tV){return E(_8F);}else{if(_tV>_tI){return E(_8F);}else{var _tW=E(_tU[2])[1];if(_tK>_tW){return E(_8F);}else{if(_tW>_tL){return E(_8F);}else{var _tX=E(_tE[(imul(_tV-_tH|0,(_tL-_tK|0)+1|0)|0)+(_tW-_tK|0)|0]);if(!_tX[0]){_tP=_tT;return null;}else{return [1,new T(function(){return _7U(_sX,_tX,[0,imul(_tV,40)|0],[0,imul(_tW,40)|0]);}),new T(function(){return _tO(_tT);})];}}}}}}})(_tP);if(_tQ!=null){return _tQ;}}};return _tO(_tB);}else{return [1,new T(function(){return _7U(_sX,_tN,[0,imul(_tJ,40)|0],[0,imul(_tM,40)|0]);}),new T(function(){var _tY=function(_tZ){while(1){var _u0=(function(_u1){var _u2=E(_u1);if(!_u2[0]){return [0];}else{var _u3=_u2[2],_u4=E(_u2[1]),_u5=E(_u4[1])[1];if(_tH>_u5){return E(_8F);}else{if(_u5>_tI){return E(_8F);}else{var _u6=E(_u4[2])[1];if(_tK>_u6){return E(_8F);}else{if(_u6>_tL){return E(_8F);}else{var _u7=E(_tE[(imul(_u5-_tH|0,(_tL-_tK|0)+1|0)|0)+(_u6-_tK|0)|0]);if(!_u7[0]){_tZ=_u3;return null;}else{return [1,new T(function(){return _7U(_sX,_u7,[0,imul(_u5,40)|0],[0,imul(_u6,40)|0]);}),new T(function(){return _tY(_u3);})];}}}}}}})(_tZ);if(_u0!=null){return _u0;}}};return _tY(_tB);})];}}}}}}})],function(_u8){return E([0,function(_){var _u9=jsResetCanvas(_r5),_ua=jsDrawImage(_r7,_sY,0,0);return new T(function(){return A(_tt,[_tw]);});}]);});};},_ub=new T(function(){return _tr(_lH);}),_uc=function(_ud,_ue,_uf,_ug,_uh,_ui){var _uj=new T(function(){return A(_ui,[_6T]);}),_uk=new T(function(){return _uc(_ud,_ue,_uf,_ug,_uh,_ui);});return _qM(_rA,function(_ul){var _um=E(_ul);if(!_um[0]){return E(E(_um[1])[1])==113?E(new T(function(){var _un=new T(function(){return _uc(_lE,_ig,_1O,_lG,_8J,_ui);});return A(_ub,[function(_uo){return E(_un);}]);})):E(_uk);}else{var _up=_kP(E(_um[1])[1],40);if(0>_up){return E(_uj);}else{if(_up>7){return E(_uj);}else{var _uq=_kP(E(_um[2])[1],40);return 0>_uq?E(_uj):_uq>7?E(_uj):[0,function(_){var _ur=jsResetCanvas(_r5),_us=jsDrawImage(_r7,_sY,0,0);return new T(function(){var _ut=[0,[0,_up],[0,_uq]],_uu=new T(function(){var _uv=E(_ud),_uw=E(_uv[1]),_ux=E(_uv[2]),_uy=E(_uw[1])[1];if(_uy>_up){return E(_8F);}else{if(_up>E(_ux[1])[1]){return E(_8F);}else{var _uz=E(_uw[2])[1],_uA=E(_ux[2])[1];if(_uz>_uq){return E(_8F);}else{if(_uq>_uA){return E(_8F);}else{var _uB=E(_uv[4][(imul(_up-_uy|0,(_uA-_uz|0)+1|0)|0)+(_uq-_uz|0)|0]);return _uB[0]==0?[0]:E(E(_uB[1])[1])==0?[1,_ut]:[0];}}}}}),_uC=new T(function(){return A(_uD,[_ud,_8t,_8u,_uu,_1O,_ue,_uf,_ug,_uh,_ui]);});return E(_uu)[0]==0?E(_uC):[0,function(_){var _uE=jsDrawImage(_r7,_rj,imul(_up,40)|0,imul(_uq,40)|0);return new T(function(){return A(_ti,[_fR(_ut,_ud,_8u,_ue,_uf,_uh),function(_uF){return E(_uC);}]);});}];});}];}}}});},_uD=function(_uG,_uH,_uI,_uJ,_uK,_uL,_uM,_uN,_uO){var _uP=E(_uK);if(!_uP[0]){var _uQ=new T(function(){return _uD(_uG,_uH,_uI,_uJ,_1O,_uL,_uM,_uN,_uO);}),_uR=new T(function(){return _uD(_uG,_uH,_uI,_1O,_1O,_uL,_uM,_uN,_uO);});return function(_uS){var _uT=new T(function(){return A(_uS,[_6T]);}),_uU=new T(function(){return A(_uQ,[_uS]);});return _qM(_rA,function(_uV){var _uW=E(_uV);if(!_uW[0]){return E(E(_uW[1])[1])==113?E(new T(function(){var _uX=new T(function(){return _uc(_lE,_ig,_1O,_lG,_8J,_uS);});return A(_ub,[function(_uY){return E(_uX);}]);})):E(_uU);}else{if(E(_uH)==2){var _uZ=_kP(E(_uW[1])[1],40);if(0>_uZ){return E(_uT);}else{if(_uZ>7){return E(_uT);}else{var _v0=_kP(E(_uW[2])[1],40);return 0>_v0?E(_uT):_v0>7?E(_uT):[0,function(_){var _v1=jsResetCanvas(_r5),_v2=jsDrawImage(_r7,_sY,0,0);return new T(function(){var _v3=[0,[0,_uZ],[0,_v0]],_v4=E(_uJ);if(!_v4[0]){var _v5=new T(function(){var _v6=E(_uG),_v7=E(_v6[1]),_v8=E(_v6[2]),_v9=E(_v7[1])[1];if(_v9>_uZ){return E(_8F);}else{if(_uZ>E(_v8[1])[1]){return E(_8F);}else{var _va=E(_v7[2])[1],_vb=E(_v8[2])[1];if(_va>_v0){return E(_8F);}else{if(_v0>_vb){return E(_8F);}else{var _vc=E(_v6[4][(imul(_uZ-_v9|0,(_vb-_va|0)+1|0)|0)+(_v0-_va|0)|0]);return _vc[0]==0?[0]:E(E(_vc[1])[1])==0?E(_uI)==0?[1,_v3]:[0]:E(_uI)==0?[0]:[1,_v3];}}}}}),_vd=new T(function(){return A(_uD,[_uG,_8t,_uI,_v5,_1O,_uL,_uM,_uN,_uO,_uS]);});return E(_v5)[0]==0?E(_vd):[0,function(_){var _ve=jsDrawImage(_r7,_rj,imul(_uZ,40)|0,imul(_v0,40)|0);return new T(function(){return A(_ti,[_fR(_v3,_uG,_uI,_uL,_uM,_uO),function(_vf){return E(_vd);}]);});}];}else{return !_dG(_5c,_v3,new T(function(){var _vg=E(_uJ);return _vg[0]==0?E(_n):_fR(_vg[1],_uG,_uI,_uL,_uM,_uO);}))?A(_uR,[_uS]):[0,function(_){var _vh=jsGet(E(_r4[1])[1],toJSStr(E(_nY)));return new T(function(){return A(_uD,[_uG,_8t,_uI,_v4,[1,[0,_bA,[0,new T(function(){var _vi=E(_uJ);return _vi[0]==0?E(_n):E(_vi[1]);}),_v3]]],_uL,_uM,_uN,new T(function(){var _vj=new T(function(){return fromJSStr(_vh);});return !_kS(_vj,_5t)?!_kS(_vj,_5o)?!_kS(_vj,_5s)?4:3:1:2;}),_uS]);});}];}});}];}}}else{return E(_uT);}}});};}else{var _vk=E(_uP[1]),_vl=E(_vk[2]),_vm=E(_vl[1]),_vn=_vm[1],_vo=_vm[2],_vp=E(_vl[2]),_vq=E(E(_vk[1])[1]);if(_vq==8){var _vr=new T(function(){var _vs=_8V(_uG,_uI,_uL,_uM,_uO,_vl),_vt=_vs[1],_vu=_vs[3],_vv=_vs[4],_vw=_vs[5],_vx=_vs[6],_vy=_vs[7],_vz=_vs[8],_vA=_vs[9],_vB=[0,_vt,_vs[2],_vu,_vv,_vw,_vx,_vy,_vz,_vA];return !_5e(_5d,_io(_vB),_e)?E(_vB):[0,_vt,new T(function(){return !_e5(_vu,_vB)?0:1;}),_uI,_vv,_vw,_vx,_vy,_vz,_vA];}),_vC=new T(function(){return _tr(_vr);});return function(_vD){var _vE=new T(function(){return A(new T(function(){var _vF=E(_vr),_vG=_vF[1],_vH=_vF[3],_vI=_vF[4],_vJ=_vF[5],_vK=_vF[6],_vL=_vF[7],_vM=_vF[8],_vN=_vF[9],_vO=E(_vF[2]);return _vO==2?E(_vH)==0?_uD(_vG,_8t,_8u,_vI,_vJ,_vK,_vL,_vM,_vN):function(_vP){return [0,function(_){var _vQ=jsSetTimeout(20,E([0,function(_){return _kG([1,new T(function(){return A(_t2,[_io(_vF),function(_vR){return A(_uD,[_vG,_8t,_8I,_vI,[1,[0,_bA,new T(function(){var _vS=_hg(_nX,[0,_vF,new T(function(){return _2b(function(_vT){var _vU=_hV(_jg,new T(function(){var _vV=_8V(_vG,_8I,_vK,_vL,_vN,_vT),_vW=_vV[1],_vX=_vV[3],_vY=_vV[4],_vZ=_vV[5],_w0=_vV[6],_w1=_vV[7],_w2=_vV[8],_w3=_vV[9],_w4=[0,_vW,_vV[2],_vX,_vY,_vZ,_w0,_w1,_w2,_w3];return !_5e(_5d,_io(_w4),_e)?E(_w4):[0,_vW,new T(function(){return !_e5(_vX,_w4)?0:1;}),_8I,_vY,_vZ,_w0,_w1,_w2,_w3];}));return [0,_vU[1],_vU[2]];},_vR);})]),_w5=_o8([0,_vS[1],_vS[2]]);return _w5[0]==0?E(_lN):E(E(_lI(_w5[1],_w5[2])[1])[8]);})]],_vK,_vL,_vM,_vN,_kE]);}]);}),_e],_);}])[1]);return new T(function(){return A(_vP,[_6T]);});}];}:_uD(_vG,_vO,_vH,_vI,_vJ,_vK,_vL,_vM,_vN);}),[_vD]);});return A(_vC,[function(_w6){return E(_vE);}]);};}else{var _w7=new T(function(){return _tr([0,new T(function(){var _w8=E(_uG),_w9=_w8[3],_wa=E(_w8[1]),_wb=E(_w8[2]);return _8S(function(_){var _wc=newArr(_w9,_8w),_=(function(_wd,_){while(1){if(_wd!=_w9){var _=_wc[_wd]=_w8[4][_wd],_we=_wd+1|0;_wd=_we;continue;}else{return E(_);}}})(0,_),_wf=E(_wa[1])[1],_wg=E(_vn)[1];if(_wf>_wg){return E(_8F);}else{if(_wg>E(_wb[1])[1]){return E(_8F);}else{var _wh=E(_wa[2])[1],_wi=E(_wb[2])[1],_wj=E(_vo)[1];if(_wh>_wj){return E(_8F);}else{if(_wj>_wi){return E(_8F);}else{var _=_wc[(imul(_wg-_wf|0,(_wi-_wh|0)+1|0)|0)+(_wj-_wh|0)|0]=_1O,_wk=_wc;return [0,E(_wa),E(_wb),_w9,_wk];}}}}});}),_uH,_uI,_uJ,_uP,_uL,_uM,_uN,_uO]);}),_wl=new T(function(){return _7U([0,_r7],new T(function(){var _wm=E(_uG),_wn=E(_wm[1]),_wo=E(_wm[2]),_wp=E(_wn[1])[1],_wq=E(_vn)[1];if(_wp>_wq){return E(_8F);}else{if(_wq>E(_wo[1])[1]){return E(_8F);}else{var _wr=E(_wn[2])[1],_ws=E(_wo[2])[1],_wt=E(_vo)[1];return _wr>_wt?E(_8F):_wt>_ws?E(_8F):E(_wm[4][(imul(_wq-_wp|0,(_ws-_wr|0)+1|0)|0)+(_wt-_wr|0)|0]);}}}),new T(function(){var _wu=E(_vn)[1];return [0,(imul(_wu,40)|0)+_kP(imul(imul(E(_vp[1])[1]-_wu|0,40)|0,_vq)|0,8)|0];}),new T(function(){var _wv=E(_vo)[1];return [0,(imul(_wv,40)|0)+_kP(imul(imul(E(_vp[2])[1]-_wv|0,40)|0,_vq)|0,8)|0];}));});return function(_ww){var _wx=new T(function(){return A(_wl,[function(_wy){return E([0,function(_){var _wz=jsSetTimeout(20,E([0,function(_){return _kG([1,new T(function(){return A(_uD,[_uG,_uH,_uI,_uJ,[1,[0,[0,_vq+1|0],_vl]],_uL,_uM,_uN,_uO,_kE]);}),_e],_);}])[1]);return new T(function(){return A(_ww,[_6T]);});}]);}]);});return A(_w7,[function(_wA){return E(_wx);}]);};}}};return _uc(_lE,_ig,_1O,_lG,_8J,_l7);}),_e]]],_e],_);}},_wB=E(_nC);if(!_wB[0]){return _rv(_);}else{var _wC=E(_wB[1]),_wD=_wC[1],_wE=_wC[2],_wF=E(_ru)[1],_wG=A(_kn,[new T(function(){return _6t(E(_wD)[1]+E(_wE)[1]|0,2)==0?E(_nv):E(_nu);}),function(_ni,_){return _kZ(new T(function(){var _wH=new T(function(){return [0,imul(E(_wE)[1],40)|0];}),_wI=new T(function(){return [0,E(_wH)[1]+40|0];}),_wJ=new T(function(){return [0,imul(E(_wD)[1],40)|0];}),_wK=new T(function(){return [0,E(_wJ)[1]+40|0];});return _mo([1,[0,_wJ,_wH],[1,[0,_wK,_wH],[1,[0,_wK,_wI],[1,[0,_wJ,_wI],[1,[0,_wJ,_wH],_e]]]]]);}),_ni,_);},_wF,_]),_wL=(function(_wM,_){while(1){var _wN=(function(_wO,_){var _wP=E(_wO);if(!_wP[0]){return _6T;}else{var _wQ=E(_wP[1]),_wR=_wQ[1],_wS=_wQ[2],_wT=A(_kn,[new T(function(){return _6t(E(_wR)[1]+E(_wS)[1]|0,2)==0?E(_nv):E(_nu);}),function(_ni,_){return _kZ(new T(function(){var _wU=new T(function(){return [0,imul(E(_wS)[1],40)|0];}),_wV=new T(function(){return [0,E(_wU)[1]+40|0];}),_wW=new T(function(){return [0,imul(E(_wR)[1],40)|0];}),_wX=new T(function(){return [0,E(_wW)[1]+40|0];});return _mo([1,[0,_wW,_wU],[1,[0,_wX,_wU],[1,[0,_wX,_wV],[1,[0,_wW,_wV],[1,[0,_wW,_wU],_e]]]]]);}),_ni,_);},_wF,_]);_wM=_wP[2];return null;}})(_wM,_);if(_wN!=null){return _wN;}}})(_wB[2],_);return _rv(_);}}}}}}}}else{return E(_lO);}}}}}},_wY=unCStr("promo"),_wZ=[1,_wY,_e],_x0=unCStr("message"),_x1=[1,_x0,_wZ],_x2=unCStr("canvas"),_x3=[1,_x2,_x1],_x4=unCStr("body"),_x5=[1,_x4,_x3],_x6=new T(function(){return _4e(_1Y,_4u,_x5,_qY);});
var hasteMain = function() {A(_x6, [0]);};window.onload = hasteMain;