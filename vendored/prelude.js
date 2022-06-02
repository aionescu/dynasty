const _v=v=>({_v:v});
const _f=f=>({_f:f});
const _e=t=>{if(t._f){t._v=t._f();t._f=undefined;};return t._v;};
const _runIO=t=>_e(t)._r();
const pure=_v(a=>({_r:()=>_e(a)}));
const $GtGtEq=_v(a=>f=>({_r:()=>_e(f)({_v:_runIO(a)})._r()}));
const $StarGt=_v(a=>b=>_e($GtGtEq)(a)(_v(_=>_e(b))));
const $Plus=_v(a=>b=>_e(a)+_e(b));
const print=_v(a=>({_r:()=>{console.log(_e(a));return {_c:"Tuple"};}}));
