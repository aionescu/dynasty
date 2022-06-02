const $e=t=>{if(t.$f){const f=t.$f;t.$f=()=>{throw "Black hole"};t.$v=f();t.$f=undefined;};return t.$v;};
const pure={$v:a=>({$r:()=>$e(a)})};
const $GtGtEq={$v:a=>f=>({$r:()=>$e(f)({$v:$e(a).$r()}).$r()})};
const $StarGt={$v:a=>b=>$e($GtGtEq)(a)({$v:_=>$e(b)})};
const $Plus={$v:a=>b=>$e(a)+$e(b)};
const $Star={$v:a=>b=>$e(a)*$e(b)};
const $Minus={$v:a=>b=>$e(a)-$e(b)};
const print={$v:a=>({$r:()=>{console.log($e(a));return {$:"Tuple"};}})};
