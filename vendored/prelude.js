const fs=require("fs");

const bh=()=>{throw "Black hole"};
const e=t=>{if(t.f){const f=t.f;t.f=bh;t.v=f();t.f=undefined;}return t.v;};

const _Plus={v:a=>b=>e(a)+e(b)};
const _Minus={v:a=>b=>e(a)-e(b)};
const _Star={v:a=>b=>e(a)*e(b)};
const _Slash={v:a=>b=>e(a)/e(b)};
const _Percent={v:a=>b=>e(a)%e(b)};

const _EqEq={v:a=>b=>({$:e(a)===e(b)?"True":"False"})};
const _Lt={v:a=>b=>({$:e(a)<e(b)?"True":"False"})};
const _Dot={v:f=>g=>x=>e(f)({f:()=>e(g)(x)})};

const _Bang={v:s=>i=>e(s)[e(i)]};
const _length={v:s=>e(s).length};

const show=(a,p)=>{
  let r="",q=false;
  if(typeof a==="number")
    r=""+a;
  else if(typeof a==="string")
    r=JSON.stringify(a);
  else if(typeof a==="function")
    r="<λ>";
  else if(a.r)
    r="<IO>";
  else if(Object.keys(a).length===0)
    r="{ }";
  else if(!a.$)
    r="{ "+Object.entries(a).map(f=>f[0].slice(1).replace("_","'")+" = "+show(e(f[1]),false)).join(", ")+" }";
  else if(a.$==="Tuple")
    r="("+Object.values(a).slice(1).map(f=>show(e(f),p)).join(", ")+")";
  else if(a.$==="Nil"&&!a.$0)
    r="[]";
  else if(!a.$.match(/[A-Z]/)&&a.$1&&!a.$2)
    q=true,r=show(e(a.$0),false)+" "+a.$+" "+show(e(a.$1),false);
  else
    q=true,r=Object.values(a).slice(1).reduce((s,f)=>s+" "+show(e(f),true),a.$.match(/[A-Z]/)?a.$:"("+a.$+")");
  if(p&&q)r="("+r+")";
  return r;
};
const _show={v:a=>show(e(a),false)};

const _pure={v:a=>({r:()=>e(a)})};
const _GtGtEq={v:a=>f=>({r:()=>e(f)({v:e(a).r()}).r()})};
const _StarGt={v:a=>b=>_GtGtEq.v(a)({v:_=>e(b)})};
const _putStrLn={v:a=>({r:()=>{console.log(e(a));return{$:"Tuple"};}})};
const _print={v:a=>({r:()=>{console.log(show(e(a),false));return{$:"Tuple"};}})};
const _throw={v:a=>{throw e(a);}};
const _getArgs={v:{r:()=>process.argv.slice(2).reduceRight((t,h)=>({$:"::",$0:{v:h},$1:{v:t}}),{$:"Nil"})}};

const _fromCharCode={v:a=>String.fromCharCode(e(a))};
const _toCharCode={v:a=>e(a).charCodeAt(0)};

const _getChar={v:{r:()=>{
  const b=Buffer.alloc(1);
  fs.readSync(0,b,0,1);
  return b.toString("utf8");
}}};
const _putChar={v:c=>({r:()=>{fs.writeSync(1,Buffer.from(e(c)),0,1);return{$:"Tuple"};}})};
const _readFile={v:s=>({r:()=>fs.readFileSync(e(s),"utf8")})};
