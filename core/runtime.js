const bh=()=>{throw "<Self-recursive thunk>"};const e=t=>{if(t.f){const f=t.f;t.f=bh;t.v=f();t.f=undefined;}return t.v;};
const show=(a,p)=>{
  let r="",q=false;
  if(typeof a==="number")r=""+a;
  else if(typeof a==="string")r=JSON.stringify(a);
  else if(typeof a==="function")r="<λ>";
  else if(a.r)r="<IO>";
  else if(Object.keys(a).length===0)r="{ }";
  else if(!a.$)r="{ "+Object.entries(a).map(f=>f[0].slice(1).replace("_","'")+" = "+show(e(f[1]),false)).join(", ")+" }";
  else if(a.$==="Tuple"&&!(a.$0&&!a.$1))r="("+Object.values(a).slice(1).map(f=>show(e(f),p)).join(", ")+")";
  else if(a.$==="Nil"&&!a.$0)r="[]";
  else if(!a.$.match(/[A-Z]/)&&a.$1&&!a.$2)q=true,r=show(e(a.$0),true)+" "+a.$+" "+show(e(a.$1),true);
  else q=true,r=Object.values(a).slice(1).reduce((s,f)=>s+" "+show(e(f),true),a.$.match(/[A-Z]/)?a.$:"("+a.$+")");
  if(p&&q)r="("+r+")";return r;
};
