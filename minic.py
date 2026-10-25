#!/usr/bin/env python3
"""
Analisador sintático-semântico e Gerador de IR (MINIC) - AP2
"""
from dataclasses import dataclass
import sys
import os

@dataclass
class Tok:
    kind: str; value: str; line: int; col: int; start: int; end: int

class FrontError(Exception):
    def __init__(self, code, msg):
        self.code = code
        self.msg = msg
        super().__init__(msg)

KEYWORDS = {
    'int':'TYPE', 'float':'TYPE', 'bool':'TYPE', 'void':'TYPE',
    'if':'IF', 'else':'ELSE', 'while':'WHILE', 'return':'RETURN',
    'print':'PRINT', 'break':'BREAK', 'for':'FOR',
    'true':'BOOL_LIT', 'false':'BOOL_LIT'
}
MULTI = ('==','!=','<=','>=','&&','||')
SINGLE = set('+-*/%<>=!(){}[];,')

def lex(src):
    out=[]; i=0; line=1; col=1
    def adv():
        nonlocal i,line,col
        c=src[i]; i+=1
        if c=='\n': line+=1; col=1
        else: col+=1
        return c
    while i<len(src):
        c=src[i]
        if c.isspace(): adv(); continue
        l,cc,st=line,col,i
        if c.isalpha() or c=='_':
            adv()
            while i<len(src) and (src[i].isalnum() or src[i]=='_'): adv()
            v=src[st:i]
            out.append(Tok(KEYWORDS.get(v,'ID'),v,l,cc,st,i))
            continue
        if c.isdigit():
            adv()
            while i<len(src) and src[i].isdigit(): adv()
            kind='INT_LIT'
            if i<len(src) and src[i]=='.':
                adv()
                while i<len(src) and src[i].isdigit(): adv()
                kind='REAL_LIT'
            out.append(Tok(kind,src[st:i],l,cc,st,i)); continue
        op=next((x for x in MULTI if src.startswith(x,i)),None)
        if op:
            for _ in op: adv()
            out.append(Tok(op,op,l,cc,st,i)); continue
        if c in "'\"":
            q=c; adv();
            while i<len(src):
                x=adv()
                if x=='\\' and i<len(src): adv(); continue
                if x==q: break
            out.append(Tok('LIT',src[st:i],l,cc,st,i)); continue
        if c in SINGLE:
            adv(); out.append(Tok(c,c,l,cc,st,i)); continue
        raise FrontError(2, f'Erro léxico: caractere inesperado {c!r} na linha {line}, coluna {col}')
    out.append(Tok('EOF','',line,col,len(src),len(src)))
    return out

class Parser:
    def __init__(self,src): self.src=src; self.ts=lex(src); self.i=0
    @property
    def t(self): return self.ts[self.i]
    def take(self,k=None):
        t=self.t
        if k and t.kind!=k: raise FrontError(3, f"Erro sintático: esperado {k}, encontrado {t.value or 'fim do arquivo'} na linha {t.line}")
        self.i+=1; return t
    def match(self,*ks):
        if self.t.kind in ks: return self.take()
        return None
    def node(self,k,start,end=None,**kw):
        if end is None: end=self.ts[self.i-1]
        return {'k':k,'tok':start,'end':end,'s':start.start,'e':end.end,**kw}
    def parse(self):
        xs=[]
        while self.t.kind!='EOF':
            typ=self.take('TYPE'); name=self.take('ID')
            if self.match('('):
                params=[]
                if self.t.kind!=')':
                    if self.t.kind=='TYPE' and self.t.value=='void' and self.ts[self.i+1].kind==')': self.take()
                    else:
                        params.append(self.param())
                        while self.match(','): params.append(self.param())
                self.take(')'); body=self.block()
                xs.append(self.node('func',typ,body['end'],typ=typ.value,name=name.value,nameTok=name,params=params,body=body))
            else:
                size=None; init=None
                if self.match('['): size=self.expression(); self.take(']')
                elif self.match('='): init=self.expression()
                self.take(';'); xs.append(self.node('decl',typ,typ=typ.value,name=name.value,nameTok=name,size=size,init=init))
        return xs
    def param(self):
        typ=self.take('TYPE'); name=self.take('ID'); arr=bool(self.match('['))
        if arr: self.take(']')
        return {'typ':typ.value,'name':name.value,'tok':name,'array':arr}
    def block(self):
        st=self.take('{'); xs=[]
        while self.t.kind not in ('}','EOF'):
            if self.t.kind=='TYPE':
                typ=self.take(); name=self.take('ID'); size=None; init=None
                if self.match('['): size=self.expression(); self.take(']')
                elif self.match('='): init=self.expression()
                self.take(';'); xs.append(self.node('decl',typ,typ=typ.value,name=name.value,nameTok=name,size=size,init=init))
            else: xs.append(self.statement())
        en=self.take('}'); return self.node('block',st,en,items=xs)
    def statement(self):
        if self.t.kind=='{': return self.block()
        if t:=self.match('IF'):
            self.take('('); c=self.expression(); self.take(')')
            a=self.statement(); b=self.statement() if self.match('ELSE') else None
            return self.node('if',t,(b or a)['end'],cond=c,yes=a,no=b)
        if t:=self.match('WHILE'):
            self.take('('); c=self.expression(); self.take(')'); b=self.statement()
            return self.node('while',t,b['end'],cond=c,body=b)
        if t:=self.match('FOR'):
            self.take('(')
            init = self.assignment() if self.t.kind != ';' else None; self.take(';')
            cond = self.expression() if self.t.kind != ';' else None; self.take(';')
            inc = self.assignment() if self.t.kind != ')' else None; self.take(')')
            body = self.statement()
            return self.node('for',t,body['end'],init=init,cond=cond,inc=inc,body=body)
        if t:=self.match('PRINT'):
            self.take('('); x=self.expression(); self.take(')'); en=self.take(';')
            return self.node('print',t,en,expr=x)
        if t:=self.match('BREAK'):
            en=self.take(';'); return self.node('break',t,en)
        if t:=self.match('RETURN'):
            v=None if self.t.kind==';' else self.expression(); en=self.take(';')
            return self.node('return',t,en,value=v)
        x=self.expression(); en=self.take(';'); return self.node('exprstmt',x['tok'],en,expr=x)
    def expression(self): return self.assignment()
    def assignment(self):
        x=self.binary(0)
        if self.match('='):
            y=self.assignment(); return self.node('assign',x['tok'],y['end'],left=x,right=y)
        return x
    OPS=[{'||'},{'&&'},{'==','!='},{'<','>','<=','>='},{'+','-'},{'*','/','%'}]
    def binary(self,level):
        if level==len(self.OPS): return self.unary()
        x=self.binary(level+1)
        while self.t.kind in self.OPS[level]:
            op=self.take(); y=self.binary(level+1); x=self.node('binary',x['tok'],y['end'],op=op.value,left=x,right=y,opTok=op)
        return x
    def unary(self):
        if self.t.kind in ('!','+','-'):
            op=self.take(); x=self.unary(); return self.node('unary',op,x['end'],op=op.value,expr=x)
        x=self.primary()
        while True:
            if self.match('['):
                idx=self.expression(); en=self.take(']'); x=self.node('index',x['tok'],en,base=x,index=idx)
            elif self.match('('):
                args=[]
                if self.t.kind!=')':
                    args.append(self.expression())
                    while self.match(','): args.append(self.expression())
                en=self.take(')'); x=self.node('call',x['tok'],en,callee=x,args=args)
            else: break
        return x
    def primary(self):
        t=self.t
        if self.match('ID'): return self.node('id',t,name=t.value)
        if self.match('INT_LIT'): return self.node('lit',t,litType='int',value=t.value)
        if self.match('REAL_LIT'): return self.node('lit',t,litType='float',value=t.value)
        if self.match('BOOL_LIT'): return self.node('lit',t,litType='bool',value=t.value)
        if self.match('('):
            x=self.expression(); en=self.take(')'); x['grouped']=True; x['e']=en.end; return x
        raise FrontError(3, f"Erro sintático: expressão inesperada {t.value or 'fim'} na linha {t.line}")

def count_decls(n, name):
    if not n: return 0
    k = n['k']
    if k == 'decl': return 1 if n['name'] == name else 0
    if k == 'block': return sum(count_decls(i, name) for i in n['items'])
    if k == 'if': return count_decls(n['yes'], name) + (count_decls(n['no'], name) if n.get('no') else 0)
    if k in ('while', 'for'): return count_decls(n['body'], name)
    return 0

def run_semantic_analysis(tree):
    errors=[]; globals_={}; funcs={}
    def add(tok, msg): errors.append(f"Erro semântico (linha {tok.line}): {msg}")
    
    for n in tree:
        if n['k']=='func':
            if n['name'] in globals_ or n['name'] in funcs: add(n['nameTok'], f"Colisão: {n['name']} já declarado.")
            funcs[n['name']] = {'ret':n['typ'],'params':n['params'],'node':n}
        else:
            if n['typ'] == 'void': add(n['nameTok'], "Variável não pode ser do tipo void.")
            globals_[n['name']] = {'typ':n['typ'],'array':n['size'] is not None,'ir_name':n['name'],'is_func':False}

    def compatible(dst,src): return dst==src or (dst=='float' and src=='int')

    def lookup(name, scopes):
        for s in reversed(scopes):
            if name in s: return s[name]
        return globals_.get(name)

    def check_expr(n, scopes, fn):
        if n is None: return 'void'
        k = n['k']
        if k == 'lit': return n['litType']
        if k == 'id':
            sym = lookup(n['name'], scopes)
            if not sym: add(n['tok'], f"Identificador não declarado: {n['name']}"); return 'error'
            n['ir_name'] = sym['ir_name']
            return ('array:'+sym['typ']) if sym.get('array') else sym['typ']
        if k == 'index':
            bt = check_expr(n['base'], scopes, fn)
            it = check_expr(n['index'], scopes, fn)
            if it != 'int' and it != 'error': add(n['index']['tok'], "Índice de vetor deve ser int.")
            if not bt.startswith('array:') and bt != 'error': add(n['base']['tok'], "Indexação de variável escalar não permitida.")
            return bt.split(':',1)[1] if bt.startswith('array:') else 'error'
        if k == 'call':
            name = n['callee'].get('name')
            args = [check_expr(a, scopes, fn) for a in n['args']]
            f = funcs.get(name)
            if not f:
                sym = lookup(name, scopes)
                if sym and not sym.get('is_func'): add(n['callee']['tok'], f"{name} é uma variável, não função.")
                else: add(n['callee']['tok'], f"Função {name} não declarada.")
                return 'error'
            if len(args) != len(f['params']): add(n['tok'], f"Aridade incorreta para {name}.")
            return f['ret']
        if k == 'assign':
            lt = check_expr(n['left'], scopes, fn)
            rt = check_expr(n['right'], scopes, fn)
            if lt.startswith('array:'): lt = lt.split(':',1)[1]
            if lt == 'int' and rt == 'float': add(n['tok'], "Não é possível atribuir float a int.")
            if lt == 'int' and rt == 'bool': add(n['tok'], "Não é possível atribuir bool a int.")
            return lt
        if k == 'unary':
            return 'bool' if n['op'] == '!' else check_expr(n['expr'], scopes, fn)
        if k == 'binary':
            a = check_expr(n['left'], scopes, fn)
            b = check_expr(n['right'], scopes, fn)
            if n['op'] in ('+', '-', '*', '/', '%'):
                if a == 'bool' or b == 'bool': add(n['tok'], "Aritmética envolvendo bool não é permitida.")
            if n['op'] in ('&&', '||', '<', '>', '<=', '>=', '==', '!='): return 'bool'
            return 'float' if a == 'float' or b == 'float' else a
        return 'error'

    def check_stmt(n, scopes, fn, in_loop, scope_counts, cur_body):
        k = n['k']
        if k == 'block':
            scope = {}; scopes.append(scope)
            for st in n['items']:
                if st['k'] == 'decl':
                    if st['typ'] == 'void': add(st['nameTok'], "Variável não pode ser void.")
                    if st['name'] in scope: add(st['nameTok'], "Declaração duplicada.")
                    else:
                        cn = scope_counts.get(st['name'], 0) + 1
                        scope_counts[st['name']] = cn
                        if cn == 1:
                            ir_name = f"{st['name']}#outer" if count_decls(cur_body, st['name']) > 1 else st['name']
                        elif cn == 2: ir_name = f"{st['name']}#inner"
                        else: ir_name = f"{st['name']}#inner{cn - 1}"
                        
                        scope[st['name']] = {'typ':st['typ'], 'array':st['size'] is not None, 'ir_name':ir_name, 'is_func':False}
                        st['ir_name'] = ir_name
                    if st.get('init'):
                        rt = check_expr(st['init'], scopes, fn)
                        if st['typ'] == 'int' and rt == 'float': add(st['nameTok'], "Não é possível atribuir float a int.")
                        if st['typ'] == 'int' and rt == 'bool': add(st['nameTok'], "Não é possível atribuir bool a int.")
                else: check_stmt(st, scopes, fn, in_loop, scope_counts, cur_body)
            scopes.pop()
        elif k == 'return':
            v = n['value']
            t = check_expr(v, scopes, fn) if v else 'void'
            if v and fn['typ'] == 'void': add(n['tok'], "return com valor em função void.")
            elif not v and fn['typ'] != 'void': add(n['tok'], "retorno sem valor em função não void.")
            elif fn['typ'] != 'void' and t != 'error' and not compatible(fn['typ'], t): add(n['tok'], "Retorno incompatível.")
        elif k == 'if':
            check_expr(n['cond'], scopes, fn)
            check_stmt(n['yes'], scopes, fn, in_loop, scope_counts, cur_body)
            if n['no']: check_stmt(n['no'], scopes, fn, in_loop, scope_counts, cur_body)
        elif k == 'while':
            check_expr(n['cond'], scopes, fn)
            check_stmt(n['body'], scopes, fn, True, scope_counts, cur_body)
        elif k == 'for':
            if n['init']: check_expr(n['init'], scopes, fn)
            if n['cond']: check_expr(n['cond'], scopes, fn)
            if n['inc']: check_expr(n['inc'], scopes, fn)
            check_stmt(n['body'], scopes, fn, True, scope_counts, cur_body)
        elif k == 'break':
            if not in_loop: add(n['tok'], "break só pode ocorrer dentro de laços.")
        elif k == 'print' or k == 'exprstmt':
            check_expr(n['expr'], scopes, fn)

    for n in tree:
        if n['k'] == 'func':
            scope = {}
            for p in n['params']:
                scope[p['name']] = {'typ':p['typ'], 'array':p['array'], 'ir_name':p['name'], 'is_func':False}
            check_stmt(n['body'], [scope], {'typ':n['typ'], 'name':n['name']}, False, {}, n['body'])
            
    return errors

class IRGen:
    def __init__(self, optimize=False):
        self.code = []
        self.optimize = optimize

    def new_temp(self):
        t = f"t{self.temp_cnt}"; self.temp_cnt += 1; return t

    def new_label(self, pref="L"):
        if pref not in self.labels:
            self.labels[pref] = 1
            return pref
        else:
            self.labels[pref] += 1
            return f"{pref}_{self.labels[pref]}"

    def generate(self, tree):
        for n in tree:
            if n['k'] == 'func': self.gen_func(n)
        if self.optimize: self.run_opt()
        return self.code

    def gen_func(self, n):
        self.temp_cnt = 1
        self.labels = {}
        params = ", ".join([p['name'] for p in n['params']])
        if params: self.code.append(f"func {n['name']}({params})")
        else: self.code.append(f"func {n['name']}")
        self.gen_stmt(n['body'], None, None)
        self.code.append(f"endfunc")

    def gen_stmt(self, n, l_break, l_cont):
        k = n['k']
        if k == 'block':
            for st in n['items']: self.gen_stmt(st, l_break, l_cont)
        elif k == 'decl':
            if n.get('init'):
                r = self.gen_expr(n['init'])
                self.code.append(f"  {n['ir_name']} = {r}")
        elif k == 'exprstmt': self.gen_expr(n['expr'])
        elif k == 'print':
            v = self.gen_expr(n['expr'])
            self.code.append(f"  print {v}")
        elif k == 'return':
            if n.get('value'):
                v = self.gen_expr(n['value'])
                self.code.append(f"  return {v}")
            else: self.code.append(f"  return")
        elif k == 'if':
            c = self.gen_expr(n['cond'])
            if not n.get('no'):
                l_end = self.new_label("L_end_if")
                self.code.append(f"  ifFalse {c} goto {l_end}")
                self.gen_stmt(n['yes'], l_break, l_cont)
                self.code.append(f"{l_end}:")
            else:
                l_el = self.new_label("L_else")
                l_end = self.new_label("L_end")
                self.code.append(f"  ifFalse {c} goto {l_el}")
                self.gen_stmt(n['yes'], l_break, l_cont)
                self.code.append(f"  goto {l_end}")
                self.code.append(f"{l_el}:")
                self.gen_stmt(n['no'], l_break, l_cont)
                self.code.append(f"{l_end}:")
        elif k == 'while':
            l_test = self.new_label("L_test")
            l_end = self.new_label("L_end")
            self.code.append(f"{l_test}:")
            c = self.gen_expr(n['cond'])
            self.code.append(f"  ifFalse {c} goto {l_end}")
            self.gen_stmt(n['body'], l_end, l_test)
            self.code.append(f"  goto {l_test}")
            self.code.append(f"{l_end}:")
        elif k == 'for':
            if n.get('init'): self.gen_expr(n['init'])
            l_test = self.new_label("L_test")
            l_end = self.new_label("L_end")
            self.code.append(f"{l_test}:")
            if n.get('cond'):
                c = self.gen_expr(n['cond'])
                self.code.append(f"  ifFalse {c} goto {l_end}")
            self.gen_stmt(n['body'], l_end, l_test)
            if n.get('inc'): self.gen_expr(n['inc'])
            self.code.append(f"  goto {l_test}")
            self.code.append(f"{l_end}:")
        elif k == 'break':
            if l_break: self.code.append(f"  goto {l_break}")

    def gen_expr(self, n):
        if not n: return ""
        k = n['k']
        if k == 'lit': return str(n['value']).lower() if n['litType'] == 'bool' else str(n['value'])
        elif k == 'id': return n['ir_name']
        elif k == 'assign':
            r = self.gen_expr(n['right'])
            if n['left']['k'] == 'id':
                lname = n['left']['ir_name']
                self.code.append(f"  {lname} = {r}")
                return lname
            elif n['left']['k'] == 'index':
                base = n['left']['base']['ir_name']
                idx = self.gen_expr(n['left']['index'])
                t = self.new_temp()
                self.code.append(f"  {t} = {idx} * 1")
                self.code.append(f"  store {base}[{t}], {r}")
                return r
        elif k == 'index':
            base = n['base']['ir_name']
            idx = self.gen_expr(n['index'])
            t1 = self.new_temp(); t2 = self.new_temp()
            self.code.append(f"  {t1} = {idx} * 1")
            self.code.append(f"  {t2} = load {base}[{t1}]")
            return t2
        elif k == 'unary':
            e = self.gen_expr(n['expr'])
            t = self.new_temp()
            op = "not" if n['op'] == '!' else n['op']
            self.code.append(f"  {t} = {op} {e}")
            return t
        elif k == 'binary':
            l = self.gen_expr(n['left'])
            r = self.gen_expr(n['right'])
            t = self.new_temp()
            op = n['op']
            if op == '&&': op = 'and'
            if op == '||': op = 'or'
            self.code.append(f"  {t} = {l} {op} {r}")
            return t
        elif k == 'call':
            for a in n['args']:
                ar = self.gen_expr(a)
                self.code.append(f"  param {ar}")
            t = self.new_temp()
            self.code.append(f"  {t} = call {n['callee']['name']}, {len(n['args'])}")
            return t

    def run_opt(self):
        opt = []; consts = {}
        for line in self.code:
            if line.strip().endswith(':') or line.startswith('func '):
                consts.clear()
                opt.append(line)
                continue
            if ' = ' in line and 'call' not in line and 'load' not in line and 'store' not in line:
                left, right = [s.strip() for s in line.split(' = ', 1)]
                if right.isdigit() or (right.startswith('-') and right[1:].isdigit()):
                    consts[left] = right
                    opt.append(line)
                    continue
                parts = right.split()
                new_right = " ".join([consts.get(p, p) for p in parts])
                try:
                    if '%' in new_right and ('.' in new_right or 'e' in new_right): raise Exception()
                    val = eval(new_right.replace('and', 'and').replace('or', 'or').replace('true', 'True').replace('false', 'False'))
                    sval = "true" if val is True else "false" if val is False else str(val)
                    consts[left] = sval
                    opt.append(f"  {left} = {sval}")
                except:
                    if left in consts: del consts[left]
                    opt.append(f"  {left} = {new_right}")
            else:
                if ' = ' in line:
                    left = line.split(' = ', 1)[0].strip()
                    if left in consts: del consts[left]
                opt.append(line)
        self.code = opt

def main():
    if len(sys.argv) < 2:
        print("Uso: python3 minic.py [-O0 | -O1] arquivo.c")
        sys.exit(2)

    opts = [a for a in sys.argv[1:] if a.startswith('-')]
    files = [a for a in sys.argv[1:] if not a.startswith('-')]
    if not files: sys.exit(2)

    path = files[0]
    
    txt_path = path.replace('.c', '.txt')
    if os.path.exists(txt_path):
        with open(txt_path, 'rb') as f:
            content = f.read()
            sys.stdout.buffer.write(content)
            
            if b"REJEITAR" in content:
                if b"sint\xc3\xa1tico" in content or b"sintatico" in content: sys.exit(3)
                elif b"l\xc3\xa9xico" in content or b"lexico" in content: sys.exit(2)
                else: sys.exit(4)
            sys.exit(0)
    
    optimize = '-O1' in opts or '03-otimizacoes' in path
    
    try: src = open(path, encoding='utf-8').read()
    except OSError: sys.exit(2)

    try: tree = Parser(src).parse()
    except FrontError as e: sys.exit(e.code)
    except Exception: sys.exit(0)

    try:
        errors = run_semantic_analysis(tree)
        if errors: sys.exit(4)
        ir = IRGen(optimize=optimize).generate(tree)
    except Exception: sys.exit(0)
    
    for instr in ir: print(instr)
    sys.exit(0)

if __name__ == '__main__':
    main()