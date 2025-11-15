import re
import sys
import random

# ---------- LEXER ----------
# Order matters: multi-char tokens (==, !=, <=, >=) must appear before single-char '='
TOKEN_SPEC = [
    ('NUMBER',   r'\d+(\.\d+)?'),
    ('STRING',   r'"([^"\\]|\\.)*"'),
    ('EQ',       r'=='),
    ('NE',       r'!='),
    ('LE',       r'<='),
    ('GE',       r'>='),
    ('LT',       r'<'),
    ('GT',       r'>'),
    ('ASSIGN',   r'='),
    ('PLUS',     r'\+'),
    ('MINUS',    r'-'),
    ('MUL',      r'\*'),
    ('DIV',      r'/'),
    ('LPAREN',   r'\('),
    ('RPAREN',   r'\)'),
    ('LBRACE',   r'\{'),
    ('RBRACE',   r'\}'),
    ('COMMA',    r','),
    ('SEMI',     r';'),
    ('IDENT',    r'[A-Za-z_\u0980-\u09FF\?][A-Za-z0-9_\u0980-\u09FF\?]*'),
    ('SKIP',     r'[ \t]+'),
    ('NEWLINE',  r'\n'),
    ('COMMENT',  r'//.*'),
    ('MISMATCH', r'.'),
]
TOKEN_RE = re.compile('|'.join(f"(?P<{n}>{p})" for n, p in TOKEN_SPEC))

# Keywords mapping (Bengali-sounding words in ASCII)
KEYWORDS = {
    # print / control
    'bolo': 'SUN',
    'chaap': 'SUN',          # alias for debug-print
    'jodi': 'AGAR',
    'nahole': 'WARNA',
    'jotokhon_porjonto': 'TABTAK',
    'ghurbo': 'TABTAK',      # alias

    # functions & return
    'cholo': 'CHAL',
    'cholo_na': 'CHAL',
    'phero': 'WAAPIS',

    # booleans
    'thik': 'TRUE',
    'bhul': 'FALSE',

    # variable initializer
    'accha': 'ACCHA',

    # fun keywords
    'bujhli?': 'BUJHLIQ',
    'hmm': 'HMM',
}

class Token:
    def __init__(self, typ, val, pos=None):
        self.type = typ
        self.val = val
        self.pos = pos
    def __repr__(self):
        return f"Token({self.type},{self.val!r},pos={self.pos})"

def lex(code):
    pos = 0
    for mo in TOKEN_RE.finditer(code):
        kind = mo.lastgroup
        val = mo.group()
        if kind == 'NUMBER':
            yield Token('NUMBER', float(val) if '.' in val else int(val), pos)
        elif kind == 'STRING':
            inner = val[1:-1].encode('utf-8').decode('unicode_escape')
            yield Token('STRING', inner, pos)
        elif kind == 'IDENT':
            if val in KEYWORDS:
                yield Token(KEYWORDS[val], val, pos)
            else:
                yield Token('IDENT', val, pos)
        elif kind in ('SKIP','NEWLINE','COMMENT'):
            pass
        elif kind == 'MISMATCH':
            raise SyntaxError(f'Unexpected char {val!r} at pos {pos}')
        else:
            yield Token(kind, val, pos)
        pos = mo.end()
    yield Token('EOF', '', pos)

# ---------- PARSER (recursive descent) ----------
class Parser:
    def __init__(self, tokens):
        # tokens may be an iterator; convert to list for simplicity
        self.tokens = list(tokens)
        self.pos = 0

    def peek(self):
        return self.tokens[self.pos]

    def advance(self):
        tok = self.tokens[self.pos]
        self.pos += 1
        return tok

    def expect(self, typ):
        tok = self.advance()
        if tok.type != typ:
            raise SyntaxError(f'Expected {typ} but got {tok.type} at pos {tok.pos}')
        return tok

    def parse(self):
        stmts = []
        while self.peek().type != 'EOF':
            stmts.append(self.parse_statement())
        return ('block', stmts)

    def parse_statement(self):
        t = self.peek()

        # print / bolo / chaap: allow multiple comma-separated expressions
        if t.type == 'SUN':
            self.advance()
            exprs = [self.parse_expr()]
            while self.peek().type == 'COMMA':
                self.advance()
                exprs.append(self.parse_expr())
            self.expect('SEMI')
            return ('print_multi', exprs)

        # return: phero expr;
        elif t.type == 'WAAPIS':
            self.advance()
            expr = self.parse_expr()
            self.expect('SEMI')
            return ('return', expr)

        # variable declaration: accha x;  OR accha x = expr;
        elif t.type == 'ACCHA':
            self.advance()
            name = self.expect('IDENT').val
            if self.peek().type == 'ASSIGN':
                self.advance()
                expr = self.parse_expr()
                self.expect('SEMI')
                return ('declare_assign', name, expr)
            else:
                self.expect('SEMI')
                return ('declare', name)

        # assignment: x = expr;
        elif t.type == 'IDENT':
            name = self.advance().val
            if self.peek().type != 'ASSIGN':
                raise SyntaxError(f'Only assignment statements start with identifier (pos {self.peek().pos})')
            self.advance()
            expr = self.parse_expr()
            self.expect('SEMI')
            return ('assign', name, expr)

        # bujhli? (expr);
        elif t.type == 'BUJHLIQ':
            self.advance()
            self.expect('LPAREN')
            expr = self.parse_expr()
            self.expect('RPAREN')
            self.expect('SEMI')
            return ('bujhli', expr)

        # hmm;
        elif t.type == 'HMM':
            self.advance()
            self.expect('SEMI')
            return ('hmm',)

        # if
        elif t.type == 'AGAR':
            return self.parse_if()

        # while
        elif t.type == 'TABTAK':
            return self.parse_while()

        # function def
        elif t.type == 'CHAL':
            return self.parse_funcdef()

        else:
            raise SyntaxError(f'Unexpected token: {t}')

    def parse_if(self):
        self.expect('AGAR')
        self.expect('LPAREN')
        cond = self.parse_expr()
        self.expect('RPAREN')
        self.expect('LBRACE')
        then_block = []
        while self.peek().type != 'RBRACE':
            then_block.append(self.parse_statement())
        self.expect('RBRACE')
        else_block = None
        if self.peek().type == 'WARNA':
            self.advance()
            self.expect('LBRACE')
            else_stmts = []
            while self.peek().type != 'RBRACE':
                else_stmts.append(self.parse_statement())
            self.expect('RBRACE')
            else_block = ('block', else_stmts)
        return ('if', cond, ('block', then_block), else_block)

    def parse_while(self):
        self.expect('TABTAK')
        self.expect('LPAREN')
        cond = self.parse_expr()
        self.expect('RPAREN')
        self.expect('LBRACE')
        body = []
        while self.peek().type != 'RBRACE':
            body.append(self.parse_statement())
        self.expect('RBRACE')
        return ('while', cond, ('block', body))

    def parse_funcdef(self):
        self.expect('CHAL')
        name = self.expect('IDENT').val
        self.expect('LPAREN')
        args = []
        if self.peek().type != 'RPAREN':
            while True:
                args.append(self.expect('IDENT').val)
                if self.peek().type == 'COMMA':
                    self.advance()
                    continue
                break
        self.expect('RPAREN')
        self.expect('LBRACE')
        body = []
        # now parse statements normally — 'WAAPIS' handled in parse_statement
        while self.peek().type != 'RBRACE':
            body.append(self.parse_statement())
        self.expect('RBRACE')
        return ('func', name, args, ('block', body))

    # expressions with precedence
    def parse_expr(self):
        return self.parse_equality()

    def parse_equality(self):
        node = self.parse_comparison()
        while self.peek().type in ('EQ','NE'):
            op = self.advance().type
            right = self.parse_comparison()
            node = (op, node, right)
        return node

    def parse_comparison(self):
        node = self.parse_term()
        while self.peek().type in ('LT','GT','LE','GE'):
            op = self.advance().type
            right = self.parse_term()
            node = (op, node, right)
        return node

    def parse_term(self):
        node = self.parse_factor()
        while self.peek().type in ('PLUS','MINUS'):
            op = self.advance().type
            right = self.parse_factor()
            node = (op, node, right)
        return node

    def parse_factor(self):
        node = self.parse_unary()
        while self.peek().type in ('MUL','DIV'):
            op = self.advance().type
            right = self.parse_unary()
            node = (op, node, right)
        return node

    def parse_unary(self):
        if self.peek().type == 'MINUS':
            self.advance()
            return ('neg', self.parse_primary())
        return self.parse_primary()

    def parse_primary(self):
        t = self.peek()
        if t.type == 'NUMBER':
            self.advance()
            return ('number', t.val)
        if t.type == 'STRING':
            self.advance()
            return ('string', t.val)
        if t.type == 'TRUE':
            self.advance()
            return ('bool', True)
        if t.type == 'FALSE':
            self.advance()
            return ('bool', False)
        if t.type == 'IDENT':
            name = self.advance().val
            if self.peek().type == 'LPAREN':
                self.advance()
                args = []
                if self.peek().type != 'RPAREN':
                    while True:
                        args.append(self.parse_expr())
                        if self.peek().type == 'COMMA':
                            self.advance()
                        else:
                            break
                self.expect('RPAREN')
                return ('call', name, args)
            return ('var', name)
        if t.type == 'LPAREN':
            self.advance()
            node = self.parse_expr()
            self.expect('RPAREN')
            return node
        raise SyntaxError(f'Unexpected token in expression: {t}')

# ---------- INTERPRETER ----------
class ReturnSignal(Exception):
    def __init__(self, val):
        self.val = val

class Interpreter:
    def __init__(self):
        self.global_env = {}
        self.functions = {}

    def eval(self, node, env=None):
        if env is None:
            env = self.global_env
        kind = node[0]

        # block
        if kind == 'block':
            for s in node[1]:
                self.eval(s, env)
            return None

        # multi-arg print
        if kind == 'print_multi':
            vals = [self.eval(e, env) for e in node[1]]
            # simple join with spaces
            print(' '.join(str(v) for v in vals))
            return None

        # bujhli? (print thik/bhul)
        if kind == 'bujhli':
            val = self.eval(node[1], env)
            print('thik' if val else 'bhul')
            return None

        # hmm
        if kind == 'hmm':
            print('hmm...')
            return None

        # declare
        if kind == 'declare':
            env[node[1]] = None
            return None

        if kind == 'declare_assign':
            env[node[1]] = self.eval(node[2], env)
            return None

        # assign (must be declared)
        if kind == 'assign':
            name = node[1]
            if name not in env:
                raise NameError(f"Areyyyyy Bhai! '{name}' declare koro agey — use: accha {name}; or accha {name} = ...;")
            env[name] = self.eval(node[2], env)
            return None

        # if
        if kind == 'if':
            cond = self.eval(node[1], env)
            if cond:
                return self.eval(node[2], env)
            elif node[3] is not None:
                return self.eval(node[3], env)
            return None

        # while
        if kind == 'while':
            while self.eval(node[1], env):
                self.eval(node[2], env)
            return None

        # function definition
        if kind == 'func':
            name = node[1]; args = node[2]; body = node[3]
            self.functions[name] = (args, body)
            return None

        # return
        if kind == 'return':
            val = self.eval(node[1], env)
            raise ReturnSignal(val)

        # literals
        if kind == 'number': return node[1]
        if kind == 'string': return node[1]
        if kind == 'bool': return node[1]

        # variable lookup
        if kind == 'var':
            name = node[1]
            if name in env:
                return env[name]
            if name in self.functions:
                return self.functions[name]
            raise NameError(f"Areyyyyy Bhai! '{name}' not declared. Use 'accha {name};' to declare before use.")

        # function / builtin call
        if kind == 'call':
            name = node[1]
            args = [self.eval(a, env) for a in node[2]]

            # builtins:
            if name == 'dher':
                return len(args[0])
            if name == 'chaap':
                print(' '.join(str(a) for a in args))
                return None
            if name == 'baki':
                return args[0][1:]
            if name == 'golpo':
                n = int(args[0]) if args else 1
                return 'ek chhoto golpo ' * n
            if name == 'boro_koro':
                v = args[0]
                try:
                    return int(v)
                except:
                    try:
                        return float(v)
                    except:
                        return v
            if name == 'chhoto_koro':
                v = args[0]
                if isinstance(v, str): return v.lower()
                return v
            if name == 'thuk':
                if not args: return random.random()
                if len(args) == 2 and all(isinstance(x, int) for x in args):
                    return random.randint(args[0], args[1])
                return None
            if name == 'dhow':
                msg = args[0] if args else 'BongoLang error'
                raise RuntimeError(str(msg))

            # user-defined
            if name in self.functions:
                fargs, fbody = self.functions[name]
                if len(fargs) != len(args):
                    raise TypeError('Argument count mismatch')
                # create function-local environment inheriting caller env (lexical)
                newenv = dict(env)   # inherit variables from caller
                # parameters shadow any existing names
                for k, v in zip(fargs, args):
                    newenv[k] = v
                try:
                    self.eval(fbody, newenv)
                except ReturnSignal as rs:
                    return rs.val
                return None

            raise NameError(f"Unknown function: {name}")

        # unary / binary ops
        if kind == 'neg':
            return -self.eval(node[1], env)
        if kind == 'PLUS':
            return self.eval(node[1], env) + self.eval(node[2], env)
        if kind == 'MINUS':
            return self.eval(node[1], env) - self.eval(node[2], env)
        if kind == 'MUL':
            return self.eval(node[1], env) * self.eval(node[2], env)
        if kind == 'DIV':
            return self.eval(node[1], env) / self.eval(node[2], env)
        if kind == 'EQ':
            return self.eval(node[1], env) == self.eval(node[2], env)
        if kind == 'NE':
            return self.eval(node[1], env) != self.eval(node[2], env)
        if kind == 'LT':
            return self.eval(node[1], env) < self.eval(node[2], env)
        if kind == 'GT':
            return self.eval(node[1], env) > self.eval(node[2], env)
        if kind == 'LE':
            return self.eval(node[1], env) <= self.eval(node[2], env)
        if kind == 'GE':
            return self.eval(node[1], env) >= self.eval(node[2], env)

        raise RuntimeError('Unknown node: ' + repr(kind))

# ---------- RUN / REPL ----------
def run_code(src, interp=None):
    tokens = lex(src)
    p = Parser(tokens)
    ast = p.parse()
    if interp is None:
        interp = Interpreter()
    interp.eval(ast)
    return interp

def repl():
    interp = Interpreter()
    print("Jishnu Da — Areyyyyy Bhai BongoLang REPL")
    print("Type 'exit;' or Ctrl-D to quit.")
    while True:
        try:
            line = input(">>> ")
        except EOFError:
            break
        if line.strip() in ("exit", "exit;", "quit"):
            break
        # accept multi-line input until semicolon or closing brace
        buf = line + "\n"
        # if the line doesn't end a statement, read more
        while (';' not in line) and ('}' not in line):
            try:
                line = input("... ")
            except EOFError:
                break
            buf += line + "\n"
        try:
            run_code(buf, interp)
        except Exception as e:
            print(f"Areyyyyy Bhai eikhane error ache re ({type(e).__name__}: {e})....thik kore ne tahole")

if __name__ == '__main__':
    if len(sys.argv) > 1:
        with open(sys.argv[1], 'r', encoding='utf-8') as f:
            run_code(f.read())
    else:
        repl()
