import sys

# sys.exit()

nt = type(None)
I = lambda x: x

class Error:
    def __init__(self, msg, origin, expr, name):
        self.msg = msg
        self.origin = origin
        self.expr = expr
        self.name = name
    def display(self):
        print(str(self))
    def __repr__(self):
        return 'Unhandled exception: {}\n{}\n{}\n{}'.format(self.name, self.expr,(' ' * self.origin + '^\n') if self.origin else '\b', self.msg)
        #return f'Unhandled exception: {self.name}\n' + (f'{self.expr}\n{' ' * self.origin + '^\n' if self.expr else '\n'}' + f'{self.msg}'

def parse_error(origin_buffer, origin, errmsg, errname):
    n = Node(None, 'Value', origin_buffer)
    n.value = lambda a, e, _: Error(errmsg, origin_buffer +  origin, e, errname)
    return [n]
 
def cls_stringify(cls):
    return str(cls)[8:-2] # Removes the "<class '" and "'>" part from string (e.g. "<class 'object'>")
def lst_stringify(lst):
    return ', '.join([cls_stringify(j) for j in lst])

class Value:
    valid = [chr(x)for x in range(97, 123)]+[chr(x)for x in range(65, 91)]+['_']+[str(s)for s in range(0, 10)]
class Operation:
    class Unary:
        class Boolean:
            class Negation:
                valid = [bool]
                @staticmethod
                def function(p, c):
                    return not p
    class Binary:
        class Boolean:
            @staticmethod
            def excl_disjunction(p, q, c):
                return (p or q) and not (p and q)
        class Arithmetic:
            class Addition:
                valid = [[int, int], [float, int], [int, float], [float, float], [str, str]]
                @staticmethod
                def function(a, b, c):
                    return a + b
                    
            class Subtraction:
                valid = [[int, int], [float, int], [int, float], [float, float]]
                @staticmethod
                def function(a, b, c):
                    return a - b

            class Multiplaction:
                valid = [[int, int], [float, int], [int, float], [float, float], [str, int]]
                @staticmethod
                def function(a, b, c):
                    return a * b

            class Division:
                valid = [[int, int], [float, int], [int, float], [float, float]]
                @staticmethod
                def function(a, b, c):
                    if b == 0:
                        return Error('Cannot divide by zero!', c["self"].origin, c["expr"], 'DivideByZeroError')
                    return a / b
    
class Operators:
    Unary = {
        "name": "Unary functions",
        "tags": ["unary"],
        "ops": {
            "~": { # Negation
                "funct": lambda a, _: not a,
                "supported": [bool]
                }
            },
            "!": { # Negation
                "funct": lambda a, _: not a,
                "supported": [bool]
            }

        }
    Binary = [
        # Boolean Functions
        { # And
            "name": "And",
            "category": "boolean-functions",
            "tags": [],
            "ops": {
                '∧': {
                    "funct": lambda a, b, _: a and b,
                    "supported": [[bool, bool]]
                    }
                }
            },
        { # Or
            "name": "Or",
            "category": "boolean-functions",
            "tags": [],
            "ops": {
                '∨': {
                    "funct": lambda a, b, _: a or b,
                    "supported": [[bool, bool]]
                    }
                }
            },
        # Mathmatical Functions
        { # Add, Sub
            "name": "Add-Sub",
            "category": "Mathmatical-functions",
            "tags": [],
            "ops": {
                '+': {
                    "funct": Operation.Binary.Arithmetic.Addition.function,
                    "supported": Operation.Binary.Arithmetic.Addition.valid
                    },
                '-': {
                    "funct": Operation.Binary.Arithmetic.Subtraction.function,
                    "supported": Operation.Binary.Arithmetic.Subtraction.valid
                    }
                }
            },
        { # Mult, Div
            "name": "Mult-Div",
            "category": "Mathmatical-functions",
            "tags": [],
            "ops": {
                    '*': { # Mult
                        "funct": Operation.Binary.Arithmetic.Multiplaction.function,
                        "supported": Operation.Binary.Arithmetic.Multiplaction.valid
                        },
                    '/': { # Div
                        "funct": Operation.Binary.Arithmetic.Division.function,
                        "supported": Operation.Binary.Arithmetic.Division.valid
                        }
                }
            },
        # Turnary Operators
        { # ?, :
            "name": "Turnary Operations",
            "category": None,
            "tags": [],
            "ops": {
                '?': { # If-Clause
                    "funct": lambda b, o, c: [b] + ([c["r"].value(c["vars"], c["expr"], c)] if b else []),
                    "supported": [[bool, object]], 
                    "ignores": ["r"] 
                    },
                ':': { # Else-clause
                    "funct": lambda l, o, c: (l[1] if l[0] else c["r"].value(c["vars"], c["expr"], c)), 
                    "supported": [[list, object]], 
                    "ignores": ["r"]
                    }
                }
            },
        # Eval function
        {
            "name": "Eval-function",
            "category": "Meta-functions",
            "tags": ["meta"],
            "ops": {
                '#': {
                    "funct": lambda v, e, c: expr_parse(e, v, 0, 0, c["show_steps"])[0].value(c["vars"], e, c), 
                    "supported": [[dict, str]]
                    },
                }
            },
        {
            "name": "Application",
            "category": "Lambda-Calculus",
            "tags": [],
            "ops": {
                "<=": {
                    "funct": lambda a, b, c: a(b),
                    "supported": [[type(I), object]]
                }
            }
        }
    ]
        
class Node:
    def __init__(self, info, _type, origin):
        self.info = info
        self.type = _type
        self.origin = origin
        self.left = None
        self.riht = None
        self.is_operation = False
        self.is_unary = False

    def __repr__(self):
        return 'Node: {0}, Type: {1}, Origin: {2}, Is_op: {3}.'.format(self.info, self.type, self.origin, self.is_operation)

    
    def value(self, _vars, expr, _context):
        if not self.is_operation and not self.is_unary:
            if self.info in list(_vars.keys()):
                return _vars[self.info]
            if self.info == 'Y':
                return Error('This is an error.\nDo not use this.', self.origin, expr, "YError")
            try:
                return eval(self.info)
            except NameError as e:
                return Error('{0}'.format('{0}.'.format(e)[5:]), self.origin, expr, 'UndefinedVariableError')
            except SyntaxError as e:
                return Error('Invalid syntax? {0}'.format(e), self.origin, expr, 'InvalidSyntaxError')

        elif self.is_unary:
            assert(False)
        elif self.is_operation:
            for priority in Operators.Binary:
                l = None
                tl = nt
                r = None
                tr = nt

                if self.info not in list(priority["ops"].keys()):
                    continue

                ignores = []
                try:
                    ignores = priority["ops"][self.info]["ignores"]
                except KeyError:
                    pass


                context = {"l": self.left, "r": self.riht, "expr": expr, "vars": _vars, "show_steps": _context["show_steps"], "self": self, "tl": nt, "tr": nt}
                if "l" not in ignores:
                    l = self.left.value(_vars, expr, context)
                    tl = type(l)
                    context["tl"] = tl
                    if tl == Error:
                        return l

                if self.info not in list(priority["ops"].keys()):
                    return Error('\'' + self.info + '\' isn\'t a valid operator!', self.origin, expr, 'InvalidOperatorError')

                if "r" not in ignores:
                    r = self.riht.value(_vars, expr, context)
                    tr = type(r)
                    context["tr"] = tr
                    if tr == Error:
                        return r
                for types in priority["ops"][self.info]["supported"]:
                    if (("l" in ignores) or issubclass(tl, types[0])) and (("r" in ignores) or issubclass(tr, types[1])):
                        break
                else:
                    return Error('Unsupported operand type(s) for the \'{0}\' operator: {1}, {2}.'.format(self.info, cls_stringify(tl), cls_stringify(tr)), self.origin, expr, 'UnsupportedOperandTypesError')
                try:
                    return priority["ops"][self.info]["funct"](l, r, context)
                except TypeError as e:
                   return Error('The operands given to the \'{0}\' operator resulted in an error. {1}.'.format(self.info, e), self.origin, expr, 'InternalErrorInOperationError')
            else:
                return Error('\'' + self.info + '\' isn\'t a valid operator!', self.origin, expr, 'InvalidOperatorError') 
        else:
            return Error('Invalid node type!', None)

    def to_tuple(self):
        return (self.info, self.type, self.origin)

    def visualise(self, depth=0, buffer=0):
        print(' ' * buffer * 3 + (' ' * (depth-1) * 3 + '|- ' if depth!= 0 else '') + str(self))
        if self.left:
            self.left.visualise(depth + 1, buffer)
        if self.riht:
            self.riht.visualise(depth + 1, buffer)

def class_of(char):
    return "Value" if char in Value.valid else "Operation"

# Value:
    # Boolean
    # Integer
    # Number
    # String

def current_node(current, _type, origin):
    return [Node(current, _type, origin)] if current != '' else []

def expr_parse(expr, given={}, depth=0, origin_buffer=0, show_steps=False):
    if expr == '':
        return parse_error(origin_buffer, 0, 'Empty expression given.', 'EmptyExpressionError')

    if show_steps:
        print(depth * '  ' + 'Evaluating {0}, Buffer: {1}'.format(expr, origin_buffer))

    # Step one: Lex
    current = ''
    on_new = True
    origin = 0
    parsed = []
    current_class = ''
    prev_class = ''

    in_sub_expr = False
    sub_expr = ''
    sub_origin = 0
    indent = 0

    in_list = False
    sub_list = []

    in_string = False
    temp_ignore_escp = False

    in_infix = False

    for i, char in enumerate(list(expr + ' ')):
        if in_sub_expr:
            if char == '(':
                indent += 1
                sub_expr += '('
            elif char == ')':
                if indent == 0:
                    in_sub_expr = False
                    on_new = True
                    current = ''
                    parsed += expr_parse(sub_expr, given, depth+1, origin_buffer+sub_origin+1, show_steps)
                    sub_expr = ''
                else:
                    indent -= 1
                    sub_expr += ')'
            else:
                sub_expr += char
                if i == len(expr):
                    return parse_error(origin_buffer, sub_origin, 'Mismatching left parenthesis.', 'MismatchingLeftParenError')

        elif in_string:
            if char == '\'' and not temp_ignore_escp:
                parsed += current_node(current + '\'', 'Value', origin+origin_buffer)
                current = ''
                on_new = True
                in_string = False
                prev_class = 'Value'

            elif char == '\\':
                temp_ignore_escp = True
            else:
                if i == len(expr) - 1:
                    return parse_error(origin_buffer, i + 1, 'Unexpected EOL while reading string literal.', 'UnexpectedEndOfLineError')
                current += char
                temp_ignore_escp = False

        elif in_infix:
            if char == '`':
                in_infix = False
                parsed += current_node(current, 'Operation', origin+origin_buffer)
                current = ''
                on_new = True
                prev_class = 'Operation'
            else:
                current += char
        else:
            if char == ' ':
                parsed += current_node(current, prev_class, origin+origin_buffer)
                current = ''
                on_new = True
            elif char == '(':
                in_sub_expr = True
                sub_origin = i
                parsed += current_node(current, prev_class, origin+origin_buffer)
                current = ''
                on_new = True
            elif char == ')':
                return parse_error(origin_buffer, i, 'Mismatching right parenthesis.', 'MismatchingRightParenError')
            elif char == '\'':
                in_string = True
                parsed += current_node(current, prev_class, origin+origin_buffer)
                current = '\''
            elif char == '`': # Infix notation
                in_infix = True
                origin = i
            elif on_new:
                current = char
                prev_class = class_of(char)
                if in_infix:
                    prev_class = 'Operation'
                on_new = False
                origin = i 
            else:
                current_class = class_of(char)
                if in_infix:
                    current_class = 'Operation'
                    prev_class = 'Operation'
                if prev_class != current_class:
                    parsed += current_node(current, prev_class, origin+origin_buffer)
                    current = char
                    origin = i
                else:
                    current += char
                on_new = False
                prev_class = current_class

    if show_steps:
        print(' ' * 3 * depth +  'Given:')
        for i in parsed:
            i.visualise(buffer=depth)
        print()

    # AST

    # Step two: Unary Operations
        # Todo:
            # Detect Unaries. Use pattern ..OOV.. (Op, Op, Val).
            # Resolve Unaries. Implement ..OOV.. -> ..Ov[V].. (Op, Op, Val -> Op, Val [Val]).
    
    # Step two_two: value application, and unexpected operation check

    parsed_new = []
    prev_type = 'Operation'
    for k, n in enumerate(parsed):
        parsed_new += [n]
        current_type = n.type
        if prev_type == current_type:
            if current_type == 'Operation':
                return parse_error(origin_buffer, n.origin, 'Unexpected operator', 'UnexpectedOperatorError')
            elif current_type == 'Value':
                parsed_new += [Node('<=', 'Operation', n.origin - 1), parsed_new.pop()]
            else:
                return parse_error(origin_buffer, n.origin, 'Invalid node type! {0}'.format(current_type), 'InvalidNodeTypeError')
        prev_type = current_type
    parsed = parsed_new

    if show_steps:
        print(' ' * 3 * depth +  'Insert Application:')
        for i in parsed:
            i.visualise(buffer=depth)
        print()

    if parsed[-1].type != 'Value':
        return parse_error(origin_buffer, parsed[-1].origin + len(parsed[-1].info) + 1, 'Missing value after operator.', 'MissingValueError')

    # Step three: Known Binary Operations.
        # Todo: Use order of Operations

    # Check single: Is the pattern a single node?
    if len(parsed) == 1:
        return parsed
    if len(parsed) == 2:
        raise EnvironmentError('Check single; 2')

    # Step four: Unknown Binary Operations.

    while len(parsed) != 1:
        parsed[1].left = parsed[0]
        parsed[1].riht = parsed[2]

        parsed.pop(0)
        parsed.pop(1)
        parsed[0].type = 'Value'
        parsed[0].is_operation = True


    # Assert single: Is the pattern a single node?
    if len(parsed) != 1:
        raise EnvironmentError('Assert single node')

    if show_steps:
        print(' ' * 3 * depth +  'Result:')
        [p.visualise(buffer=depth) for p in parsed]
        print()

    return parsed

_vars = {
    'T': True,
    'F': False,
    'X': {},
    'I': lambda x: x,
    'M': lambda x: x(x),
    'K': eval('lambda a: lambda b: a')
}

def resolve(fn, text):
    e = expr_parse(text, show_steps=False)
    result = e[0].value(_vars, text, {"show_steps": False})
    if type(result) == Error:
        return None, result
    return result, None