import re
from docutils import nodes
from docutils.parsers.rst import Directive
from sphinx.locale import _

# Nodes

class Pattern(nodes.General, nodes.Element):
    pass

def make_pattern(content, lineno, state):
    def parse_title_line(title_line):
        match = re.search('^(.*):(.*)$', title_line)
        if match:
            title = match.groups()[0]
            rest  = match.groups()[1]
            match = re.search('^(.*?)=(.*)$', rest)
            if match:
                return (title, match.groups()[0], match.groups()[1])
            else:
                return (title, rest, "")
        else:
            return (title_line, "", "")

    title, type_, semantics = parse_title_line(content[0])
    if len(content) > 1: body = '\n'.join(content[1:])
    else:                body = ""

    pattern_node = Pattern()
    pattern_node['classes'] += ['pattern-' + nodes.make_id(title)]
    # add title
    pattern_node += make_pattern_title(title.strip(), lineno, state)
    # add type
    pattern_node += make_pattern_type(type_.strip(), lineno, state)
    # add semantics
    pattern_node += make_pattern_semantics(semantics.strip(), lineno, state)
    # add body
    body_text, _ = state.inline_text(body.strip(), lineno)
    pattern_node += nodes.paragraph('', *body_text)

    return [pattern_node]

class PatternTitle(nodes.General, nodes.Element):
    pass

def make_pattern_title(content, lineno, state):
    title_node = PatternTitle()
    title_text, _ = state.inline_text(content.strip(), lineno)
    title_node += nodes.Text(*title_text)
    return [title_node]

class PatternType(nodes.General, nodes.Element): pass

def make_pattern_type(content, lineno, state):
    type_node = PatternType()
    type_text, _ = state.inline_text(content.strip(), lineno)
    if type_text != []:
        type_node += nodes.Text(*type_text)
    return [type_node]

class PatternSemantics(nodes.General, nodes.Element): pass

def make_pattern_semantics(content, lineno, state):
    sema_node = PatternSemantics()
    sema_text, _ = state.inline_text(content.strip(), lineno)
    if sema_text != []:
        sema_node += nodes.Text(*sema_text)
    return [sema_node]

# Directives

class PatternDirective(Directive):
    has_content = True

    def run(self):
        env = self.state.document.settings.env

        targetid = "pattern-%d" % env.new_serialno('pattern')
        targetnode = nodes.target('', '', ids=[targetid])

        pat = make_pattern(self.content, self.lineno, self.state)

        return [targetnode] + pat

# Utility function

def type_to_latex(type_):
    type_ = type_.replace("->", r"\rightarrow{}")
    type_ = type_.replace("*", r"\times{}")
    type_ = type_.replace("<", r"\langle{}")
    type_ = type_.replace(">", r"\rangle{}")
    type_ = re.sub(r"(.*)\W(.*)/(.*)\W(.*)",
                   r"\1{{}^{\2}\\!/\\!_{\3}}\4",
                   type_)
    return type_

def sema_to_latex(sema):
    sema = type_to_latex(sema)
    sema = sema.replace("...", r"\ldots{}")
    sema = sema.replace(";", r"\\")
    sema = sema.replace(" ", r"\ ")
    sema = sema.replace(r"\\\ ", r"\\")
    sema = sema.replace(",\ ", r", ")
    sema = sema.replace("\ =\ ", r"=")
    sema = sema.replace("\ +\ ", r"+")
    sema = sema.replace("\ -\ ", r"-")
    return sema


# HTML printing

def visit_pattern_node_html(self, node):
    self.body.append(self.starttag(node, 'div', CLASS=('pattern lift ')))

def depart_pattern_node_html(self, node):
    self.body.append('</div>')

def visit_pattern_title_node_html(self, node):
    self.body.append(r'<div><span class="pattern-title"><code class="docutils literal">')

def depart_pattern_title_node_html(self, node):
    self.body.append(r'</code></span>')

def visit_pattern_type_node_html(self, node):
    if len(node.children) > 0:
        self.body.append(r'<span class="pattern-type"><span class="math">\(')
        type_ = node.children[0]
        # print the content ourself ...
        self.body.append(type_to_latex(type_))
        # ... and then remove the node, so that it is not printed a second time
        node.remove(type_)
        self.has_printed_type = 'yes'
    else:
        self.has_printed_type = 'no'

def depart_pattern_type_node_html(self, node):
    if self.has_printed_type == 'yes':
        self.body.append(r'\)</span></span>')

def visit_pattern_sema_node_html(self, node):
    if len(node.children) > 0:
        self.body.append(r'<span class="pattern-sema"><span class="math">\(')
        sema = node.children[0]
        self.body.append(sema_to_latex(sema))
        node.remove(sema)
        self.has_printed_sema = 'yes'
    else:
        self.has_printed_sema = 'no'

def depart_pattern_sema_node_html(self, node):
    if self.has_printed_sema == 'yes':
        self.body.append(r'\)</span></span>')
    self.body.append(r'</div>')


# LaTeX printing

def visit_pattern_node_latex(self, node):
    self.body.append("\n\n")

def depart_pattern_node_latex(self, node):
    pass

def visit_pattern_title_node_latex(self, node):
    self.body.append(r'\sphinxcode{')

def depart_pattern_title_node_latex(self, node):
    self.body.append(r'}')

def visit_pattern_type_node_latex(self, node):
    if len(node.children) > 0:
        self.body.append(' : \\(')
        type_ = node.children[0]
        # print the content ourself ...
        self.body.append(type_to_latex(type_))
        # ... and then remove the node, so that it is not printed a second time
        node.remove(type_)
        self.has_printed_type = 'yes'
    else:
        self.has_printed_type = 'no'

def depart_pattern_type_node_latex(self, node):
    if self.has_printed_type == 'yes':
        self.body.append('\\)')
    self.body.append('\n')

def visit_pattern_sema_node_latex(self, node):
    if len(node.children) > 0:
        self.body.append(' \\(\\equiv\\) \\begin{align*}')
        type_ = node.children[0]
        # print the content ourself ...
        self.body.append(sema_to_latex(type_))
        # ... and then remove the node, so that it is not printed a second time
        node.remove(type_)
        self.has_printed_sema = 'yes'
    else:
        self.has_printed_sema = 'no'

def depart_pattern_sema_node_latex(self, node):
    if self.has_printed_sema == 'yes':
        self.body.append('\\end{align*}')
    self.body.append('\n')


# Text rendering

def visit_pattern_node(self, node):
    pass

def depart_pattern_node(self, node):
    pass



def setup(app):
    app.add_node(Pattern,
                 html=(visit_pattern_node_html, depart_pattern_node_html),
                 latex=(visit_pattern_node_latex, depart_pattern_node_latex),
                 text=(visit_pattern_node, depart_pattern_node))

    app.add_node(PatternTitle,
                 html=(visit_pattern_title_node_html, depart_pattern_title_node_html),
                 latex=(visit_pattern_title_node_latex, depart_pattern_title_node_latex),
                 text=(visit_pattern_node, depart_pattern_node))

    app.add_node(PatternType,
                 html=(visit_pattern_type_node_html, depart_pattern_type_node_html),
                 latex=(visit_pattern_type_node_latex, depart_pattern_type_node_latex),
                 text=(visit_pattern_node, depart_pattern_node))

    app.add_node(PatternSemantics,
                 html=(visit_pattern_sema_node_html, depart_pattern_sema_node_html),
                 latex=(visit_pattern_sema_node_latex, depart_pattern_sema_node_latex),
                 text=(visit_pattern_node, depart_pattern_node))

    app.add_directive('lift.pattern', PatternDirective)

    return {'version': '0.1'}

