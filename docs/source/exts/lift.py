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
            return match.groups()
        else:
            return (title_line, "")

    title, type_ = parse_title_line(content[0])
    if len(content) > 1: body = '\n'.join(content[1:])
    else:                body = ""

    pattern_node = Pattern()
    pattern_node['classes'] += ['pattern-' + nodes.make_id(title)]
    # add title
    pattern_node += make_pattern_title(title.strip(), lineno, state)
    # add type
    pattern_node += make_pattern_type(type_.strip(), lineno, state)
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
    type_node += nodes.Text(*type_text)
    return [type_node]

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
    type_ = re.sub(r"(.*)\W(.*)/(.*)\W(.*)",
                   r"\1{{}^{\2}\\!/\\!_{\3}}\4",
                   type_)
    return type_


# HTML printing

def visit_pattern_node_html(self, node):
    self.body.append(self.starttag(node, 'div', CLASS=('pattern lift ')))

def depart_pattern_node_html(self, node):
    self.body.append('</div>')

def visit_pattern_title_node_html(self, node):
    self.body.append(r'<code class="docutils literal pattern-title">')

def depart_pattern_title_node_html(self, node):
    self.body.append(r'</code>')

def visit_pattern_type_node_html(self, node):
    self.body.append(r' : <span class="math">\(')
    type_ = node.children[0]
    # print the content ourself ...
    self.body.append(type_to_latex(type_))
    # ... and then remove the node, so that it is not printed a second time
    node.remove(type_)

def depart_pattern_type_node_html(self, node):
    self.body.append(r'\)</span>')


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
    self.body.append(' : \\(')
    type_ = node.children[0]
    # print the content ourself ...
    self.body.append(type_to_latex(type_))
    # ... and then remove the node, so that it is not printed a second time
    node.remove(type_)

def depart_pattern_type_node_latex(self, node):
    self.body.append('\\)\n')


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

    app.add_directive('lift.pattern', PatternDirective)

    return {'version': '0.1'}

