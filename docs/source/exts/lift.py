import re
from docutils import nodes
from docutils.parsers.rst import Directive
from sphinx.locale import _


class Pattern(nodes.General, nodes.Element):
    pass

def make_pattern(content, lineno, state):
    def parseContent(content):
        match = re.search('^(.*):(.*)$', content)
        if match:
            return match.groups()
        else:
            return (content, "")

    title, type_ = parseContent('\n'.join(content))
    pattern_node = Pattern()
    pattern_node['classes'] += ['pattern-' + nodes.make_id(title)]
    pattern_node += make_pattern_title(title.strip(), lineno, state)
    if type_ != "":
        pattern_node += make_pattern_type(type_.strip(), lineno, state)

    return [pattern_node]

def visit_pattern_node_html(self, node):
    self.body.append(self.starttag(node, 'div', CLASS=('pattern lift ')))

def depart_pattern_node_html(self, node):
    self.body.append('</div>')

def visit_pattern_node(self, node):
    self.visit_admonition(node)

def depart_pattern_node(self, node):
    self.depart_admonition(node)


class PatternTitle(nodes.General, nodes.Element):
    pass

def make_pattern_title(content, lineno, state):
    title_node = PatternTitle()
    title_text, _ = state.inline_text(content.strip(), lineno)
    title_node += nodes.paragraph('', *title_text)
    return [title_node]

def visit_pattern_title_node_html(self, node):
    self.body.append(r'<code class="docutils literal pattern-title">')

def depart_pattern_title_node_html(self, node):
    self.body.append(r'</code>')


class PatternType(nodes.General, nodes.Element): pass

def make_pattern_type(content, lineno, state):
    type_node = PatternType()
    type_text, _ = state.inline_text(content.strip(), lineno)
    type_node += nodes.paragraph('', *type_text)
    return [type_node]

def visit_pattern_type_node_html(self, node):
    self.body.append(r' : <span class="math">\(')
    paragraph = node.children[0]
    type_ = paragraph.children[0]
    paragraph.replace_self(nodes.paragraph('', type_to_latex(type_)))

def depart_pattern_type_node_html(self, node):
    self.body.append(r'\)</span>')

def type_to_latex(type_):
    type_ = type_.replace("->", r"\rightarrow{}")
    type_ = type_.replace("*", r"\times{}")
    type_ = re.sub(r"(.*)\W(.*)/(.*)\W(.*)",
                   r"\1{{}^{\2}\\!/\\!_{\3}}\4",
                   # r"\1{\\frac{\2}{\3}}\4",
                   type_)
    return type_


class PatternDirective(Directive):
    has_content = True

    def run(self):
        env = self.state.document.settings.env

        targetid = "pattern-%d" % env.new_serialno('pattern')
        targetnode = nodes.target('', '', ids=[targetid])

        pat = make_pattern(self.content, self.lineno, self.state)

        return [targetnode] + pat


def setup(app):
    app.add_node(Pattern,
                 html=(visit_pattern_node_html, depart_pattern_node_html),
                 latex=(visit_pattern_node, depart_pattern_node),
                 text=(visit_pattern_node, depart_pattern_node))

    app.add_node(PatternTitle,
                 html=(visit_pattern_title_node_html, depart_pattern_title_node_html))

    app.add_node(PatternType,
                 html=(visit_pattern_type_node_html, depart_pattern_type_node_html))

    app.add_directive('lift.pattern', PatternDirective)

    return {'version': '0.1'}

